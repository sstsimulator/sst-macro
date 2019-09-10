
helpText = """The following environmental variables can be defined for the SST compiler
SSTMAC_VERBOSE=0 or 1:        produce verbose output from the SST compiler (default 0)
SSTMAC_DELETE_TEMPS=0 or 1:   remove all temp source-to-source files (default 1)
SSTMAC_SRC2SRC=0 or 1: run a source-to-source pass converting globals to TLS (default 1)
SSTMAC_CONFIG=0: running automake, cmake - skip certain steps to fool build system
"""

def createBashWrapper(compiler, exeName, ldTarget, sstCore, sstmacExe):
  import commands
  #there is one scenario in which autoconf actually WANTS
  #this to fail... check for it now
  output = commands.getoutput("nm %s | grep some_bogus_nonexistent_symbol" % ldTarget)
  if output:
    #crash and burn
    return 1
  
  import os
  cmd = ""
  if sstCore:
    sys.exit("Do not yet support standalone exe for SST core")
  else:
    exeLoad = ldTarget
    if not os.path.isabs(exeLoad):
      exeLoad = os.path.join(os.getcwd(), ldTarget)
    cmd = "%s -a -n 1 --exe=%s --use-app-rc --app-argv='%%s'" % (sstmacExe, exeLoad)
  str_arr = ["#! /usr/bin/env python",
   "# -*- coding: utf-8 -*-",
   "import os",
   "import sys",
   "import subprocess as sp",
   'argv = " ".join(sys.argv[1:])',
   "cmd = ['%s', '-a', '-n', '1', '--exe=%s', '--use-app-rc', '--app-argv=%%s' %% argv]" % (sstmacExe, exeLoad),
   "child = sp.Popen(cmd)",
   "streamdata = child.communicate()[0]",
   "rc = child.returncode",
   "sys.exit(rc)",
   "",
   '"""',
   open(ldTarget).read(),
   '"""',
  ]
  open(exeName,"w").write("\n".join(str_arr))
  os.system("chmod +x %s" % exeName)
  return 0

def runCmdArr(cmdArr,verbose):
  import sys,os
  if cmdArr:
    cmd = " ".join(cmdArr)
    if verbose: sys.stderr.write("%s\n" % cmd)
    return os.system(cmd)
  else:
    return 0
    
class TempFiles:
  def __init__(self, doDelete, verbose):
    self.doDelete = doDelete
    self.verbose = verbose
    self.files = []

  def append(self, f):
    self.files.append(f)

  def __del__(self):
    self.cleanUp()

  def cleanUp(self):
    import os
    import sys
    cmd = "rm -f %s" % " ".join(self.files)
    import traceback
    if self.doDelete and self.files:
        if self.verbose:
          sys.stderr.write("%s\n" % cmd)
        os.system(cmd)

def runAllCmds(cmds, verbose, doDelete):
  tmpFiles = TempFiles(doDelete, verbose)
  from subprocess import check_output,STDOUT,Popen,PIPE
  import sys
  for outfile, cmdArr, tempFiles in cmds:
    if verbose:
      sys.stderr.write("===============================\n")
      sys.stderr.write(" ".join(cmdArr))
      sys.stderr.write("\n")
    stdout=None
    if outfile:
      stdout = open(outfile,"w")
    child = Popen(cmdArr,stdout=stdout)
    result = child.communicate()
    if outfile:
      stdout.close()
    rc = child.returncode
    if not rc == 0:
      return rc

    for tmp in tempFiles:
      tmpFiles.append(tmp)

  return 0


class Context:
  def __init__(self):
    self.cxxFlags = []
    self.cFlags = []
    self.cppFlags = []
    self.ldFlags = []
    self.libs = []
    self.typ = ""
    self.defines = []
    self.directIncludes = []
    self.ld = None
    self.cc = None
    self.cxx = None
    self.compiler = None
    self.clangArgs = []

def run(typ, extraLibs="", makeLibrary=False, redefineSymbols=True, runClang=True):
  import os
  import sys
  import platform
  from configlib import getstatusoutput
  from sstccvars import sstLdFlags, sstCppFlags
  from sstccvars import prefix, execPrefix, includeDir, cc, cxx, spackcc, spackcxx
  from sstccvars import sstCxxFlagsStr, sstCFlagsStr
  from sstccvars import includeDir
  from sstccvars import sstCore
  from sstccvars import soFlagsStr
  from sstccvars import clangCppFlagsStr, clangLdFlagsStr
  from sstccutils import cleanFlag, getProcTree, swapSuffix

  ctx = Context()
  ctx.cc = spackcc if spackcc else cc
  ctx.cxx = spackcxx if spackcxx else cxx
  ctx.typ = typ

  needfPIC = "fPIC" in sstCxxFlagsStr

  memoizing = False
  if os.environ.has_key("SSTMAC_MEMOIZE"):
    val = int(os.environ["SSTMAC_MEMOIZE"])
    memoizing = bool(val)

  skeletonizing = False
  if os.environ.has_key("SSTMAC_SKELETONIZE"):
    val = int(os.environ["SSTMAC_SKELETONIZE"])
    skeletonizing = bool(val)

  sstmacExe = cleanFlag(os.path.join(prefix, "bin", "sstmac"))

  if not sstCore:
    ctx.libs.append('-lsstmac')
    ctx.libs.append('-lsprockit')
    ctx.libs.append('-lundumpi')
  
  verbose = False     #whether to print verbose output
  delTempFiles = True #whether to delete all temp files created
  #whether to make a shared object for loading 
  #or a bash script that emulates an executable
  makeBashExe = False     
  if "SSTMAC_VERBOSE" in os.environ:
    flag = int(os.environ["SSTMAC_VERBOSE"])
    verbose = verbose or flag
  if "SSTMAC_DELETE_TEMPS" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
    delTempFiles = delTempFiles and flag
  if "SSTMAC_CONFIG" in os.environ:
    flag = int(os.environ["SSTMAC_CONFIG"])
    makeBashExe = flag

  procTree = getProcTree()[1:] #throw out python command
  parentProc = procTree[0]
  #if parentProc.endswith("configure"):
  #  makeBashExe = True
  # the parent proc here is just launchd - configure vanishes
  if parentProc.endswith("cmake"):
    numCmakes = 0
    for exe in procTree:
      if exe.endswith("cmake"):
        numCmakes += 1
    if numCmakes > 1:
        makeBashExe = True

  haveClangSrcToSrc = bool(clangCppFlagsStr)

  for entry in sstCppFlags:
    ctx.cppFlags.append(cleanFlag(entry))

  import argparse
  parser = argparse.ArgumentParser(description='Process flags for the SST/macro compiler wrapper')
  parser.add_argument('-o', '--output', type=str,  
                    help="the linker/compilation target")
  parser.add_argument('--keep-exe', default=False, action="store_true",
                    help="whether to create an executable script or build a loadable shared object")
  parser.add_argument('--skeletonize', default="", type=str,
                    help="whether to activate skeletonization mode, stripping compute and mem allocation. Can take a list of LLVM passes as argument.")
  parser.add_argument('--memoize', default=False, type=str,
                    help="whether to activate memoization mode that instruments and records execution. Can take a list of LLVM passes as argument.")
  parser.add_argument('-I', action="append", type=str, help="an include path", default=[])
  parser.add_argument('-W', action="append", type=str, help="activate a particular warning", default=[])
  parser.add_argument('-L', action="append", type=str, help="a library path", default=[])
  parser.add_argument('-l', action="append", type=str, help="a library to link against", default=[])
  parser.add_argument('-Wl', action="append", type=str, help="activate a particular linker argument", default=[])
  parser.add_argument('-O', type=str)
  parser.add_argument('-c', '--compile', default=False, action="store_true")
  parser.add_argument('-E', '--preprocess', default=False, action="store_true")
  parser.add_argument('-V', '--version', default=False, action="store_true", help="Print SST and compiler version info")
  parser.add_argument('--flags', default=False, action="store_true", help="Print the extra flags SST automatically adds")
  parser.add_argument('--prefix', default=False, action="store_true", help="Print the SST installation prefix")
  parser.add_argument('--sst-component', default=False, action="store_true", 
                      help="Whether we are building an SST component and should skip all source-to-source")
  parser.add_argument('-fPIC', default=False, action="store_true", help="compile for position independent code")
  parser.add_argument('-std', type=str, help="specify the standard level for C or C++")
  parser.add_argument('--no-integrated-cpp', default=False, action="store_true", help="whether to skip preprocessing")

  parser.add_argument("-fvisibility", type=str, help="control the visibility of certain symbols")
  
  args, extraArgs = parser.parse_known_args()

  if "SSTMAC_COMPONENT_BUILD" in os.environ:
    flag = int(os.environ["SSTMAC_COMPONENT_BUILD"])
    if flag:
      args.sst_component = True

  if args.no_integrated_cpp:
    sys.exit("SST compiler wrapper cannot handle --no-integrated-cpp flag")

  #keep visibility arg, but not if it causes hidden symbols
  if args.fvisibility and args.fvisibility != "hidden": 
    ctx.compilerFlags.append("-fvisibility=%s" % args.fvisibility)

  if args.skeletonize: ctx.clangArgs.append("--skeletonize")
# if args.memoize: clangArgs.append("--memoize")
  
  #this is probably cmake being a jack-donkey during configure, overwrite it
  if args.std == "c++98": args.std = "c++1y"

  if redefineSymbols and not args.sst_component:
    repldir = os.path.join(cleanFlag(includeDir), "sstmac", "replacements")
    repldir = cleanFlag(repldir)
    args.I.append(os.path.join(prefix, "include", "sumi"))
    ctx.defines.append("SSTMAC=1")
    args.I.insert(0,repldir)

  sysargs = sys.argv[1:]
  asmFiles = False
  sourceFiles = []
  givenObjects = []
  unparsedArgs = []
  for arg in extraArgs:
    sarg = arg.strip().strip("'")
    #okay, well, the flags might have literal quotes in them
    #which get lost passing into here - restore all " to literal quotes
    sarg = sarg.replace("\"",r'\"')
    sarg = sarg.replace(" ", r'\ ')
    if sarg.endswith('.o'):
      givenObjects.append(sarg)
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c') \
                               or sarg.endswith(".cxx") or sarg.endswith(".C"):
      sourceFiles.append(sarg)
    elif sarg.endswith('.S') or sarg.endswith(".s"):
      asmFiles = True
    elif sarg.startswith("-g"):
      unparsedArgs.append("-g") #gstab,g3,etc can appear but mess things up - make them regular -g
    else:
      unparsedArgs.append(sarg)
  ctx.cFlags.extend(unparsedArgs) #add anything I didn't recognize here for now
  ctx.cxxFlags.extend(unparsedArgs) #add anything I didn't recognize here for now

  # Substitute compiler path when built using Spack

  if args.fPIC or needfPIC:
    #assume fPIC was given
    ctx.cxxFlags.append("-fPIC")
    ctx.cFlags.append("-fPIC")

  if args.sst_component: #shut down everything special
    runClang = False
    memoizing = False
    skeletonizing = False
    args.memoize = False
    args.skeletonize = False
  else: #oh, okay, building a skeleton
    #always indicate that we are compiling an external skeleton app
    ctx.defines.append("SSTMAC_EXTERNAL")

  if (runClang and haveClangSrcToSrc) or args.sst_component:
    ctx.defines.append("SSTMAC_NO_REFACTOR_MAIN")

  if memoizing or args.memoize or args.sst_component:
    ctx.defines.append("SSTMAC_NO_REPLACEMENTS")

  #for now, just assume any time an exe name "conftest"
  #is being built, it comes from configure
  if args.output == "conftest":
    makeBashExe = True

  for entry in sstLdFlags:
    ctx.ldFlags.append(cleanFlag(entry))


  
  if sstCore:
    ctx.defines.append("SSTMAC_EXTERNAL_SKELETON")

  if typ == "c++":
    ctx.directIncludes.append("cstdint")
  else:
    ctx.directIncludes.append("stdint.h")
  ctx.directIncludes.append("sstmac/compute.h")
  ctx.directIncludes.append("sstmac/skeleton.h")

  src2src = True
  if "SSTMAC_SRC2SRC" in os.environ:
    src2src = int(os.environ["SSTMAC_SRC2SRC"])

  runClang = haveClangSrcToSrc and src2src and runClang and not args.sst_component


  sstparser = argparse.ArgumentParser(description='Process flags for the SST/macro compiler wrapper')
  sstparser.add_argument('-std', type=str, help="specify the standard level for C or C++")
  sstparser.add_argument('-O', type=str, help="the optimization level for SST/macro - this gets consumed and not forwarded")
  sstparser.add_argument('-g', action="store_true", default=False,
                         help="the debug level for SST/macro = this gets consumed and not forwarded")
  sstCxxFlags = cleanFlag(sstCxxFlagsStr).split()
  sstCxxArgs, passedThroughSstCxxFlags = sstparser.parse_known_args(sstCxxFlags)
  sstCFlags = cleanFlag(sstCFlagsStr).split()
  sstCArgs, passedThroughSstCFlags = sstparser.parse_known_args(sstCFlags)

  ctx.cxxFlags.extend(passedThroughSstCxxFlags)
  ctx.cFlags.extend(passedThroughSstCFlags)

  compiler = ""
  sstCompilerFlags = []
  if typ.lower() == "c++":
    ctx.compiler = cxx
    ctx.ld = cxx
    ctx.compilerFlags = ctx.cxxFlags
    if args.std:
      if sstCxxArgs.std and args.std != sstCxxArgs.std:
        sys.stderr.write("WARNING: SST compiled with %s, but app compiled with %s - choosing app's version\n" % (sstCxxArgs.std, args.std))
      ctx.cxxFlags.append("-std=%s" % args.std)
    elif sstCxxArgs.std:
      ctx.cxxFlags.append("-std=%s" % sstCxxArgs.std)
    else:
      pass
    ctx.compilerFlags = ctx.cxxFlags[:]
  elif typ.lower() == "c":
    ctx.compiler = cc
    if runClang:
      #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game
      ctx.ld = cxx
    else:
      ctx.ld = cc 
    if args.std:
      ctx.cFlags.append("-std=%s" % args.std)
    elif sstCArgs.std:
      ctx.cFlags.append("-std=%s" % sstCArgs.std)
    ctx.compilerFlags = ctx.cFlags[:]
    if sstCxxArgs.std: #we will still do some C++, make sure we get the right -std flag
      ctx.cxxFlags.append("-std=%s" % sstCxxArgs.std)

  if args.version:
    import inspect, os
    pathStr = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
    print(pathStr)
    cmd = "%s --version" % (cxx)
    os.system(cmd)
    sys.exit()
  elif args.flags:
    sys.stderr.write("LDFLAGS=%s\n" % " ".join(ctx.ldFlags))
    sys.stderr.write("CPPFLAGS=%s\n" % " ".join(ctx.cppFlags))
    if typ == "c++":
      sys.stderr.write("CXXFLAGS=%s\n" % " ".join(ctx.compilerFlags))
    else:
      sys.stderr.write("CFLAGS=%s\n" % " ".join(ctx.compilerFlags))
    sys.exit()
  elif args.prefix:
    sys.stdout.write("%s\n" % prefix)
    sys.exit()



  if asmFiles:
    #just execute the command as-is with no frills
    cmdArr = [
      cc
    ]
    cmdArr.extend(sys.argv[1:])
    cmds = [ [None,cmdArr,[]] ]
    return runAllCmds(cmds, verbose, delTempFiles)

  #this might be an actual library, not an exe wrapper
  ldTarget = args.output
  if not ldTarget: 
    ldTarget = "a.out"
  if ldTarget.startswith("lib"):
    if ".so" in ldTarget or ".dylib" in ldTarget:
      makeBashExe = False

  exeName = ldTarget #maybe needed later
  if makeBashExe:
    ldTarget += "_exe"

  runLinker = not args.preprocess and not args.compile

  from sstcompile import addSrc2SrcCompile, addComponentCompile, addCompile, addPreprocess
  from sstlink import addLink

  #the format of the entry in the cmd arr is as follows
  # [outfile, [cmds,...], [tmpFiles,...]]
  # the outfile can be None or a string saying where stdout should be piped
  #     if None, stdout is printed to screen 
  # the cmd arr is directly passed the Popen command to run a subprocess
  # the tmp files is a (possibly empty) list of files that get generated
  #      but should be cleaned up after all commands finish
  cmds = []
  if args.preprocess:
    for srcFile in sourceFiles:
      addPreprocess(ctx, srcFile, None, args, cmds)
    rc = runAllCmds(cmds, verbose, delTempFiles)
    return rc

  generatedObjects = []
  #this is more complicated - we have to use clang to do a source to source transformation
  #then we need to run the compiler on that modified source
  for srcFile in sourceFiles:
    target = args.output
    if not target or len(givenObjects) > 1 or not args.compile:
      srcName = os.path.split(srcFile)[-1]
      target = swapSuffix("o", srcName)
    generatedObjects.append(target)
    if runClang:
      addSrc2SrcCompile(ctx, srcFile, target, args, cmds)
    elif args.sst_component:
      addComponentCompile(ctx, srcFile, target, args, cmds)
    else:
      addCompile(ctx, srcFile, target, args, cmds)

  allObjects = generatedObjects[:]
  allObjects.extend(givenObjects)
  if runLinker:
    addLink(ctx, ldTarget, args, cmds, allObjects)
    if makeBashExe:
      objects = allObjects[:]
      objects.append("-lsstmac_main")
      addLink(ctx, ldTarget + "_validate", args, cmds, objects, toExe=True)


  rc = runAllCmds(cmds, verbose, delTempFiles)
  if not rc == 0: return rc

  if makeBashExe:
    rc = createBashWrapper(compiler, exeName, ldTarget, sstCore, sstmacExe)
    if not rc == 0: return rc

  return 0







