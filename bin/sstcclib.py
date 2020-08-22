"""
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
"""

helpText = """The following environmental variables can be defined for the SST compiler
SSTMAC_VERBOSE=0 or 1:        produce verbose output from the SST compiler (default 0)
SSTMAC_DELETE_TEMPS=0 or 1:   remove all temp source-to-source files (default 1)
SSTMAC_DELETE_TEMP_OFILES=0 or 1:   remove all temporary object files (default 1)
SSTMAC_CONFIG=0: running automake, cmake - skip certain steps to fool build system
"""

def createBashWrapper(compiler, exeName, ldTarget, sstCore, sstmacExe):
  import configlib
  #there is one scenario in which autoconf actually WANTS
  #this to fail... check for it now
  output = configlib.getoutput("nm %s | grep some_bogus_nonexistent_symbol" % ldTarget)
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
   configlib.read_utf8(ldTarget),
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
  def __init__(self, doDeleteSources, doDeleteObjects, verbose, clangBin):
    self.doDeleteSources = doDeleteSources
    self.doDeleteObjects = doDeleteObjects
    self.verbose = verbose
    self.files = []
    self.clangBin = clangBin

  def append(self, f):
    self.files.append(f)

  def __del__(self):
    self.cleanUp()

  def cleanUp(self):
    import os
    import sys
    import traceback
    objects = [f for f in self.files if f.endswith('.o')]
    sources = [f for f in self.files if not f.endswith('.o')]
    if self.doDeleteSources and sources:
        cmdall = "rm -f %s" % " ".join(sources)
        if self.verbose:
          sys.stderr.write("%s\n" % cmdall)
        os.system(cmdall)
        self.files = []
    if self.doDeleteObjects and objects:
        cmdobjects = "rm -f %s" % " ".join(objects)
        if self.verbose:
          sys.stderr.write("%s\n" % cmdobjects)
        os.system(cmdobjects)
    if not self.doDeleteSources: # attempt to format the files with clangformat
        # taken from https://stackoverflow.com/questions/377017/test-if-executable-exists-in-python/12611523
        clang_format_prog = "clang-format"
        clang_format = os.path.join(self.clangBin + "bin/", clang_format_prog)
        has_clang_format = os.path.isfile(clang_format) and os.access(clang_format, os.X_OK)

        if not has_clang_format: # Look for one in the path
            for path in os.environ["PATH"].split(os.pathsep):
                exe = os.path.join(path, clang_format_prog)
                if os.path.isfile(exe) and os.access(exe, os.X_OK):
                    has_clang_format = True
                    clang_format = exe
                    break

        if self.verbose:
            if has_clang_format:
                sys.stderr.write("Attempting to format temp files with %s\n" % clang_format)
            else:
                sys.stderr.write("Could not find %s\n" % clang_format)

        if has_clang_format:
            for f in sources:
                cmd = clang_format + " -i -style=llvm " + f 
                if self.verbose:
                    sys.stderr.write(cmd + "\n")
                os.system(cmd)

def runAllCmds(cmds, verbose, doDeleteSources, doDeleteObjects, clangBin):
  tmpFiles = TempFiles(doDeleteSources, doDeleteObjects, verbose, clangBin)
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
    child = Popen([x.strip() for x in cmdArr],stdout=stdout)
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
  SKELETONIZE = 0
  MEMOIZE = 1
  COMPONENT = 2
  NONE = 3

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
    self.mode = self.NONE
    self.replacementIncludes = []
    self.sstCore = False
    self.hasClang = False
    self.src2srcDebug = False

  def simulateMode(self):
    return self.mode == self.SKELETONIZE

  def srcToSrc(self):
    return self.hasClang and self.simulateMode()

  def modeString(self):
    if self.mode == self.SKELETONIZE: return "SKELETONIZE"
    if self.mode == self.MEMOIZE:     return "MEMOIZE"
    if self.mode == self.COMPONENT:   return "COMPONENT"
    if self.mode == self.NONE:        return "NONE"

  def setMode(self, mode):
    if self.mode != self.NONE:
      sys.exit("Mode already set to %s - undefined behavior to also use --sst_component" % self.modeString())
    self.mode = mode

  def setDefaultMode(self, mode):
    if self.mode == self.NONE:
      self.mode = mode

def run(typ, extraLibs=""):
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
  from sstccvars import clangBin
  from sstccvars import clangCppFlagsStr, clangLdFlagsStr
  from sstccutils import cleanFlag, getProcTree, swapSuffix


  needfPIC = "fPIC" in sstCxxFlagsStr

  sstmacExe = cleanFlag(os.path.join(prefix, "bin", "sstmac"))

  verbose = False     #whether to print verbose output
  if "SSTMAC_VERBOSE" in os.environ:
    flag = int(os.environ["SSTMAC_VERBOSE"])
    verbose = verbose or flag

  #whether to make a shared object for loading
  #or a bash script that emulates an executable
  makeBashExe = False
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


  import argparse
  parser = argparse.ArgumentParser(description='Process flags for the SST/macro compiler wrapper')
  parser.add_argument('-o', '--output', type=str,  
                    help="the linker/compilation target")
  parser.add_argument('--keep-exe', default=False, action="store_true",
                    help="whether to create an executable script or build a loadable shared object")
  parser.add_argument('--skeletonize', type=str,
                    help="whether to activate skeletonization mode, stripping compute and mem allocation. Can take a list of LLVM passes as argument.")
  parser.add_argument('--memoize', type=str,
                    help="whether to activate memoization mode that instruments and records execution. Can take a list of LLVM passes as argument.")
  parser.add_argument('-I', action="append", type=str, help="an include path", default=[])
  parser.add_argument('-D', action="append", type=str, help="a defines", default=[])
  parser.add_argument('-L', action="append", type=str, help="a library path", default=[])
  parser.add_argument('-l', action="append", type=str, help="a library to link against", default=[])
  parser.add_argument('-O', type=str)
  parser.add_argument('-g', action="store_true", default=False)
  parser.add_argument('-c', '--compile', default=False, action="store_true")
  parser.add_argument('-E', '--preprocess', default=False, action="store_true")
  parser.add_argument('-V', '--version', default=False, action="store_true", help="Print SST and compiler version info")
  parser.add_argument('--disable-mpi', default=False, action="store_true", help="Do not include virtual MPI environment")
  parser.add_argument('--flags', default=False, action="store_true", help="Print the extra flags SST automatically adds")
  parser.add_argument('--prefix', default=False, action="store_true", help="Print the SST installation prefix")
  parser.add_argument('--sst-component', default=False, action="store_true", 
                      help="Whether we are building an SST component and should skip all source-to-source")
  parser.add_argument('-fPIC', default=False, action="store_true", help="compile for position independent code")
  parser.add_argument('-std', type=str, help="specify the standard level for C or C++")
  parser.add_argument('--no-integrated-cpp', default=False, action="store_true", help="whether to skip preprocessing")

  parser.add_argument("-fvisibility", type=str, help="control the visibility of certain symbols")
  parser.add_argument("--host-cxx", type=str, help="override the C++ compiler used underneath from the one used to build SST/macro")
  parser.add_argument("--host-cc", type=str, help="override the C compiler used underneath from the one used to build SST/macro")
  
  args, extraArgs = parser.parse_known_args()

  ctx = Context()

  for entry in sstCppFlags:
    clean = cleanFlag(entry)
    if clean: #don't add empty flags
      ctx.cppFlags.append(clean)

  ctx.sstCore = sstCore
  ctx.cc = spackcc if spackcc else cc
  ctx.cxx = spackcxx if spackcxx else cxx
  ctx.typ = typ
  ctx.sstCore = sstCore
  ctx.hasClang = bool(clangCppFlagsStr)


  #it is possible to override the host compilers use to do preprocessing/compilation
  if args.host_cxx:
    ctx.cxx = args.host_cxx
  elif "SSTMAC_CXX" in os.environ:
    ctx.cxx = os.environ["SSTMAC_CXX"]

  if args.host_cc:
    ctx.cc = args.host_cc
  elif "SSTMAC_CC" in os.environ:
    ctx.cc = os.environ["SSTMAC_CC"]

  if args.no_integrated_cpp:
    sys.exit("SST compiler wrapper cannot handle --no-integrated-cpp flag")

  #keep visibility arg, but not if it causes hidden symbols
  if args.fvisibility and args.fvisibility != "hidden": 
    ctx.compilerFlags.append("-fvisibility=%s" % args.fvisibility)

  delTempObjectFiles = True #whether to delete all temporary object files created
  delTempSourceFiles = True #whether to delete all temporary source files created

  if "SSTMAC_DELETE_TEMPS" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
    delTempSourceFiles = bool(flag)
    delTempObjectFiles = bool(flag)

  if "SSTMAC_DELETE_TEMP_OBJECTS" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMP_OBJECTS"])
    delTempObjectFiles = bool(flag)
  elif args.g: #debug build, don't delete temps
    delTempObjectFiles = False

  if "SSTMAC_DELETE_TEMP_SOURCES" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMP_SOURCES"])
    delTempSourceFiles = bool(flag)

  skeletonizing = False
  if "SSTMAC_SKELETONIZE" in os.environ:
    val = int(os.environ["SSTMAC_SKELETONIZE"])
    skeletonizing = bool(val)
  if skeletonizing or (not args.skeletonize is None):
    ctx.clangArgs.append("--skeletonize")
    ctx.setMode(ctx.SKELETONIZE)

  memoizing = False
  if "SSTMAC_MEMOIZE" in os.environ:
    val = int(os.environ["SSTMAC_MEMOIZE"])
    memoizing = bool(val)
  if memoizing or (not args.memoize is None):
    ctx.clangArgs.append("--memoize")
    ctx.setMode(ctx.MEMOIZE)

  if "SSTMAC_DEBUG_SRC2SRC" in os.environ:
    ctx.src2srcDebug = int(os.environ["SSTMAC_DEBUG_SRC2SRC"])

  if args.sst_component:
    ctx.setMode(ctx.COMPONENT)

  #unless told otherwise, I am skeletonizinng
  ctx.setDefaultMode(ctx.SKELETONIZE)
  from sstcompile import addModeDefines
  addModeDefines(ctx, args)

  from sstlink import addModeLinks
  addModeLinks(ctx, args)
  
  #this is probably cmake being a jack-donkey during configure, overwrite it
  if args.std == "c++98": args.std = "c++1y"

  #if we are in simulate mode, we have to create the "replacement" environment
  #we do this by rerouting all the headers to SST/macro headers
  if ctx.simulateMode():
    include_root = cleanFlag(includeDir)
    repldir = os.path.join(include_root, "sstmac", "replacements")
    repldir = cleanFlag(repldir)
    args.I.append(os.path.join(include_root, "sumi"))
    args.I.insert(0,repldir)

    #also force inclusion of wrappers
    if typ == "c++":
      ctx.directIncludes.append("cstdint")
    else:
      ctx.directIncludes.append("stdint.h")
    ctx.directIncludes.append("sstmac/compute.h")
    ctx.directIncludes.append("sstmac/skeleton.h")

    if not args.disable_mpi:
      args.I.insert(0,os.path.join(repldir, "mpi"))

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

  #for now, just assume any time an exe name "conftest"
  #is being built, it comes from configure
  if args.output == "conftest":
    makeBashExe = True

  for entry in sstLdFlags:
    flag = cleanFlag(entry)
    if flag:
      ctx.ldFlags.append(flag)


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
    ctx.compiler = ctx.cxx
    ctx.ld = ctx.cxx
    ctx.compilerFlags = ctx.cxxFlags
    if args.std:
#        let's turn off this warning for now
#      if sstCxxArgs.std and args.std != sstCxxArgs.std:
#        sys.stderr.write("WARNING: SST compiled with %s, but app compiled with %s - choosing app's version\n" % (sstCxxArgs.std, args.std))
      ctx.cxxFlags.append("-std=%s" % args.std)
    elif sstCxxArgs.std:
      ctx.cxxFlags.append("-std=%s" % sstCxxArgs.std)
    else:
      pass
    ctx.compilerFlags = ctx.cxxFlags[:]
  elif typ.lower() == "c":
    ctx.compiler = ctx.cc
    if ctx.hasClang:
      #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game
      ctx.ld = ctx.cxx
    else:
      ctx.ld = ctx.cc
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
    cmd = "%s --version" % (ctx.cxx)
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
      ctx.cc
    ]
    cmdArr.extend(sys.argv[1:])
    cmds = [ [None,cmdArr,[]] ]
    return runAllCmds(cmds, verbose, delTempSourceFiles, delTempObjectFiles, clangBin)

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

  from sstcompile import addSrc2SrcCompile, addComponentCompile
  from sstcompile import addCompile, addPreprocess
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
    rc = runAllCmds(cmds, verbose, delTempSourceFiles, delTempObjectFiles, clangBin)
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
    if ctx.srcToSrc():
      addSrc2SrcCompile(ctx, srcFile, target, args, cmds)
    elif ctx.mode == ctx.COMPONENT:
      addComponentCompile(ctx, srcFile, target, args, cmds)
    else:
      addCompile(ctx, srcFile, target, args, cmds)

  allObjects = generatedObjects[:]
  allObjects.extend(givenObjects)
  if runLinker:
    shouldMakeExe = memoizing
    addLink(ctx, ldTarget, args, cmds, allObjects, shouldMakeExe)
    if makeBashExe:
      objects = allObjects[:]
      objects.append("-lsstmac_main")
      addLink(ctx, ldTarget + "_validate", args, cmds, objects, toExe=True)


  rc = runAllCmds(cmds, verbose, delTempSourceFiles, delTempObjectFiles, clangBin)
  if not rc == 0: return rc

  if makeBashExe:
    rc = createBashWrapper(compiler, exeName, ldTarget, sstCore, sstmacExe)
    if not rc == 0: return rc

  return 0

