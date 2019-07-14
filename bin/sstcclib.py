
helpText = """The following environmental variables can be defined for the SST compiler
SSTMAC_VERBOSE=0 or 1:        produce verbose output from the SST compiler (default 0)
SSTMAC_DELETE_TEMPS=0 or 1:   remove all temp source-to-source files (default 1)
SSTMAC_SRC2SRC=0 or 1: run a source-to-source pass converting globals to TLS (default 1)
SSTMAC_CONFIG=0: running automake, cmake - skip certain steps to fool build system
"""


def getProcName():
  import os
  import commands
  import sys
  pid = int(os.getppid())
  runCmds = commands.getoutput("ps -p %d" % pid).splitlines()[-1].split()
  runCmds = runCmds[3:]
  firstCmd = runCmds[0].lstrip("-")
  if firstCmd in ("/bin/sh", "sh", "bash", "/bin/bash", "tcsh", "/bin/tcsh", "zsh", "/bin/zsh"):
    if len(runCmds) > 1: #it might just be bash
      firstCmd = runCmds[1]
  cmd = os.path.split(firstCmd)[-1]
  return cmd

def argify(x):
  if ' ' in x: 
    return "'%s'" % x
  else:
    return x

def delete(files):
  import traceback
  import os
  os.system("rm -f %s" % (" ".join(files)))

def swapSuffix(suffix, path):
  splitter = path.split(".")[:-1]
  splitter.append(suffix)
  return ".".join(splitter)

def rebaseFolder(path, srcDir, dstDir):
  folder, fname = os.path.split(path)
  newBaseFolder = folder.replace(srcDir,dstDir)
  return os.path.join(newBaseFolder, fname)


def addPrefix(prefix, path):
  import os
  splitPath = os.path.split(path)
  if len(splitPath) == 2:
    return os.path.join(splitPath[0], prefix + splitPath[1])
  else:
    return prefix + path

def addPrefixAndRebase(prefix, path, newBase):
  import os
  newPath = addPrefix(prefix, path)
  folder, name = os.path.split(newPath)
  return os.path.join(newBase, name)

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
    if self.doDelete:
        if self.verbose:
          sys.stderr.write("%s\n" % cmd)
        os.system(cmd)

def addLlvmPasses(passList, passStr):
  passList.extend(passStr.split(","))

def run(typ, extraLibs="", includeMain=True, makeLibrary=False, redefineSymbols=True, runClang=True):
  extraLibsStr = extraLibs
  extraLibs = extraLibs.split()
  import os
  import sys
  import platform
  from configlib import getstatusoutput
  from sstccvars import sstLdFlags, sstCppFlags
  from sstccvars import prefix, execPrefix, includeDir, cc, cxx
  from sstccvars import sstCxxFlagsStr, sstCFlagsStr
  from sstccvars import includeDir
  from sstccvars import sstCore
  from sstccvars import soFlagsStr
  from sstccvars import clangCppFlagsStr, clangLdFlagsStr
  from sstccvars import clangLibtoolingCxxFlagsStr, clangLibtoolingCFlagsStr
  from sstccvars import haveFloat128
  from sstccvars import defaultIncludePaths

  needfPIC = "fPIC" in sstCxxFlagsStr

  rawPaths = defaultIncludePaths.split(":")
  cleanPaths = []
  for path in rawPaths:
    cleanPaths.append(os.path.abspath(path))
  defaultIncludePaths = ":".join(cleanPaths)

  if not os.environ.has_key("SSTMAC_HEADERS"):
    topdir = os.getcwd()
    #unwind to look for a file named sstmac_headers
    validPath = True
    while os.getcwd() != "/":
      if os.path.isfile("sstmac_headers"):
        headerPath = os.path.join(os.getcwd(), "sstmac_headers")
        os.environ["SSTMAC_HEADERS"] = headerPath
        break
      os.chdir("..")
    os.chdir(topdir)
  
  memoizing = False
  if os.environ.has_key("SSTMAC_MEMOIZE"):
    val = int(os.environ["SSTMAC_MEMOIZE"])
    memoizing = bool(val)

  skeletonizing = False
  if os.environ.has_key("SSTMAC_SKELETONIZE"):
    val = int(os.environ["SSTMAC_SKELETONIZE"])
    skeletonizing = bool(val)

  def cleanFlag(flag):
    return flag.replace("${includedir}", includeDir).replace("${exec_prefix}", execPrefix).replace("${prefix}",prefix)

  sstLibs = []
  if not sstCore:
    sstLibs = [
      '-lsstmac',
      '-lsprockit',
      '-lundumpi',
    ]

  defaultIncludePaths += ":" + cleanFlag(includeDir)

  clangCppArgs = [
    cleanFlag("-I${includedir}/sstmac/clang_replacements"),
  ]

  verbose = False     #whether to print verbose output
  delTempFiles = True #whether to delete all temp files created
  keepExe = False     #whether to keep exes as exes or convert to SST libX.so
  if "SSTMAC_VERBOSE" in os.environ:
    flag = int(os.environ["SSTMAC_VERBOSE"])
    verbose = verbose or flag
  if "SSTMAC_DELETE_TEMPS" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
    delTempFiles = delTempFiles and flag
  if "SSTMAC_CONFIG" in os.environ:
    flag = int(os.environ["SSTMAC_CONFIG"])
    keepExe = flag

  parentProc = os.path.split(getProcName())[-1].strip()
  if (parentProc == "configure" or parentProc == "cmake"):
    makeBashExe = True

  haveClangSrcToSrc = bool(clangCppFlagsStr)
  clangDeglobal = None
  if haveClangSrcToSrc:
    clangDeglobal = os.path.join(prefix, "bin", "sstmac_clang")

  libDir = os.path.join(prefix, "lib")

     
  newCppFlags = []
  for entry in sstCppFlags:
    newCppFlags.append(cleanFlag(entry))
  sstCppFlags = newCppFlags


  sysargs = sys.argv[1:]
  asmFiles = False
  givenFlags = []
  controlArgs = []
  compileOnlyArgs = []
  linkerArgs = []
  sourceFiles = []
  objectFiles = []
  warningArgs = []
  givenOptFlags = []
  forwardedClangArgs = []
  objTarget = None
  ldTarget = None
  getObjTarget = False
  givenStdFlag = None
  validGccArgs = []
  llvmPasses = []
  for arg in sysargs:
    eatArg = False
    sarg = arg.strip().strip("'")
    #okay, well, the flags might have literal quotes in them
    #which get lost passing into here - restore all " to literal quotes
    sarg = sarg.replace("\"",r'\"')
    sarg = sarg.replace(" ", r'\ ')
    if sarg.endswith('.o'):
      objectFiles.append(sarg)
      if getObjTarget:
        objTarget = sarg
        getObjTarget=False
    elif sarg == "--skeletonize":
      eatArg = True
      if "=" in sarg:
        passStr = sarg.split("=")[1]
        addLlvmPasses(llvmPasses, passStr, prefix)
      #make sure if given twice only forwarded once
      if not skeletonizing:
        forwardedClangArgs.append(sarg)
      skeletonizing = True
    elif sarg == "--keep-exe":
      keepExe = True
      eatArg = True
    elif sarg.startswith("--memoize"):
      if "=" in sarg:
        passStr = sarg.split("=")[1]
        addLlvmPasses(llvmPasses, passStr)
      eatArg = True
      if not memoizing: #in case given twice
        forwardedClangArgs.append(sarg)
      memoizing = True
    elif sarg.startswith("-Wl"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-W"):
      warningArgs.append(sarg)
      givenFlags.append(sarg)
    elif sarg[:6] == "-gstab": 
      #for some reason, gstab seems to break 
      #everything when using GNU compiler/linker
      givenFlags.append("-g")
    elif sarg == "-g3":
      compileOnlyArgs.append(sarg)
    elif sarg.startswith("-L"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-l"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-O"):
      givenFlags.append(sarg)
      givenOptFlags.append(sarg)
    elif sarg == "-fPIC":
      givenFlags.append(sarg)
      givenOptFlags.append(sarg)
    elif sarg == "-g":
      givenFlags.append(sarg)
      givenOptFlags.append(sarg)
    elif sarg == "-fvisibility=hidden":
      #this foobars everything - see GitHub issue #259
      pass
    elif "-std=" in sarg:
      givenStdFlag=sarg
      if "98" in givenStdFlag: #this is probably cmake being a jack-donkey
        givenStdFlag="-std=c++1y"
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c') \
                               or sarg.endswith(".cxx") or sarg.endswith(".C"):
      sourceFiles.append(sarg)
    elif sarg.endswith('.S') or sarg.endswith(".s"):
      asmFiles = True
    elif sarg == "--verbose":
      verbose = True
    elif sarg in ("-c","-E"):
      controlArgs.append(sarg)
    elif sarg in ('-o',):
      getObjTarget=True
    elif getObjTarget:
      ldTarget = sarg
      getObjTarget=False
    else:
      givenFlags.append(sarg)

    if not eatArg:
      validGccArgs.append(sarg)

  if not "-fPIC" in givenFlags and needfPIC:
    #assume fPIC was given
    givenFlags.append("-fPIC")

  if keepExe and sstCore:
    sys.exit("Running with sst-core does not allow --keep-exe - must create libX.so")

  if runClang and haveClangSrcToSrc:
    sstCppFlags.append("-DSSTMAC_NO_REFACTOR_MAIN")
  if memoizing:
    sstCppFlags.append("-DSSTMAC_NO_REPLACEMENTS")
  #always indicate that we are compiling an external skeleton app
  sstCppFlags.append("-DSSTMAC_EXTERNAL")

  newLdFlags = []
  for entry in sstLdFlags:
    newLdFlags.append(cleanFlag(entry))
  for entry in sstLibs:
    newLdFlags.append(cleanFlag(entry))
  sstLdFlags = newLdFlags

  sstCppFlagsStr=" ".join(sstCppFlags)
  sstLdFlagsStr =  " ".join(sstLdFlags)
  ld = cc 
  repldir = os.path.join(includeDir, "sstmac", "replacements")
  repldir = cleanFlag(repldir)

  if ldTarget:
    if ldTarget.startswith("lib") and ldTarget.endswith("so"):
      includeMain = False
    

  clangCxxArgs = [
    "-stdlib=libc++", 
  ]
  clangCxxArgs.extend(clangLibtoolingCxxFlagsStr.strip().split())
  if givenStdFlag:
    clangCxxArgs.append(givenStdFlag)
  else:
    clangCxxArgs.append("-std=c++1y")

  if not haveFloat128:
    clangCxxArgs.append("-D__float128=clangFloat128Fix")
  
  exeFromSrc = sourceFiles and not '-c' in sysargs

  if sstCore:
    givenFlags.append(" -DSSTMAC_EXTERNAL_SKELETON")


  directIncludes = []
  
  if typ == "c++":
    directIncludes.append("-include cstdint")
  else:
    directIncludes.append("-include stdint.h")
  directIncludes.append("-include sstmac/compute.h")

  src2src = True
  if "SSTMAC_SRC2SRC" in os.environ:
    src2src = int(os.environ["SSTMAC_SRC2SRC"])

  runClang = haveClangSrcToSrc and src2src and runClang

  if sys.argv[1] == "--version" or sys.argv[1] == "-V":
    import inspect, os
    pathStr = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
    print(pathStr)
    cmd = "%s --version" % (cxx)
    os.system(cmd)
    sys.exit()
  elif sys.argv[1] == "--flags":
    sys.stderr.write("LDFLAGS=%s\n" % sstLdFlagsStr)
    sys.stderr.write("CPPFLAGS=%s\n" % sstCppFlagsStr)
    sys.stderr.write("CXXFLAGS=%s\n" % sstCxxFlagsStr)
    sys.exit()
  elif sys.argv[1] == "--help":
    cmd = "%s --help" % (cxx)
    os.system(cmd)
    sys.stderr.write(helpText)
    sys.exit()
  elif sys.argv[1] == "--prefix":
    sys.stdout.write("%s\n" % prefix)
    sys.exit()

  sstCompilerFlagsStr = ""
  compiler = ""
  cxxCmd = ""
  if includeMain:
    extraLibsStr += " -lsstmac_main"
  #always c++ no matter what for now
  if typ.lower() == "c++":
    sstCompilerFlagsStr = cleanFlag(sstCxxFlagsStr)
    compiler = cxx
    ld = cxx
  elif typ.lower() == "c":
    sstCompilerFlagsStr = cleanFlag(sstCFlagsStr)
    compiler = cc
    if runClang:
      ld = cxx
    else:
      ld = cc #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game

  sstCompilerFlags = []
  sstStdFlag = None
  for flag in sstCompilerFlagsStr.split():
    if "-std=" in flag:
      sstStdFlag = flag
    elif not flag.startswith("-O") and not flag == "-g":
      sstCompilerFlags.append(flag)
  sstCompilerFlagsStr = " ".join(sstCompilerFlags)

  sstCxxFlagsStr = cleanFlag(sstCxxFlagsStr)
  sstCxxFlags = []
  sstStdFlag = None
  for flag in sstCxxFlagsStr.split():
    if not flag.startswith("-O") and not flag == "-g":
      sstCxxFlags.append(flag)
    if "-std=" in flag:
      #don't automatically propagate the sst c++11 flag...
      sstStdFlag = flag
  sstCxxFlagsStr = " ".join(sstCxxFlags)
    
  sstCFlagsStr = cleanFlag(sstCFlagsStr)
  sstCFlags = []
  for flag in sstCFlagsStr.split():
    if not flag.startswith("-O") and not flag == "-g":
      sstCFlags.append(flag)
  sstCFlagsStr = " ".join(sstCFlags)

  objectFilesStr = " ".join(objectFiles)

  sstCompilerFlagsArr = sstCompilerFlagsStr.split()
  sstCompilerFlags = []
  for entry in sstCompilerFlagsArr:
    if entry[:2] == "-O": #do not send optimization flags forward
      pass
    elif entry == "-g": #do not send debug flags forward
      pass
    else:
      sstCompilerFlags.append(entry)
  sstCompilerFlagsStr = " ".join(sstCompilerFlags)

  compileOnlyArgsStr = " ".join(compileOnlyArgs)

  #okay, figure out which -std flag to include in compilation
  #treat it as a given flag on the command line

  if typ == "c++":
    if sstStdFlag and givenStdFlag and sstStdFlag != givenStdFlag:
      sys.stderr.write("WARNING: SST compiled with %s, but app compiled with %s\n" % (sstStdFlag, givenStdFlag))
      givenFlags.append(givenStdFlag)
    elif sstStdFlag:
      givenFlags.append(sstStdFlag)
    elif givenStdFlag:
      givenFlags.append(givenStdFlag)
    else:
      pass
      #this flag is no longer required in newer compilers, c++11 might be default
      #sys.stderr.write("no -std= flag obtained from SST - how did you compiled without C++11 or greater?")
      #return 1
  else:
    if givenStdFlag:
      givenFlags.append(givenStdFlag)
    

  directIncludesStr = " ".join(directIncludes)

  extraCppFlags = []
  if redefineSymbols:
    extraCppFlags = [
      "-I%s/include/sumi" % prefix,
      "-DSSTMAC=1",
    ]
    # "-D__thread=thread_local_not_yet_allowed",
    # "-Dthread_local=thread_local_not_yet_allowed",

  if asmFiles:
    #just execute the command as-is with no frills
    cmdArr = [
      cc
    ]
    cmdArr.extend(validGccArgs)
    cmd = " ".join(cmdArr)
    if verbose:
      sys.stderr.write("%s\n" % cmd)
    rc = os.system(cmd)
    return rc


  if "--no-integrated-cpp" in sysargs:
    extraCppFlags = [] #add nothing

  ldpathMaker = "-Wl,-rpath,%s/lib" % prefix

  if redefineSymbols: 
    extraCppFlags.insert(0,"-I%s" % repldir)

  llvmPassesArr = []
  if llvmPasses:
    llvmPassesArr.append("-Xclang")
    llvmPassesArr.append("-disable-O0-optnone")
  for passName in llvmPasses:
    llvmPassesArr.append("-Xclang")
    llvmPassesArr.append("-load")
    llvmPassesArr.append("-Xclang")
    fullName = "lib%s.so" % passName
    llvmPassesArr.append(os.path.join(prefix, "lib", fullName))
  llvmPassesStr = " ".join(llvmPassesArr)
  
  cxxCmdArr = []
  ldCmdArr = []
  arCmdArr = []
  ppOnly = "-E" in controlArgs
  controlArgStr = " ".join(controlArgs)
  extraCppFlagsStr = " ".join(extraCppFlags)
  givenFlagsStr = " ".join(givenFlags)
  sourceFilesStr = " ".join(sourceFiles)
  #We need to separate specific flags 
  srcFileStr = " ".join(sourceFiles)
  ppCmdArr = [
    compiler, 
    "-include sstmac/skeleton.h",
    directIncludesStr,
    extraCppFlagsStr, 
    givenFlagsStr,
    sstCppFlagsStr,
    sstCompilerFlagsStr, 
    "-E"
  ]
  sourceFileCompileFlags = [
    sourceFilesStr,
    sstCppFlagsStr,
    extraCppFlagsStr,
    compileOnlyArgsStr,
    givenFlagsStr,
    sstCompilerFlagsStr
  ]

  if '-c' in sysargs or ppOnly:
    runClang = runClang and (not ppOnly)
    if runClang:
      #we run clang on a direct source file with no includes
      #only put cxxflags in the cmd arr for now
      cxxCmdArr = [
        compiler,
        sstCompilerFlagsStr,
        givenFlagsStr
      ]
    else: 
      cxxCmdArr = [
        compiler, 
        extraCppFlagsStr, 
        givenFlagsStr,
        sstCppFlagsStr, 
        sstCompilerFlagsStr, 
        controlArgStr,
        srcFileStr
      ]
      if objTarget:
        folder, obj = os.path.split(objTarget)
        if folder:
          if not os.path.isdir(folder):
            try: os.makedirs(folder)
            except OSError: pass
        cxxCmdArr.append("-o")
        cxxCmdArr.append(objTarget)
    allCompilerFlags = sstCxxFlagsStr + sstCppFlagsStr + givenFlagsStr
    #if not "fPIC" in allCompilerFlags:
    #  sys.stderr.write("Linker/dlopen may eventually fail on .so file: fPIC not in C/CXXFLAGS\n")
  elif objTarget and sourceFiles:
    cxxCmdArr = [
      compiler, 
      extraCppFlagsStr, 
      sstCppFlagsStr, 
      givenFlagsStr,
      sstCompilerFlagsStr, 
      givenFlagsStr,
      sourceFilesStr,
      sstLdflagsStr, 
      extraLibsStr, 
      ldpathMaker
    ]
  else: #we are building an exe or a lib
    if not ldTarget: ldTarget = "a.out"
    #linking executable/lib from object files (or source files)
    runClang = runClang and sourceFiles

    if not keepExe: #turn exe into a library
      libTarget = ldTarget
      arCmdArr = [
        ld,
        soFlagsStr,
        objectFilesStr,
        sstLdFlagsStr,
        givenFlagsStr,
        sstCompilerFlagsStr,
        ldpathMaker,
        "-o",
        libTarget
      ]
      if sourceFiles and not runClang: 
        arCmdArr.extend(sourceFileCompileFlags)
      arCmdArr.extend(linkerArgs)
    else: #keep commands as they are
      ldCmdArr = [
        ld,
        objectFilesStr,
        extraLibsStr,
        sstLdFlagsStr, 
        givenFlagsStr,
        sstCompilerFlagsStr,
        extraLibsStr, 
        ldpathMaker,
        "-o",
        ldTarget
      ]
      ldCmdArr.extend(linkerArgs)
      if sourceFiles and not runClang: 
        ldCmdArr.extend(sourceFileCompileFlags)

  clangExtraArgs = []
  #if sourceFiles and len(objectFiles) > 1:
  #  sys.exit("Specified multiple object files for source compilation: %" % " ".join(objectFiles))
  if runClang:
    #this is more complicated - we have to use clang to do a source to source transformation
    #then we need to run the compiler on that modified source
    allTemps = TempFiles(delTempFiles, verbose)
    for srcFile in sourceFiles:
      target = objTarget
      if not objTarget or len(objectFiles) > 1:
        srcName = os.path.split(srcFile)[-1]
        target = swapSuffix("o", srcName)
      objBaseFolder, objName = os.path.split(target)

      ppTmpFile = addPrefixAndRebase("pp.",srcFile, objBaseFolder)
      cmdArr = ppCmdArr[:]
      cmdArr.append(srcFile)
      cmdArr.append("> %s" % ppTmpFile)
      ppCmd = " ".join(cmdArr) 
      allTemps.append(ppTmpFile)
      if verbose: sys.stderr.write("%s\n" % ppCmd)
      rc = os.system(ppCmd)
      if not rc == 0:
        allTemps.cleanUp()
        return rc

      ppText = open(ppTmpFile).read()

      srcRepl = addPrefixAndRebase("sst.pp.",srcFile,objBaseFolder)
      cxxInitSrcFile = addPrefixAndRebase("sstGlobals.pp.",srcFile,objBaseFolder) + ".cpp"

      clangCmdArr = [clangDeglobal]
      #don't use the clang --extra-arg anymore - put them after the '--'
      clangCmdArr.append(ppTmpFile)
      clangCmdArr.extend(forwardedClangArgs)
      clangCmdArr.append("--system-includes=%s" % defaultIncludePaths)
      clangCmdArr.append("--")
      #all of the compiler options go after the -- separator
      #fix intrinsics which might not be known to clang if using a different compiler
      intrinsicsFixerPath = os.path.join(cleanFlag(includeDir), "sstmac", "replacements", "fixIntrinsics.h")
      intrinsicsFixer = "-include%s" % intrinsicsFixerPath
      clangCmdArr.append(intrinsicsFixer)
      if typ == "c++":
        clangCmdArr.extend(clangCxxArgs)
        clangCmdArr.extend(clangLibtoolingCxxFlagsStr.split())
      else:
        clangCmdArr.extend(clangLibtoolingCFlagsStr.split())
      clangCmdArr.extend(warningArgs)
      clangCmd = " ".join(clangCmdArr)
      if verbose: sys.stderr.write("%s\n" % clangCmd)
      rc = os.system(clangCmd)
      allTemps.append(srcRepl)
      allTemps.append(cxxInitSrcFile)
      if not rc == 0:
        allTemps.cleanUp()
        return rc

      #the source to source generates temp .cc files
      #we need the compile command to generate .o files from the temp .cc files
      #update the command to point to them
      cmdArr = [
        compiler, 
        extraCppFlagsStr, 
        sstCppFlagsStr, 
        sstCompilerFlagsStr, 
        llvmPassesStr,
        givenFlagsStr
      ]


      tmpTarget = addPrefix("tmp.", target)
      allTemps.append(tmpTarget)
      cmdArr.append("-o")
      cmdArr.append(tmpTarget)
      cmdArr.append("-c")
      cmdArr.append(srcRepl)
      cmdArr.append("--no-integrated-cpp")
      cxxCmd = " ".join(cmdArr)
      if verbose: sys.stderr.write("%s\n" % cxxCmd)
      rc = os.system(cxxCmd)
      if not rc == 0:
        allTemps.cleanUp()
        return rc

      #now we generate the .o file containing the CXX linkage 
      #for global variable CXX init - because C is stupid
      cxxInitObjFile = addPrefix("sstGlobals.",target)
      allTemps.append(cxxInitObjFile)
      cxxInitCmdArr = [
        cxx,
        sstCxxFlagsStr,
        sstCppFlagsStr,
        "-o",
        cxxInitObjFile,
        "-I%s/include" % prefix,
        "-c",
        cxxInitSrcFile
      ]
      cxxInitCmdArr.extend(givenOptFlags)
      cxxInitCompileCmd = " ".join(cxxInitCmdArr)
      if verbose: sys.stderr.write("%s\n" % cxxInitCompileCmd)
      rc = os.system(cxxInitCompileCmd)
      allTemps.append(cxxInitObjFile)
      if not rc == 0:
        allTemps.cleanUp()
        return rc

      linker = "ld -r"
      if not platform.system() == "Darwin":
        linker += " --unique"
      
      mergeCmdArr = [
        linker, "-o",
        target,
        tmpTarget, cxxInitObjFile
      ]
      mergeCmd = " ".join(mergeCmdArr)
      if verbose: sys.stderr.write("%s\n" % mergeCmd)
      rc = os.system(mergeCmd)
      if not rc == 0:
        return rc


    #some generate multiple .o files at once, I don't know why
    manyObjects = objTarget == None #no specific target specified
    allObjects = []
    for srcFile in sourceFiles:
      srcFileNoSuffix = ".".join(srcFile.split(".")[:-1])
      cxxInitObjFile = addPrefix("sstGlobals.", swapSuffix("o",srcFile))
      if exeFromSrc:
        newFile = objTarget
        if not objTarget:
          newFile = swapSuffix("o", srcFile)
        allObjects.append(newFile)
        allTemps.append(newFile)

    if exeFromSrc:
      if ldCmdArr:
        ldCmdArr.extend(allObjects)
        rc = runCmdArr(ldCmdArr,verbose)
        if not rc == 0: return rc
      if arCmdArr:
        arCmdArr.extend(allObjects)
        rc = runCmdArr(arCmdArr,verbose)
        if not rc == 0: return rc
      if delTempFiles:
        delete(allObjects)
    return 0

  if not runClang:
    rc = runCmdArr(cxxCmdArr,verbose)
    if not rc == 0: return rc
    rc = runCmdArr(ldCmdArr,verbose)
    if not rc == 0: return rc
    rc = runCmdArr(arCmdArr,verbose)
    return rc




