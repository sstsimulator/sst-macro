
helpText = """The following environmental variables can be defined for the SST compiler
SSTMAC_VERBOSE=0 or 1:        produce verbose output from the SST compiler (default 0)
SSTMAC_DELETE_TEMPS=0 or 1:   remove all temp source-to-source files (default 1)
SSTMAC_SRC2SRC=0 or 1: run a source-to-source pass converting globals to TLS (default 1)
"""

def argify(x):
  if ' ' in x: 
    return "'%s'" % x
  else:
    return x

def delete(files):
  import os
  os.system("rm -f %s" % (" ".join(files)))

def swapSuffix(suffix, path):
  splitter = path.split(".")[:-1]
  splitter.append(suffix)
  return ".".join(splitter)

def addPrefix(prefix, path):
  import os
  splitPath = os.path.split(path)
  if len(splitPath) == 2:
    return os.path.join(splitPath[0], prefix + splitPath[1])
  else:
    return prefix + path


def addClangArg(a, ret):
  ret.append("--extra-arg=%s" % a)

def addClangArgs(argList, ret):
  for a in argList:
    addClangArg(a,ret)
  return ret
  
def runCmdArr(cmdArr,verbose):
  import sys,os
  if cmdArr:
    cmd = " ".join(cmdArr)
    if verbose: sys.stderr.write("%s\n" % cmd)
    return os.system(cmd)
  else:
    return 0

def run(typ, extraLibs="", includeMain=True, makeLibrary=False, redefineSymbols=True, runClang=True):
  extraLibsStr = extraLibs
  extraLibs = extraLibs.split()
  import os
  import sys
  from configlib import getstatusoutput
  from sstccvars import sstLdFlags, sstCppFlags
  from sstccvars import prefix, execPrefix, includeDir, cc, cxx
  from sstccvars import sstCxxFlagsStr, sstCFlagsStr
  from sstccvars import includeDir
  from sstccvars import sstCore
  from sstccvars import soFlagsStr
  from sstccvars import clangCppFlagsStr, clangLdFlagsStr
  from sstccvars import clangLibtoolingCxxFlagsStr, clangLibtoolingCFlagsStr

  def cleanFlag(flag):
    return flag.replace("${includedir}", includeDir).replace("${exec_prefix}", execPrefix).replace("${prefix}",prefix)

  sstLibs = []
  if not sstCore:
    sstLibs = [
      '-lsstmac',
      '-lsprockit',
      '-lundumpi',
    ]


  clangCppArgs = [
    cleanFlag("-I${includedir}/sstmac/clang_replacements"),
  ]
  clangCxxArgs = [
    "-std=c++1y",
    "-stdlib=libc++", 
  ]
  clangCxxArgs.extend(clangLibtoolingCxxFlagsStr.strip().split())

  verbose = False
  delTempFiles = True
  if "SSTMAC_VERBOSE" in os.environ:
    flag = int(os.environ["SSTMAC_VERBOSE"])
    verbose = verbose or flag
  if "SSTMAC_DELETE_TEMPS" in os.environ:
    flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
    delTempFiles = delTempFiles and flag

  haveClangSrcToSrc = bool(clangCppFlagsStr)
  clangDeglobal = None
  if haveClangSrcToSrc:
    clangDeglobal = os.path.join(prefix, "bin", "sstmac_clang_deglobal")


  newCppFlags = []
  for entry in sstCppFlags:
    newCppFlags.append(cleanFlag(entry))
  sstCppFlags = newCppFlags

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

  sysargs = sys.argv[1:]

  asmFiles = False
  givenFlags = []
  controlArgs = []
  linkerArgs = []
  sourceFiles = []
  objectFiles = []
  warningArgs = []
  optFlags = []
  objTarget = None
  ldTarget = None
  getObjTarget = False
  givenStdFlag = None
  for arg in sysargs:
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
      else:
        cxxInitFile = addPrefix("sstGlobals.", sarg)
        if os.path.isfile(cxxInitFile):
          objectFiles.append(cxxInitFile)
        elif ".libs" in cxxInitFile:
          cxxInitFile = cxxInitFile.replace(".libs/","")
          if os.path.isfile(cxxInitFile):
            objectFiles.append(cxxInitFile)
          else:
            pass
            #sys.stderr.write("no file %s\n" % cxxInitFile)
        else:
          pass
          #sys.stderr.write("no file %s\n" % cxxInitFile)
    elif sarg.startswith("-Wl"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-W"):
      warningArgs.append(sarg)
      givenFlags.append(sarg)
    elif sarg.startswith("-L"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-l"):
      linkerArgs.append(sarg)
    elif sarg.startswith("-O"):
      givenFlags.append(sarg)
      optFlags.append(sarg)
    elif sarg == "-g":
      givenFlags.append(sarg)
      optFlags.append(sarg)
    elif "-std=" in sarg:
      givenStdFlag=sarg
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c') \
                               or sarg.endswith(".cxx") or sarg.endswith(".C"):
      sourceFiles.append(sarg)
    elif sarg.endswith('.S'):
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

  exeFromSrc = sourceFiles and not objectFiles and not '-c' in sysargs

  if sstCore:
    givenFlags.append(" -DSSTMAC_EXTERNAL_SKELETON")

  if sourceFiles and len(objectFiles) > 1:
    sys.exit("Specified multiple object files for source compilation: %" % " ".join(objectFiles))

  directIncludes = []
  
  if type == "c++":
    directIncludes.append("-include cstdint")
  else:
    directIncludes.append("-include stdint.h")
  directIncludes.append("-include sstmac/compute.h")

  src2src = True
  if "SSTMAC_SRC2SRC" in os.environ:
    src2src = int(os.environ["SSTMAC_SRC2SRC"])

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
    ld = cxx #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game

  #okay this is sooo dirty - but autoconf is a disaster to trick
  #if this detects something auto-confish, bail and pass through to regular compiler
  if "conftest.c" in sysargs:
    cmd = "%s %s" % (compiler, " ".join(sysargs))
    sys.stderr.write("passing through on autoconf: %s\n" % cmd) 
    rc = os.system(cmd)
    if rc == 0: return 0
    else: return 1

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

  #okay, figure out which -std flag to include in compilation
  #treat it as a given flag on the command line

  if typ == "c++":
    if sstStdFlag and givenStdFlag:
      #take whichever is greater
      if sstStdFlag > givenStdFlag:
        givenFlags.append(sstStdFlag)
      else:
        givenFlags.append(givenStdFlag)
    elif sstStdFlag:
      givenFlags.append(sstStdFlag)
    elif givenStdFlag:
      givenFlags.append(givenStdFlag)
    else:
      sys.stderr.write("no -std= flag obtained from SST - how did you compiled without C++11 or greater?")
      return 1
    

  directIncludesStr = " ".join(directIncludes)

  extraCppFlags = []
  if redefineSymbols:
    extraCppFlags = [
      "-I%s/include/sumi" % prefix,
      "-DSSTMAC=1",
      "-D__thread=thread_local_not_yet_allowed",
      "-Dthread_local=thread_local_not_yet_allowed",
    ]

  if asmFiles:
    extraCppFlags = [] #add nothing

  if "--no-integrated-cpp" in sysargs:
    extraCppFlags = [] #add nothing

  ldpathMaker = "-Wl,-rpath,%s/lib" % prefix

  if redefineSymbols: 
    extraCppFlags.insert(0,"-I%s" % repldir)

  
  cxxCmdArr = []
  ldCmdArr = []
  arCmdArr = []
  ppOnly = "-E" in controlArgs
  runClang = haveClangSrcToSrc and src2src and runClang
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
    if not "fPIC" in allCompilerFlags:
      sys.stderr.write("Linker/dlopen will eventually fail on .so file: fPIC not in C/CXXFLAGS\n")
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
  else:
    if not ldTarget: ldTarget = "a.out"
    #linking executable/lib from object files (or source files)
    runClang = runClang and sourceFiles

    if not sstCore:
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
    else:
      libTarget = ldTarget
      if not libTarget.endswith(".so"):
        libTarget += ".so"
      if not libTarget.startswith("lib"):
        libTarget = "lib" + libTarget

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

  clangExtraArgs = []
  if runClang:
    #this is more complicated - we have to use clang to do a source to source transformation
    #then we need to run the compiler on that modified source
    for srcFile in sourceFiles:
      ppTmpFile = addPrefix("pp.",srcFile)
      cmdArr = ppCmdArr[:]
      cmdArr.append(srcFile)
      cmdArr.append("> %s" % ppTmpFile)
      ppCmd = " ".join(cmdArr) 
      if verbose: sys.stderr.write("%s\n" % ppCmd)
      rc = os.system(ppCmd)
      if not rc == 0:
        if delTempFiles:
          os.system("rm -f %s" % ppTmpFile)  
        return rc

      ppText = open(ppTmpFile).read()

      srcRepl = addPrefix("sst.pp.",srcFile)
      cxxInitSrcFile = addPrefix("sstGlobals.pp.",srcFile) + ".cpp"

      clangCmdArr = [clangDeglobal]
      if typ == "c++":
        addClangArgs(clangCxxArgs, clangCmdArr)
        addClangArgs(clangLibtoolingCxxFlagsStr.split(), clangCmdArr)
      else:
        addClangArgs(clangLibtoolingCFlagsStr.split(), clangCmdArr)
      #oh, hell, I have to fix intrinsics
      intrinsicsFixerPath = os.path.join(cleanFlag(includeDir), "sstmac", "replacements", "fixIntrinsics.h")
      intrinsicsFixer = "-include%s" % intrinsicsFixerPath
      #addClangArg(intrinsicsFixer, clangCmdArr)

      clangCmdArr.append(ppTmpFile)
      clangCmdArr.append("--")
      clangCmdArr.extend(warningArgs)
      clangCmd = " ".join(clangCmdArr)
      if verbose: sys.stderr.write("%s\n" % clangCmd)
      rc = os.system(clangCmd)
      if not rc == 0:
        if delTempFiles:
          os.system("rm -f %s" % ppTmpFile)
          os.system("rm -f %s" % srcRepl)
          os.system("rm -f %s" % cxxInitSrcFile)
        return rc

      #the source to source generates temp .cc files
      #we need the compile command to generate .o files from the temp .cc files
      #update the command to point to them
      cmdArr = [
        compiler, 
        extraCppFlagsStr, 
        sstCppFlagsStr, 
        sstCompilerFlagsStr, 
        givenFlagsStr
      ]
      target = objTarget
      if not objTarget:
        srcName = os.path.split(srcFile)[-1]
        target = swapSuffix("o", srcName)
      cmdArr.append("-o")
      cmdArr.append(target)
      cmdArr.append("-c")
      cmdArr.append(srcRepl)
      cmdArr.append("--no-integrated-cpp")
      cxxCmd = " ".join(cmdArr)
      if verbose: sys.stderr.write("%s\n" % cxxCmd)
      rc = os.system(cxxCmd)
      if not rc == 0:
        if delTempFiles:
          os.system("rm -f %s" % ppTmpFile)
          os.system("rm -f %s" % srcRepl)
          os.system("rm -f %s" % target)
          os.system("rm -f %s" % cxxInitSrcFile)
        return rc

      #now we generate the .o file containing the CXX linkage 
      #for global variable CXX init - because C is stupid
      cxxInitObjFile = addPrefix("sstGlobals.",target)
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
      cxxInitCompileCmd = " ".join(cxxInitCmdArr)
      if verbose: sys.stderr.write("%s\n" % cxxInitCompileCmd)
      rc = os.system(cxxInitCompileCmd)
      if delTempFiles:
        os.system("rm -f %s %s %s" % (ppTmpFile, cxxInitSrcFile, srcRepl))
      if not rc == 0:
        return rc


    #some generate multiple .o files at once, I don't know why
    manyObjects = objTarget == None #no specific target specified
    allObjects = []
    for srcFile in sourceFiles:
      srcFileNoSuffix = ".".join(srcFile.split(".")[:-1])
      cxxInitObjFile = addPrefix("sstGlobals.", swapSuffix("o",srcFile))
      if exeFromSrc:
        if objTarget:
          allObjects.append(objTarget)
        else:
          allObjects.append(swapSuffix("o", srcFile))
        allObjects.append(cxxInitObjFile)

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




