
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

def run(typ, extraLibs="", includeMain=True, makeLibrary=False, redefineSymbols=True):
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
  optFlags = []
  objTarget = None
  ldTarget = None
  getObjTarget = False
  for arg in sysargs:
    sarg = arg.strip().strip("'")
    if sarg.endswith('.o'):
      objectFiles.append(sarg)
      if getObjTarget:
        objTarget = sarg
        getObjTarget=False
    elif sarg.startswith("-Wl"):
      linkerArgs.append(sarg)
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
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c') or sarg.endswith(".cxx"):
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

  exeFromSrc = sourceFiles and not objectFiles

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
    cmd = "%s %s" % (cxx, sys.argv[1])
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

  sstCompilerFlags = []
  for flag in sstCompilerFlagsStr.split():
    if not flag.startswith("-O") and not flag == "-g":
      sstCompilerFlags.append(flag)
  sstCompilerFlagsStr = " ".join(sstCompilerFlags)

  sstCxxFlagsStr = cleanFlag(sstCxxFlagsStr)
  sstCxxFlags = []
  for flag in sstCxxFlagsStr.split():
    if not flag.startswith("-O") and not flag == "-g":
      sstCxxFlags.append(flag)
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
  runClang = haveClangSrcToSrc and src2src 
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
    allCompilerFlags = sstCxxFlagsStr + sstCppFlagsStr + givenFlagsStr
    if not "fPIC" in allCompilerFlags:
      sys.stderr.write("Linker/dlopen will eventually fail on .so file: fPIC not in C/CXXFLAGS\n")
      return 1
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

      srcRepl = addPrefix("sst.pp.",srcFile)
      cxxInitSrcFile = addPrefix("sstGlobals.pp.",srcFile) + ".cpp"

      clangCmdArr = [clangDeglobal]
      if typ == "c++":
        addClangArgs(clangCxxArgs, clangCmdArr)
        addClangArgs(clangLibtoolingCxxFlagsStr.split(), clangCmdArr)
      else:
        addClangArg(clangLibtoolingCFlagsStr, clangCmdArr)
      clangCmdArr.append(ppTmpFile)
      clangCmdArr.append("--")
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
      srcTformObjFile = swapSuffix("o", srcRepl)
      if objTarget:
        srcTformObjFile = addPrefix("sst.", objTarget)
      cmdArr.append("-o")
      cmdArr.append(srcTformObjFile)
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
          os.system("rm -f %s" % srcTformObjFile)
          os.system("rm -f %s" % cxxInitSrcFile)
        return rc

      #now we generate the .o file containing the CXX linkage 
      #for global variable CXX init - because C is stupid
      cxxInitObjFile = addPrefix("sstGlobals.",srcFile) + ".o"
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
    mergeCmdArr = [compiler]
    mergeCmdArr.append("-Wl,-r -nostdlib")
    allObjects = []
    for srcFile in sourceFiles:
      target = objTarget
      srcFileNoSuffix = ".".join(srcFile.split(".")[:-1])
      srcObjTarget = srcFileNoSuffix + ".o"
      srcTformObjFile = swapSuffix("o", addPrefix("sst.pp.", srcFile))
      if objTarget:
        srcTformObjFile = addPrefix("sst.", objTarget)
      cxxInitObjFile = addPrefix("sstGlobals.", srcFile) + ".o"
      #now we have to merge the src-to-src generated .o with cxx linkage .o
      #we need to generate a .o for each source file
      cmdArr = mergeCmdArr[:]
      target = objTarget
      if manyObjects: 
        target = srcObjTarget
      cmdArr.append("-o")
      cmdArr.append(target)
      cmdArr.append(srcTformObjFile)
      cmdArr.append(cxxInitObjFile)
      cxxMergeCmd = " ".join(cmdArr)
      if verbose: sys.stderr.write("%s\n" % cxxMergeCmd)
      rc, output = getstatusoutput(cxxMergeCmd)
      if delTempFiles:
        os.system("rm -f %s %s" % (srcTformObjFile, cxxInitObjFile))
      if not rc == 0:
        delete(allObjects)
        sys.stderr.write("deglobal merge error on %s:\n%s\n" % (target, output))
        return rc
      if exeFromSrc:
        allObjects.append(target)

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

  else:
    rc = runCmdArr(cxxCmdArr,verbose)
    if not rc == 0: return rc
    rc = runCmdArr(ldCmdArr,verbose)
    if not rc == 0: return rc
    rc = runCmdArr(arCmdArr,verbose)
    return rc




