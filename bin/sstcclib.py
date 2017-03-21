import os
import sys
import commands

helpText = """The following environmental variables can be defined for the SST compiler
SSTMAC_VERBOSE=0 or 1:        produce verbose output from the SST compiler (default 0)
SSTMAC_DELETE_TEMPS=0 or 1:   remove all temp source-to-source files (default 1)
SSTMAC_REMOVE_GLOBALS=0 or 1: run a source-to-source pass converting globals to TLS (default 1)
"""

sstmac_libs = [
'-lsprockit',
'-lundumpi',
'-lsstmac',
]


from sstccvars import sstmac_default_ldflags, sstmac_extra_ldflags, sstmac_cppflags
from sstccvars import prefix, exec_prefix, includedir, cc, cxx, cxxflags, cflags
from sstccvars import includedir
from sstccvars import sst_core
from sstccvars import so_flags
from sstccvars import clang_cppflags, clang_ldflags, clang_libtooling_cxxflags, clang_libtooling_cflags

def cleanFlag(flag):
    return flag.replace("${includedir}", includedir).replace("${exec_prefix}", exec_prefix).replace("${prefix}",prefix)

clangCppArgs = [
  cleanFlag("-I${includedir}/sstmac/clang_replacements"),
]
clangCxxArgs = [
  "-std=c++1y",
  "-stdlib=libc++", 
]
clangCxxArgs.extend(clang_libtooling_cxxflags.strip().split())

def addClangArg(a, ret):
  ret.append("--extra-arg=%s" % a)

def addClangArgs(argList, ret):
  for a in argList:
    addClangArg(a,ret)
  return ret

haveClangSrcToSrc = bool(clang_cppflags)
clangDeglobal = None
if haveClangSrcToSrc:
  clangDeglobal = os.path.join(prefix, "bin", "sstmac_clang_deglobal")

sstmac_ldflags = []
sstmac_ldflags.extend(sstmac_default_ldflags)
sstmac_ldflags.extend(sstmac_libs)

ldflags=" ".join(sstmac_ldflags)


new_cppflags = []
for entry in sstmac_cppflags:
  new_cppflags.append(cleanFlag(entry))
sstmac_cppflags = new_cppflags

new_ldflags = []
for entry in sstmac_ldflags:
  new_ldflags.append(cleanFlag(entry))
sstmac_ldflags = new_ldflags

sstCppFlagsStr=" ".join(sstmac_cppflags)
ldflags =  " ".join(sstmac_ldflags)
ld = cc 
repldir = os.path.join(includedir, "sstmac", "replacements")
repldir = cleanFlag(repldir)

import sys
def argify(x):
  if ' ' in x: 
    return "'%s'" % x
  else:
    return x

sysargs = sys.argv[1:]


so_sysargs = sysargs[:]
exeTarget = None
for idx in range(len(so_sysargs)):
  entry = so_sysargs[idx]
  if entry == "-o":
    exeTarget = so_sysargs[idx+1]
    break
so_args = " ".join(map(argify, so_sysargs))
so_args += " " + so_flags


srcFiles = False
asmFiles = False
verbose = False
delTempFiles = True
givenCppFlags = []
controlArgs = []
sourceFiles = []
objectFiles = []
objTarget = None
getObjTarget = False
for arg in sysargs:
  sarg = arg.strip().strip("'")
  if sarg.endswith('.o'):
    objectFiles.append(sarg)
    objTarget = sarg
    getObjTarget=False
  elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c'):
    srcFiles = True
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
    objTarget = sarg
    getObjTarget=False
  else:
    givenCppFlags.append(sarg)
if sst_core:
  givenCppFlags.append(" -DSSTMAC_EXTERNAL_SKELETON")

if sourceFiles and len(objectFiles) > 1:
  sys.exit("Specified multiple object files for source compilation: %" % " ".join(objectFiles))

if os.environ.has_key("SSTMAC_VERBOSE"):
  flag = int(os.environ["SSTMAC_VERBOSE"])
  verbose = verbose or flag
if os.environ.has_key("SSTMAC_DELETE_TEMPS"):
  flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
  delTempFiles = delTempFiles and flag
  

def run(typ, extralibs="", includeMain=True, makeLibrary=False, redefineSymbols=True):
    directIncludes = []
    global ldflags
    global sstCppFlagsStr
    import os
    
    if type == "c++":
      directIncludes.append("-include cstdint")
    else:
      directIncludes.append("-include stdint.h")

    remGlobals = True
    if os.environ.has_key("SSTMAC_REMOVE_GLOBALS"):
      remGlobals = int(os.environ["SSTMAC_REMOVE_GLOBALS"])

    if sys.argv[1] == "--version" or sys.argv[1] == "-V":
      import inspect, os
      print os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
      cmd = "%s %s" % (cxx, sys.argv[1])
      os.system(cmd)
      sys.exit()
    elif sys.argv[1] == "--flags":
      sys.stderr.write("LDFLAGS=%s\n" % ldflags)
      sys.stderr.write("CPPFLAGS=%s\n" % sstCppFlagsStr)
      sys.stderr.write("CXXFLAGS=%s\n" % cxxflags)
      sys.exit()
    elif sys.argv[1] == "--help":
      cmd = "%s --help" % (cxx)
      os.system(cmd)
      sys.stderr.write(helpText)
      sys.exit()

    compilerFlags = ""
    compiler = ""
    cxxCmd = ""
    if includeMain:
      extralibs += " -lsstmac_main"
    #always c++ no matter what for now
    if typ.lower() == "c++":
        compilerFlags = cleanFlag(cxxflags)
        ldflags = "%s %s" % (compilerFlags, ldflags)
        compiler = cxx
        ld = cxx
    elif typ.lower() == "c":
        compilerFlags = cleanFlag(cflags)
        compiler = cc
        ld = cxx #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game
    ldflags = "%s %s" % (compilerFlags, ldflags)

    directIncludesStr = " ".join(directIncludes)

    extraCppFlags = []
    if redefineSymbols:
        extraCppFlags = [
        "-I%s/include/sumi" % prefix,
        "-DSSTMAC=1",
        "-D__thread=dontallow",
      ]

    if asmFiles:
        extraCppFlags = [] #add nothing

    if "--no-integrated-cpp" in sysargs:
        extraCppFlags = [] #add nothing

    ldpathMaker = "-Wl,-rpath,%s/lib" % prefix

    if redefineSymbols: 
      extraCppFlags.insert(0,"-I%s" % repldir)

    
    cxxCmdArr = []
    ppCmdArr = []
    ppOnly = "-E" in controlArgs
    runClang = haveClangSrcToSrc and remGlobals
    controlArgStr = " ".join(controlArgs)
    extraCppFlagsStr = " ".join(extraCppFlags)
    givenCppFlagsStr = " ".join(givenCppFlags)
    srcFileStr = " ".join(sourceFiles)
    if '-c' in sysargs or ppOnly:
      runClang = runClang and (not ppOnly)
      if runClang:
        ppCmdArr = [
          compiler, 
          "-include sstmac/skeleton.h",
          directIncludesStr,
          extraCppFlagsStr, 
          givenCppFlagsStr,
          sstCppFlagsStr,
          compilerFlags, 
          "-E"
        ]
        #we run clang on a direct source file with no includes
        #only put cxxflags in the cmd arr for now
        cxxCmdArr = [
          compiler,
          compilerFlags
        ]
      else: 
        cxxCmdArr = [
          compiler, 
          extraCppFlagsStr, 
          givenCppFlagsStr,
          sstCppFlagsStr, 
          compilerFlags, 
          controlArgStr,
          srcFileStr
        ]
    elif objTarget and srcFiles:
      runClang = True
      cxxCmdArr = [
        compiler, 
        extraCppFlagsStr, 
        sstCppFlagsStr, 
        compilerFlags, 
        args, 
        ldflags, 
        extralibs, 
        ldpathMaker
      ]
    elif objTarget:
      global verbose
      global exeTarget
      runClang = False
      makeLibrary = False
      if exeTarget.endswith("dylib") or exeTarget.endswith("so") or exeTarget.endswith(".a"):
        makeLibrary = True

      if sst_core:
        if not makeLibrary:
          sys.exit("SST core requires all external elements to be .so files\n")
        verbose = True

      if makeLibrary:
        if sst_core:
          ldflags=ldflags.replace("-lsstmac","")
        cxxCmdArr = [
          ld,
          so_args, 
          ldflags,
          ldpathMaker
        ]
      else: #executable
        cxxCmdArr = [
          ld,
          extralibs,
          ldflags, 
          extralibs, 
          ldpathMaker
        ]
        cxxCmdArr.extend(sysargs)
    else: #all in one
      cxxCmdArr = [
        compiler, 
        args, 
        extraCppFlagsStr, 
        cppflags, 
        compilerFlags, 
        ldflags, 
        extralibs, 
        ldpathMaker
      ]

    cxxCmd = " ".join(cxxCmdArr)
    clangExtraArgs = []
    if runClang:
      #this is more complicated - we have to use clang to do a source to source transformation
      #then we need to run the compiler on that modified source
      for srcFile in sourceFiles:
        ppTmpFile = "pp." + srcFile
        cmdArr = ppCmdArr[:]
        cmdArr.append(srcFile)
        cmdArr.append("> %s" % ppTmpFile)
        ppCmd = " ".join(cmdArr) 
        if verbose: sys.stderr.write("%s\n" % ppCmd)
        os.system(ppCmd)

        srcRepl = "sst.pp." + srcFile
        cxxInitSrcFile = "sstGlobals.pp." + srcFile + ".cpp"

        clangCmdArr = [clangDeglobal]
        if typ == "c++":
          addClangArgs(clangCxxArgs, clangCmdArr)
          addClangArgs(clang_libtooling_cxxflags.split(), clangCmdArr)
        else:
          addClangArg(clang_libtooling_cflags, clangCmdArr)
        addClangArgs(givenCppFlags, clangCmdArr)
        addClangArgs(clangCppArgs, clangCmdArr)
        clangCmdArr.append(srcFile)
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
        cmdArr = cxxCmdArr[:]
        if objTarget:
          objRepl = "sst." + objTarget
          cmdArr.append("-o")
          cmdArr.append(objRepl)
        cmdArr.append("-c")
        cmdArr.append(srcRepl)
        cmdArr.append("--no-integrated-cpp")
        cxxCmd = " ".join(cmdArr)
        if verbose: sys.stderr.write("%s\n" % cxxCmd)
        rc = os.system(cxxCmd)
        if not rc == 0:
          if delTempFiles:
            os.system("rm -f %s" % ppTmpFile)
            os.system("rm -f %s" % objRepl)
            os.system("rm -f %s" % cxxInitSrcFile)
          return rc

        #now we generate the .o file containing the CXX linkage 
        #for global variable CXX init - because C is stupid
        cxxInitObjFile = "sstGlobals." + srcFile + ".o"
        cxxInitCompileCmd = "%s -o %s -I%s/include -c %s" % (cxx, cxxInitObjFile, prefix, cxxInitSrcFile)
        if verbose: sys.stderr.write("%s\n" % cxxInitCompileCmd)
        rc = os.system(cxxInitCompileCmd)
        if delTempFiles:
          os.system("rm -f %s" % ppTmpFile)
          os.system("rm -f %s" % cxxInitSrcFile)
        if not rc == 0:
          return rc


      #some idiots generate multiple .o files at once
      manyObjects = objTarget == None #no specific target specified
      mergeCmdArr = ["%s -Wl,-r" % compiler]
      for srcFile in sourceFiles:
        srcFileNoSuffix = ".".join(srcFile.split(".")[:-1])
        srcObjTarget = srcFileNoSuffix + ".o"
        srcTformObjFile = "sst." + srcObjTarget
        cxxInitObjFile = "sstGlobals." + srcFile + ".o"
        #now we have to merge the src-to-src generated .o with cxx linkage .o
        if manyObjects:
          #we need to generate a .o for each source file
          cxxMergeCmd = "%s -Wl,-r %s %s -o %s" % (cxx, srcTformObjFile, cxxInitObjFile, srcObjTarget)
          if verbose: sys.stderr.write("%s\n" % cxxMergeCmd)
          rc, output = commands.getstatusoutput(cxxMergeCmd)
          if delTempFiles:
            os.system("rm -f %s %s %s" % (srcTformObjFile, cxxInitObjFile, srcRepl))
          if not rc == 0:
            return rc
        else:
          mergeCmdArr.append("%s %s" % (srcTformObjFile, cxxInitObjFile))
      if not manyObjects:
        mergeCmdArr.append("-o %s" % objTarget)
        mergeCmd = " ".join(mergeCmdArr)
        if verbose: sys.stderr.write("%s\n" % mergeCmd)
        rc, output = commands.getstatusoutput(mergeCmd)
        if delTempFiles:
          os.system("rm -f %s %s %s" % (srcTformObjFile, cxxInitObjFile, srcRepl))
        if not rc == 0:
          sys.stderr.write("deglobal merge error on %s:\n%s\n" % (objTarget, output))
      return rc
          
    else:
      #standard compiler wrapper
      if verbose:
          sys.stderr.write("%s\n" % cxxCmd)
      rc = os.system(cxxCmd)
      return rc

