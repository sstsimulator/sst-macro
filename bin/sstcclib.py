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
  "-I/Users/jjwilke/Programs/install/clang-llvm/bin/../include/c++/v1",
  "-I/Users/jjwilke/Programs/install/clang-llvm/bin/../lib/clang/5.0.0/include",
]
clangCppArgs.extend(clang_libtooling_cflags.strip().split())
clangCxxArgs = [
  "-std=c++1y",
  "-stdlib=libc++", 
]
clangCxxArgs.extend(clang_libtooling_cxxflags.strip().split())
def addClangArgs(argList, ret):
  for a in argList:
    ret.append("--extra-arg=%s" % a)
  return ret

haveClangSrcToSrc = bool(clang_cppflags)
clangDeglobal = None
if haveClangSrcToSrc:
  clangDeglobal = os.path.join(prefix, "bin", "sstmac_clang_deglobal")

sstmac_ldflags = []
sstmac_ldflags.extend(sstmac_default_ldflags)
sstmac_ldflags.extend(sstmac_libs)

cppflags=" ".join(sstmac_cppflags)
ldflags=" ".join(sstmac_ldflags)


new_cppflags = []
for entry in sstmac_cppflags:
  new_cppflags.append(cleanFlag(entry))
sstmac_cppflags = new_cppflags

new_ldflags = []
for entry in sstmac_ldflags:
  new_ldflags.append(cleanFlag(entry))
sstmac_ldflags = new_ldflags

cppflags = " ".join(sstmac_cppflags)
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
args = " ".join(map(argify,sysargs))
if sst_core:
  args += " -DSSTMAC_EXTERNAL_SKELETON"

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
otherCppArgs = []
sourceFiles = []
objectFiles = []
objTarget = None
for arg in args.split():
    sarg = arg.strip().strip("'")
    if sarg.endswith('.o'):
      objectFiles.append(sarg)
      objTarget = sarg
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c'):
      srcFiles = True
      sourceFiles.append(sarg)
    elif sarg.endswith('.S'):
      asmFiles = True
    elif sarg == "--verbose":
      verbose = True
    elif sarg in ("-c","-o"):
      pass #do nothing
    else:
      otherCppArgs.append(sarg)

if sourceFiles and len(objectFiles) > 1:
  sys.exit("Specified multiple object files for source compilation: %" % " ".join(objectFiles))

if os.environ.has_key("SSTMAC_VERBOSE"):
  flag = int(os.environ["SSTMAC_VERBOSE"])
  verbose = verbose or flag
if os.environ.has_key("SSTMAC_DELETE_TEMPS"):
  flag = int(os.environ["SSTMAC_DELETE_TEMPS"])
  delTempFiles = delTempFiles and flag
  

def run(typ, extralibs="", includeMain=True, makeLibrary=False, redefineSymbols=True):
    global ldflags
    global cppflags
    import os
    
    if type == "c++":
      cppflags += " -include cstdint"
    else:
      cppflags += " -include stdint.h"

    if sys.argv[1] == "--version" or sys.argv[1] == "-V":
      import inspect, os
      print os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
      cmd = "%s %s" % (cxx, sys.argv[1])
      os.system(cmd)
      sys.exit()
    elif sys.argv[1] == "--flags":
      sys.stderr.write("LDFLAGS=%s\n" % ldflags)
      sys.stderr.write("CPPFLAGS=%s\n" % cppflags)
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

    extraCppFlags = []
    if redefineSymbols:
        extraCppFlags = [
        "-I%s/include/sumi" % prefix,
        "-DSSTMAC=1",
        "-D__thread=dontallow",
      ]

    if asmFiles:
        extraCppFlags = "" #add nothing
    else:
        extraCppFlags = " ".join(extraCppFlags)

    if "--no-integrated-cpp" in sysargs:
        extraCppFlags = "" #add nothing

    ldpathMaker = "-Wl,-rpath,%s/lib" % prefix

    if redefineSymbols: extraCppFlags = "-I%s " % repldir + extraCppFlags
    
    cxxCmdArr = []
    cppOnly = "-E" in sysargs
    runClang = False
    if '-c' in sysargs or cppOnly:
      runClang = not cppOnly
      cxxCmdArr = [
        compiler, 
        extraCppFlags, 
        cppflags, 
        compilerFlags, 
        args
      ]
    elif objTarget and srcFiles:
      runClang = True
      cxxCmdArr = [
        compiler, 
        extraCppFlags, 
        cppflags, 
        compilerFlags, 
        args, 
        ldflags, 
        extralibs, 
        ldpathMaker
      ]
    elif objTarget:
        global verbose
        global exeTarget
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
            args, 
            extralibs,
            ldflags, 
            extralibs, 
            ldpathMaker
          ]
    else: #all in one
        cxxCmdArr = [
          compiler, 
          args, 
          extraCppFlags, 
          cppflags, 
          compilerFlags, 
          ldflags, 
          extralibs, 
          ldpathMaker
        ]

    cxxCmd = " ".join(cxxCmdArr)
    runClang = runClang and haveClangSrcToSrc
    if os.environ.has_key("SSTMAC_REMOVE_GLOBALS"):
      runClang = runClang and int(os.environ["SSTMAC_REMOVE_GLOBALS"])
    clangExtraArgs = []
    if runClang:
      #this is more complicated - we have to use clang to do a source to source transformation
      #then we need to run the compiler on that modified source
      for srcFile in sourceFiles:
        srcRepl = "sst." + srcFile
        cxxInitSrcFile = "sstGlobals." + srcFile + ".cpp"

        clangCmdArr = [clangDeglobal]
        addClangArgs(otherCppArgs, clangCmdArr)
        addClangArgs(clangCppArgs, clangCmdArr)
        if typ == "c++":
          addClangArgs(clangCxxArgs, clangCmdArr)
        clangCmdArr.append(srcFile)
        clangCmdArr.append("--")
        clangCmd = " ".join(clangCmdArr)
        if verbose: sys.stderr.write("%s\n" % clangCmd)
        rc = os.system(clangCmd)
        if not rc == 0:
          if delTempFiles:
            os.system("rm -f %s" % srcRepl)
            os.system("rm -f %s" % cxxInitSrcFile)
          return rc
        
        #the source to source generates temp .cc files
        #we need the compile command to generate .o files from the temp .cc files
        #update the command to point to them
        objRepl = "sst." + objTarget
        cxxCmd = cxxCmd.replace(srcFile,srcRepl).replace(objTarget,objRepl)
        rc = os.system(cxxCmd)
        if not rc == 0:
          if delTempFiles:
            os.system("rm -f %s" % srcRepl)
          return rc

        #now we generate the .o file containing the CXX linkage 
        #for global variable CXX init - because C is stupid
        cxxInitObjFile = "sstGlobals." + srcFile + ".o"
        cxxInitCompileCmd = "%s -o %s -I%s/include -c %s" % (cxx, cxxInitObjFile, prefix, cxxInitSrcFile)
        if verbose: sys.stderr.write("%s\n" % cxxInitCompileCmd)
        rc = os.system(cxxInitCompileCmd)
        if delTempFiles:
          os.system("rm -f %s" % cxxInitSrcFile)
        if not rc == 0:
          return rc


      #run the regular compile command to generate the .o file (or files)
      if verbose: sys.stderr.write("%s\n" % cxxCmd)
      rc = os.system(cxxCmd)
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

