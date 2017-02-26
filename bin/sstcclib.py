import os
import sys
import commands

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

def clean_flag(flag):
    return flag.replace("${includedir}", includedir).replace("${exec_prefix}", exec_prefix).replace("${prefix}",prefix)

clangCppArgs = [
  clean_flag("-I${includedir}/sstmac/clang_replacements"),
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
  new_cppflags.append(clean_flag(entry))
sstmac_cppflags = new_cppflags

new_ldflags = []
for entry in sstmac_ldflags:
  new_ldflags.append(clean_flag(entry))
sstmac_ldflags = new_ldflags

cppflags = " ".join(sstmac_cppflags)
ldflags =  " ".join(sstmac_ldflags)
ld = cc 
repldir = os.path.join(includedir, "sstmac", "replacements")
repldir = clean_flag(repldir)

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
exe_target = None
for idx in range(len(so_sysargs)):
  entry = so_sysargs[idx]
  if entry == "-o":
    exe_target = so_sysargs[idx+1]
    break
so_args = " ".join(map(argify, so_sysargs))
so_args += " " + so_flags


srcFiles = False
asmFiles = False
verbose = False
clangArgs = []
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
    elif sarg.startswith("-I"):
      clangArgs.append(sarg)

if sourceFiles and len(objectFiles) > 1:
  sys.exit("Specified multiple object files for source compilation: %" % " ".join(objectFiles))

if os.environ.has_key("SSTMAC_VERBOSE"):
  flag = int(os.environ["SSTMAC_VERBOSE"])
  verbose = verbose or flag

def run(typ, extralibs="", include_main=True, make_library=False, redefine_symbols=True):
    global ldflags
    import os
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

    compiler_flags = ""
    compiler = ""
    cxxCmd = ""
    if include_main:
      extralibs += " -lsstmac_main"
    #always c++ no matter what for now
    if typ.lower() == "c++":
        compiler_flags = clean_flag(cxxflags)
        ldflags = "%s %s" % (compiler_flags, ldflags)
        compiler = cxx
        ld = cxx
    elif typ.lower() == "c":
        compiler_flags = clean_flag(cflags)
        compiler = cc
        ld = cxx #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game
    ldflags = "%s %s" % (compiler_flags, ldflags)

    extra_cppflags = []
    if redefine_symbols:
        extra_cppflags = [
        "-I%s/include/sumi" % prefix,
        "-DSSTMAC=1",
        "-D__thread=dontallow",
      ]

    if asmFiles:
        extra_cppflags = "" #add nothing
    else:
        extra_cppflags = " ".join(extra_cppflags)

    if "--no-integrated-cpp" in sysargs:
        extra_cppflags = "" #add nothing

    ldpath_maker = "-Wl,-rpath,%s/lib" % prefix

    if redefine_symbols: extra_cppflags = "-I%s " % repldir + extra_cppflags
    
    cxxCmdArr = []
    cppOnly = "-E" in sysargs
    runClang = False
    if '-c' in sysargs or cppOnly:
      runClang = not cppOnly
      cxxCmdArr = [
        compiler, 
        extra_cppflags, 
        cppflags, 
        compiler_flags, 
        args
      ]
    elif objTarget and srcFiles:
      runClang = True
      cxxCmdArr = [
        compiler, 
        extra_cppflags, 
        cppflags, 
        compiler_flags, 
        args, 
        ldflags, 
        extralibs, 
        ldpath_maker
      ]
    elif objTarget:
        global verbose
        global exe_target
        make_library = False
        if exe_target.endswith("dylib") or exe_target.endswith("so") or exe_target.endswith(".a"):
          make_library = True

        if sst_core:
          if not make_library:
            sys.exit("SST core requires all external elements to be .so files\n")
          verbose = True

        if make_library:
          if sst_core:
            ldflags=ldflags.replace("-lsstmac","")
          cxxCmdArr = [
            ld,
            so_args, 
            ldflags,
            ldpath_maker
          ]
        else: #executable
          cxxCmdArr = [
            ld,
            args, 
            extralibs,
            ldflags, 
            extralibs, 
            ldpath_maker
          ]
    else: #all in one
        cxxCmdArr = [
          compiler, 
          args, 
          extra_cppflags, 
          cppflags, 
          compiler_flags, 
          ldflags, 
          extralibs, 
          ldpath_maker
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
        clangCmdArr = [clangDeglobal]
        addClangArgs(clangArgs, clangCmdArr)
        addClangArgs(clangCppArgs, clangCmdArr)
        if typ == "c++":
          addClangArgs(clangCxxArgs, clangCmdArr)
        clangCmdArr.append(srcFile)
        clangCmdArr.append("--")
        clangCmd = " ".join(clangCmdArr)
        if verbose: sys.stderr.write("%s\n" % clangCmd)
        rc = os.system(clangCmd)
        if not rc == 0:
          return rc
        
        #the source to source generates temp .cc files
        #we need the compile command to generate .o files from the temp .cc files
        #update the command to point to them
        srcRepl = "sst." + srcFile
        objRepl = "sst." + objTarget
        cxxCmd = cxxCmd.replace(srcFile,srcRepl).replace(objTarget,objRepl)
        if verbose: sys.stderr.write("%s\n" % cxxCmd)
        rc = os.system(cxxCmd)
        if not rc == 0:
          return rc

        #now we generate the .o file containing the CXX linkage 
        #for global variable CXX init - because C is stupid
        cxxInitSrcFile = "sstGlobals." + srcFile + ".cpp"
        cxxInitObjFile = "sstGlobals." + srcFile + ".o"
        cxxInitCompileCmd = "%s -o %s -I%s/include -c %s" % (compiler, cxxInitObjFile, prefix, cxxInitSrcFile)
        if verbose: sys.stderr.write("%s\n" % cxxInitCompileCmd)
        rc = os.system(cxxInitCompileCmd)
        if not rc == 0:
          return rc


      #run the regular compile command to generate the .o file (or files)
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
          cxxMergeCmd = "%s -Wl,-r %s %s -o %s" % (compiler, srcTformObjFile, cxxInitObjFile, srcObjTarget)
          if verbose: sys.stderr.write("%s\n" % cxxMergeCmd)
          rc, output = commands.getstatusoutput(cxxMergeCmd)
          if not rc == 0:
            return rc
        else:
          mergeCmdArr.append("%s %s" % (srcTformObjFile, cxxInitObjFile))
      if not manyObjects:
        mergeCmdArr.append("-o %s" % objTarget)
        mergeCmd = " ".join(mergeCmdArr)
        if verbose: sys.stderr.write("%s\n" % mergeCmd)
        rc, output = commands.getstatusoutput(mergeCmd)
        if not rc == 0:
          sys.stderr.write("deglobal merge error on %s:\n%s\n" % (objTarget, output))
      return rc
          
    else:
      #standard compiler wrapper
      if verbose:
          sys.stderr.write("%s\n" % cxxCmd)
      rc = os.system(cxxCmd)
      return rc

