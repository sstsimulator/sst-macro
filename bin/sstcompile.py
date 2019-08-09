

def addPreprocess(ctx, sourceFile, outputFile, args, cmds):
  ppArgs = [ctx.compiler] 
  for entry in ctx.directIncludes:
    ppArgs.append("-include")
    ppArgs.append(entry)
  ppArgs.extend(map(lambda x: "-D%s" % x, ctx.defines))
  ppArgs.extend(map(lambda x: "-I%s" % x, args.I)) 
  ppArgs.extend(ctx.cppFlags)
  ppArgs.extend(ctx.compilerFlags)
  ppArgs.append("-E")
  ppArgs.append(sourceFile)
  cmds.append([outputFile,ppArgs,[]]) #pipe file, no extra temps

def addEmitLlvm(ctx, sourceFile, outputFile, args, cmds):
  cmdArr = [
    ctx.compiler,
    "-emit-llvm",
    "-S",
    "--no-integrated-cpp",
    sourceFile,
    "-o",
    outputFile
  ]
  cmds.append([None,cmdArr,[outputFile]])

def addLlvmOptPass(ctx, llFile, llvmPass, args, cmds):
  from sstccvars import clangDir
  from sstccvars import prefix
  import os
  optTool = os.path.join(clangDir, "bin", "opt")
  llvmLib = os.path.join(prefix, "lib", "lib%s.so" % llvmPass)
  cmdArr = [
    optTool,
    "-S",
    "-load",
    llvmLib,
    llFile,
    "-sst-%s" % llvmPass,
    "-o",
    llFile,
  ]
  cmds.append([None,cmdArr,[]])

def addLlvmCompile(ctx, llFile, objFile, args, cmds):
  cmdArr = [
    ctx.compiler,
    "-o",
    objFile,
    "-c",
    llFile,
  ]
  cmdArr.extend(ctx.compilerFlags)
  cmds.append([None,cmdArr,[objFile]])

def addSrc2SrcCompile(ctx, sourceFile, outputFile, args, cmds):
  from sstccvars import prefix
  from sstccvars import defaultIncludePaths, includeDir
  from sstccutils import cleanFlag, swapSuffix, addPrefixAndRebase, addPrefix
  from sstccvars import clangCppFlagsStr, clangLdFlagsStr
  from sstccvars import clangLibtoolingCxxFlagsStr, clangLibtoolingCFlagsStr
  from sstccvars import haveFloat128
  import os

  #First we must pre-process the file to get it read for source-to-source
  objBaseFolder, objName = os.path.split(outputFile)
  ppTmpFile = addPrefixAndRebase("pp.", sourceFile, objBaseFolder)
  addPreprocess(ctx, sourceFile, ppTmpFile, args, cmds)

  rawPaths = defaultIncludePaths.split(":")
  cleanPaths = []
  for path in rawPaths:
    cleanPaths.append(os.path.abspath(path))
  defaultIncludePaths = ":".join(cleanPaths)
  defaultIncludePaths += ":" + cleanFlag(includeDir)

  clangDeglobal = os.path.join(prefix, "bin", "sstmac_clang")
  clangCmdArr = [clangDeglobal]
  clangCmdArr.append(ppTmpFile)
  clangCmdArr.extend(ctx.clangArgs)
  clangCmdArr.append("--system-includes=%s" % defaultIncludePaths)
  clangCmdArr.append("--")
  #all of the compiler options go after the -- separator
  #fix intrinsics which might not be known to clang if using a different compiler
  intrinsicsFixerPath = os.path.join(cleanFlag(includeDir), "sstmac", "replacements", "fixIntrinsics.h")
  clangCmdArr.append("-include")
  clangCmdArr.append(intrinsicsFixerPath)
  clangCmdArr.append("-stdlib=libc++")
  if args.std:
    clangCmdArr.append("-std=%s" % args.std)
  elif ctx.typ == "c++": #make sure we have something for C++
    clangCmdArr.append("-std=c++1y")
  if not haveFloat128:
    clangCmdArr.append("-D__float128=clangFloat128Fix")
  if ctx.typ == "c++":
    clangCmdArr.extend(clangLibtoolingCxxFlagsStr.split())
  else:
    clangCmdArr.extend(clangLibtoolingCFlagsStr.split())
  clangCmdArr.extend(map(lambda x: "-W%s" % x, args.W))

  srcRepl = addPrefixAndRebase("sst.pp.",sourceFile,objBaseFolder)
  cxxInitSrcFile = addPrefixAndRebase("sstGlobals.pp.",sourceFile,objBaseFolder) + ".cpp"
  cmds.append([None,clangCmdArr,[srcRepl,cxxInitSrcFile]]) #None -> don't pipe output anywhere

  tmpTarget = addPrefix("tmp.", outputFile)
  llvmPasses = []
  if args.skeletonize:
    llvmPasses = args.skeletonize.split(",")

  if llvmPasses:
    llvmFile = swapSuffix("ll", tmpTarget)
    addEmitLlvm(ctx, srcRepl, llvmFile, args, cmds)
    for llvmPass in llvmPasses:
      addLlvmOptPass(ctx, llvmFile, llvmPass, args, cmds)
    addLlvmCompile(ctx, llvmFile, tmpTarget, args, cmds)
    
  else:
    cmdArr = [ctx.compiler]
    cmdArr.extend(ctx.compilerFlags)
    cmdArr.append("--no-integrated-cpp")
    cmdArr.append("-o")
    cmdArr.append(tmpTarget)
    cmdArr.append("-c")
    cmdArr.append(srcRepl)
    cmds.append([None,cmdArr,[tmpTarget]])

  cxxInitObjFile = addPrefix("sstGlobals.", outputFile)
  cxxInitCmdArr = [
    ctx.cxx,
    "-o",
    cxxInitObjFile,
    "-I%s/include" % prefix,
    "-c",
    cxxInitSrcFile
  ]
  cxxInitCmdArr.extend(ctx.cxxFlags)
  cxxInitCmdArr.extend(ctx.cppFlags)
  cmds.append([None,cxxInitCmdArr,[cxxInitObjFile]])

  mergeCmdArr = [
    "ld", "-r"
  ]

  import platform
  if not platform.system() == "Darwin":
    mergeCmdArr.append("--unique")

  mergeCmdArr.append(tmpTarget)
  mergeCmdArr.append(cxxInitObjFile)
  mergeCmdArr.append("-o")
  mergeCmdArr.append(outputFile)

  cmds.append([None,mergeCmdArr,[]])

def addComponentCompile(ctx, sourceFile, outputFile, args, cmds):
  buildArgs = [ctx.compiler] 
  for entry in ctx.directIncludes:
    buildArgs.append("-include")
    buildArgs.append(entry)
  buildArgs.extend(map(lambda x: "-D%s" % x, ctx.defines))
  buildArgs.extend(map(lambda x: "-I%s" % x, args.I)) 
  buildArgs.extend(ctx.cppFlags)
  buildArgs.extend(ctx.compilerFlags)
  buildArgs.append("-c")
  buildArgs.append(sourceFile)
  buildArgs.append("-o")
  buildArgs.append(outputFile)
  cmds.append([None,buildArgs,[]])

def addCompile(ctx, sourceFile, outputFile, args, cmds):
  pass

