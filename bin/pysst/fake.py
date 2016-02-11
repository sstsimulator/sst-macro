import sys
import re
import os
import pickle

def run(compiler):
    from collect import collectAllVars
    from compile import compileRefactored, compileFull, compilePath
    from refactor import refactor, refactorCompile
    import os

    pwd = os.getcwd()


    newargs = []
    args = sys.argv[1:]
    length = len(args)
    idx = 0
    dotOs = []
    srcs = []
    asmBuild = False

    while idx < length:
        arg = args[idx]
        if arg == "-o":
            idx += 1
            dotOs.append(args[idx])
        elif arg == "-c":
            pass
        elif os.path.isfile(arg):
            if arg.endswith(".cc") or arg.endswith(".c") or arg.endswith(".cpp"):
                srcs.append(args[idx])
            elif arg.endswith(".S"):
                asmBuild = True
            else:
                newargs.append("'%s'" % arg)
        else:
            newargs.append("'%s'" % arg)
        idx += 1

    isCompileMode = False
    if os.environ.has_key("SSTMAC_FAKE_BUILD"):
      isCompileMode = bool(os.environ["SSTMAC_FAKE_BUILD"])

    notObjectBuild = (not "-c" in args) or "-E" in args
    if isCompileMode or notObjectBuild or asmBuild: #link or preprocess
        return compileFull(compiler, args)

    variables = []
    if not os.environ.has_key("SSTMAC_GLOBALS_FILE"):
        raise Exception("Must set SSTMAC_GLOBALS_FILE environment variable")

    configFile = os.environ["SSTMAC_GLOBALS_FILE"]
    if not os.path.isfile:
        raise Exception("Config file %s is not a valid path" % configFile)

    text = open(configFile).read()

    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue

        variables.append(line)

    if not srcs:
        sys.exit("No source file given")

    if not dotOs:
        for src in srcs:
            relsrc = os.path.split(src)[-1]
            dotO=".".join(relsrc.split(".")[:-1]) + ".o"
            dotOs.append(dotO)

    for i in range(len(srcs)):
        src = srcs[i]
        dotO = dotOs[i]
        if not dotO.startswith("/"): #not an abs path
            dotO = os.path.join(pwd, dotO)

        rc = refactor(compiler, newargs, src, dotO, variables)

        if not rc == 0:
            return rc

    return 0

