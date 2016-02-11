
def refactorCompile(compiler, cmdline_args, src, dotO):
    from compile import compileRefactored
    newargs = cmdline_args[:]
    newargs.append("--no-integrated-cpp")
    newargs.append("-w")
    rc = compileRefactored(compiler, newargs, src, dotO)
    return rc

class FxnArgs:
    def __init__(self):
        self.fulltext = ""
        self.depth0 = ""
        self.depthN = ""

    def replace(self, old, new):
        self.fulltext = self.fulltext.replace(old, new)
        self.depthN = self.depthN.replace(old, new)

def refactor(compiler, cmdline_args, src, dotO, variables):
    from fakepath import tmpPath, refactoredPath, picklePath, preprocessedPath, dumpPath
    from fakeparse import normalize, getGlobalText, clearAllPounds, getFunctionBodies
    from var import replVars, collectVars
    from compile import preprocess, compileRefactored
    from collect import collectAllVars
    import sys, os, pickle, re

    tmpfile = tmpPath(src, dotO)
    dumpfile = preprocessedPath(src, dotO)
    
    srctext = open(src).read()
    rc, fulltext = preprocess(compiler, cmdline_args, src)
    if not rc == 0:
        sys.stderr.write("preprocess of %s failed\n" % src)
        return rc

    open(dumpfile, "w").write(fulltext)
    subtext = fulltext



    #cleantext = normalize(subtext)
    cleantext = subtext
    depthN = getFunctionBodies(cleantext)
    depth0 = getGlobalText(cleantext)
    depth0 = clearAllPounds(depth0)
    already_done = {}
    fxnargs = FxnArgs()
    fxnargs.fulltext = subtext
    fxnargs.depth0 = depth0
    fxnargs.depthN = depthN

    dumpfile = dumpPath(src, dotO, "depth0_")
    open(dumpfile, "w").write(depth0)

    outvars = []
    #make a relative path
    fscope = dotO.replace(".libs/", "")
    #make C++ valid
    fscope = fscope.replace(".","_").replace("/","_")
    collectVars(fxnargs, variables, outvars, fscope)

    subtext = replVars(subtext, fxnargs, outvars)

    subtext = clearAllPounds(subtext)

    dumpfile = refactoredPath(src, dotO)
    fobj = open(dumpfile,"w")
    fobj.write(subtext)
    fobj.close()

    return refactorCompile(compiler, cmdline_args, src, dotO)
        

