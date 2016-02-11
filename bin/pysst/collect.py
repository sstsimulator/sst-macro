import os

def replVars(compiler, cmdline_args, src, dotO, tmpfile, suffix, allvars, outvars):
    rc, fulltext = preprocess(compiler, cmdline_args, src)
    if not rc == 0:
        return rc

    fulltext = normalize(fulltext)
    depth0 = getGlobalText(fulltext)

def collectAllVars(compiler, cmdline_args, src, dotO, variables, outvars=[]):
    from var import collectVars
    from compile import preprocess
    from parse import normalize, getGlobalText, clearAllAttributes, clearAllPounds
    from path import picklePath
    import pickle
    import sys
    import os

    #make a relative path
    fscope = dotO.replace(".libs/", "")
    #make C++ valid
    fscope = fscope.replace(".","_").replace("/","_")

    rc, fulltext = preprocess(compiler, cmdline_args, src)
    if not rc == 0:
        return rc

    #fulltext = normalize(fulltext)

    depth0 = getGlobalText(fulltext)
    depth0 = clearAllPounds(depth0)
    depth0 = clearAllAttributes(depth0)

    fxnargs = FxnArgs()
    fxnargs.depthN = getFunctionBodies(fulltext)
    fxnargs.depth0 = depth0
    fxnargs.fulltext = fulltext

    collectVars(fxnargs, variables, outvars, fscope)

    return rc


