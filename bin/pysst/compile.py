import os


def getStatusOutput(args):
    from subprocess import check_output, CalledProcessError
    try:
      output = check_output(args)
      return 0, output
    except CalledProcessError as e:
      return e.returncode, e.output
      
def compileFull(compiler, cmdline_args):
    newargs = map(lambda x: "'%s'" % x, cmdline_args)
    cmd = "%s %s" % (compiler, " ".join(newargs))
    print cmd
    rc = os.system(cmd)
    return rc

def compilePath(compiler, cmdline_args, src, dotO):
    compcmd = "%s %s -o %s -c %s" % (compiler, " ".join(cmdline_args), dotO, src)
    newargs = map(lambda x: "'%s'" % x, cmdline_args)
    print compcmd
    rc = os.system(compcmd)
    return rc

def preprocess(compiler, cmdline_args, src):
    import commands
    import os
    cppargs = [compiler]
    cppargs.extend(cmdline_args)
    cppargs.append("-E")
    cppargs.append(src)
    #cppcmd = "%s %s %s %s" % (compiler, extraCPPFLAGS(), " ".join(cppargs), src)
    print " ".join(cppargs) 
    rc, stdout = getStatusOutput(cppargs)
    return rc, stdout

def compileRefactored(compiler, cmdline_args, src, dotO):
    from fakepath import tmpPath, refactoredPath
    import os
    tmpfile = tmpPath(src, dotO)
    dumpfile = refactoredPath(src, dotO)
    compcmd = "%s %s -o %s -c %s" % (compiler, " ".join(cmdline_args), dotO, dumpfile)
    print compcmd
    rc = os.system(compcmd)
    return rc

