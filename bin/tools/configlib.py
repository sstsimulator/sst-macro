
import sys

def getoutput3(cmd,stdin=None,pipe=None):
  if not (sys.version_info < (3,0)):
    #Python 3 is an unmitigated disaster
    #Thanks for breaking everything, Guido
    from subprocess import check_output,STDOUT,Popen,PIPE
    if stdin:
      stdin = open(stdin)
    elif pipe:
      pipe = Popen(pipe, stdout=PIPE)
      stdin = pipe.stdout
    import io
    result = check_output(cmd.split(),stdin=stdin,stderr=STDOUT).decode("utf-8").rstrip("\n")
    if pipe:
      pipe.wait
    #Oh, and I love decoding byte strings manually
    return result

def getoutput2(cmd,stdin=None,pipe=None):
  if sys.version_info < (3,0):
    import commands
    if stdin:
      cmd = cmd + " < %s" % stdin
    elif pipe:
      str_arr = []
      for elem in pipe:
        if " " in elem: str_arr.append("'%s'" % elem)
        else: str_arr.append(elem)
      cmd = " ".join(str_arr) + " | " + cmd
    return commands.getoutput(cmd)

getoutput = None
if sys.version_info < (3,0):
  getoutput = getoutput2
else:
  getoutput = getoutput3
  
