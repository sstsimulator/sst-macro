
import sys

def getstatusoutput3(cmd,stdin=None,pipe=None):
  if not (sys.version_info < (3,0)):
    try:
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
      return 0,result
    except:
      return 1, ""

def getstatusoutput2(cmd,stdin=None,pipe=None):
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
    return commands.getstatusoutput(cmd)

getstatusoutput = None
if sys.version_info < (3,0):
  getstatusoutput = getstatusoutput2
else:
  getstatusoutput = getstatusoutput3

def getoutput(cmd):
  rc, output = getstatusoutput(cmd)
  return output

  
