
import sys

def getoutput3(cmd,stdin=None,pipe=None):
  if not (sys.version_info < (3,0)):
    try:
      #Python 3 is an unmitigated disaster
      #Thanks for breaking everything, Guido
      from subprocess import check_output,STDOUT,Popen,PIPE
      if stdin:
        stdin = open(stdin)
      result=None
      if pipe:
        p1 = Popen(pipe, stdout=PIPE)
        p2 = Popen(cmd.split(), stdin=p1.stdout, stdout=PIPE, stderr=STDOUT)
        p1.stdout.close()  # Allow p1 to receive a SIGPIPE if p2 exits.
        result = p2.communicate()[0]
      else:
        result = check_output(cmd.split(),stdin=stdin,stderr=STDOUT)
        #Oh, and I love decoding byte strings manually
      return result.decode("utf-8").rstrip("\n")
    except Exception as e:
      sys.stderr.write("FAILED: %s" % cmd)
      if stdin:
        sys.stderr.write(" stdin=%s" % stdin)
      elif pipe:
        sys.stderr.write(" pipe=%s" % pipe)
      sys.stderr.write("\n")
      raise e

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
  
