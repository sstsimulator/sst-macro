"""
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
"""

import sys

def getstatusoutput3(cmd,stdin=None,pipe=None):
  if not (sys.version_info < (3,0)):
    try:
      #Python 3 is an unmitigated disaster
      #Thanks for breaking everything, Guido
      from subprocess import check_output,STDOUT,Popen,PIPE
      if stdin:
        stdin = open(stdin)
      result = None
      child = None
      if pipe:
        p1 = Popen(pipe, stdout=PIPE)
        child = Popen(cmd.split(), stdin=p1.stdout, stdout=PIPE, stderr=STDOUT)
        p1.stdout.close()  # Allow p1 to receive a SIGPIPE if p2 exits.
        result, stderr = child.communicate()
      else:
        #result = check_output(cmd.split(),stdin=stdin,stderr=STDOUT)
        child = Popen(cmd.split(), stdin=stdin, stdout=PIPE, stderr=STDOUT)
        result, stderr = child.communicate()
        #Oh, and I love decoding byte strings manually
      if not result:
        return 1, ""
      else:
        return child.returncode, result.decode("utf-8").rstrip("\n")
    except Exception as e:
      sys.stderr.write("FAILED: %s" % cmd)
      if stdin:
        sys.stderr.write(" stdin=%s" % stdin)
      elif pipe:
        sys.stderr.write(" pipe=%s" % pipe)
      sys.stderr.write("\n")
      raise e

def getoutput3(cmd,stdin=None,pipe=None):
  rc, text = getstatusoutput3(cmd,stdin,pipe)
  return text

def get_cmd_from_pipe2(cmd,stdin,pipe):
  if stdin:
    cmd = cmd + " < %s" % stdin
  elif pipe:
    str_arr = []
    for elem in pipe:
      if " " in elem: str_arr.append("'%s'" % elem)
      else: str_arr.append(elem)
    cmd = " ".join(str_arr) + " | " + cmd
  return cmd

def getoutput2(cmd,stdin=None,pipe=None):
  if sys.version_info < (3,0):
    import commands
    newCmd = get_cmd_from_pipe2(cmd, stdin, pipe)
    return commands.getoutput(newCmd)

def getstatusoutput2(cmd,stdin=None,pipe=None):
  if sys.version_info < (3,0):
    import commands
    newCmd = get_cmd_from_pipe2(cmd, stdin, pipe)
    return commands.getstatusoutput(newCmd)

getoutput = None
getstatusoutput = None
if sys.version_info < (3,0):
  getoutput = getoutput2
  getstatusoutput = getstatusoutput2
else:
  getstatusoutput = getstatusoutput3
  getoutput = getoutput3
  
