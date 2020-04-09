"""
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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

def addLink(ctx, ldTarget, args, cmds, objects, toExe=False):
  #xargs includes the list of object files
  from sstccvars import prefix
  from sstccvars import soFlagsStr
  ldpathMaker = "-Wl,-rpath,%s/lib" % prefix
  linkCmdArr = [ctx.ld, ldpathMaker] 
  linkCmdArr.extend(ctx.ldFlags)
  if not toExe:
    linkCmdArr.extend(soFlagsStr.split())
  linkCmdArr.extend(ctx.libs)
  linkCmdArr.extend(ctx.compilerFlags)
  linkCmdArr.extend(map(lambda x: "-W%s" % x, args.Wl)) #add all the -Wl flags for now
  linkCmdArr.extend(map(lambda x: "-L%s" % x, args.L)) #add all the -L flags for now
  linkCmdArr.extend(map(lambda x: "-l%s" % x, args.l)) #add all the -l flags for now
  linkCmdArr.extend(objects)
  linkCmdArr.append("-o")
  linkCmdArr.append(ldTarget)
  cmds.append([None,linkCmdArr,[]])

def addModeLinks(ctx, args):
  if not ctx.sstCore:
    ctx.libs.append('-lsstmac')
  ctx.libs.append('-lsprockit')
  ctx.libs.append('-lundumpi')
