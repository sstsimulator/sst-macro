
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
  ctx.libs.append('-lsstmac')
  ctx.libs.append('-lsprockit')
  ctx.libs.append('-lundumpi')
