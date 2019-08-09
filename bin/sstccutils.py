
def swapSuffix(suffix, path):
  splitter = path.split(".")[:-1]
  splitter.append(suffix)
  return ".".join(splitter)

def rebaseFolder(path, srcDir, dstDir):
  folder, fname = os.path.split(path)
  newBaseFolder = folder.replace(srcDir,dstDir)
  return os.path.join(newBaseFolder, fname)


def addPrefix(prefix, path):
  import os
  splitPath = os.path.split(path)
  if len(splitPath) == 2:
    return os.path.join(splitPath[0], prefix + splitPath[1])
  else:
    return prefix + path

def addPrefixAndRebase(prefix, path, newBase):
  import os
  newPath = addPrefix(prefix, path)
  folder, name = os.path.split(newPath)
  return os.path.join(newBase, name)

def delete(files):
  import traceback
  import os
  os.system("rm -f %s" % (" ".join(files)))

def getProcTreeHelper(mypid, arr):
  mypid = str(mypid)
  import commands
  info = commands.getoutput("ps axo pid,ppid,comm")
  for line in info.splitlines():
    args = line.strip().split()
    if args[0] == mypid:
      arr.append(" ".join(args[2:]))
      getProcTreeHelper(args[1], arr)
      break
   

def getProcTree():
  import os
  arr = []
  getProcTreeHelper(os.getpid(), arr)
  return arr

def getProcName():
  import os
  import commands
  import sys
  pid = int(os.getppid())
  runCmds = commands.getoutput("ps -p %d" % pid).splitlines()[-1].split()
  runCmds = runCmds[3:]
  firstCmd = runCmds[0].lstrip("-")
  if firstCmd in ("/bin/sh", "sh", "bash", "/bin/bash", "tcsh", "/bin/tcsh", "zsh", "/bin/zsh"):
    if len(runCmds) > 1: #it might just be bash
      firstCmd = runCmds[1]
  cmd = os.path.split(firstCmd)[-1]
  return cmd

def cleanFlag(flag):
  from sstccvars import includeDir, execPrefix, prefix 
  return flag.replace("${includedir}", includeDir).replace("${exec_prefix}", execPrefix).replace("${prefix}",prefix)
