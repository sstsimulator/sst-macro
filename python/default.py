from sst.macro import *
import sys
import os
import sst


isSoFile = True
idx = 1
while isSoFile:
  next = sys.argv[idx]
  if next.endswith(".so"):
    if not os.path.isfile(next):
      sys.exit("Invalid library specified %s" % next)
    folder, lib = os.path.split(next)
    if not folder: folder = os.getcwd()
    old = os.environ["SST_LIB_PATH"] 
    os.environ["SST_LIB_PATH"] = old + ":" + folder
    sys.path.append(folder)
    loadLibrary(next)
    del sys.argv[idx]
  else: isSoFile = False
  idx += 1

setupDeprecated()

