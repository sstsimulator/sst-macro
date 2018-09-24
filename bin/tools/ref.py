#! /usr/bin/env python

import os

files = """
"""

def check_files(fxn):
  configStatus = ""
  try:
    configStatus = open("../config.status").read()
  except:
    sys.exit("could not find valid config.status file")

  import re
  match = re.compile("srcdir=(.*)").search(configStatus)
  if not match:
    sys.exit("could not located srcdir in config.status")

  srcdir = match.groups()[0].strip().strip('"').strip("'")
  refdir = os.path.join(srcdir, "tests", "reference")

  for f in files.strip().splitlines():
    path = f.strip().split()[0]
    fname = os.path.split(path)[-1].replace("chk","tmp")
    ref = os.path.join(refdir, fname.replace("tmp","ref"))
    fxn(fname, ref)



