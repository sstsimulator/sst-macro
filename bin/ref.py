#! /usr/bin/env python

import os

files = """
"""

def check_files(fxn):
  for f in files.strip().splitlines():
    path = f.strip().split()[0]
    fname = os.path.split(path)[-1].replace("chk","tmp")
    fxn(fname)



