
    



def picklePath(src):
    import os
    import sys
    scratchdir = os.getcwd()
    if os.environ.has_key("SSTMAC_SCRATCH"):
      scratchdir = os.environ["SSTMAC_SCRATCH"]
    root = ".".join(src.split(".")[:-1])
    newfile = root + ".output.pickle"
    newpath = os.path.join(scratchdir, newfile)
    newpath = newpath.replace(".libs/","")
    return newpath


# For a given source file,
# return the location of a temporary file
# holding preprocessed text or other text
def tmpPath(src, dotO):
    suffix = src.split(".")[-1]
    root = ".".join(dotO.split(".")[:-1])
    tmpfile = dotO + "_tmp.%s" % suffix
    return tmpfile

def dumpPath(src, dotO, prefix):
    import os
    scratchdir = os.getcwd()
    if os.environ.has_key("SSTMAC_SCRATCH"):
      scratchdir = os.environ["SSTMAC_SCRATCH"]
    folder, fname = os.path.split(dotO)
    suffix = src.split(".")[-1]
    root = ".".join(fname.split(".")[:-1])
    new_fname = prefix + root + "." + suffix
    dumpfile = os.path.join(folder, new_fname)
    dumpfile = dumpfile.replace(".libs/","")
    return dumpfile

# For a given temporary file,
# return the location of the file containing
# the fully refactored, preprocessed text
def refactoredPath(src, dotO):
  return dumpPath(src, dotO, "refactored_")

def preprocessedPath(src, dotO):
  return dumpPath(src, dotO, "preprocessed_")
