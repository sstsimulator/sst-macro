import inspect
import os
import os.path
import sys

class PathData:
    include_paths = []
    skip_files = [
        "sprockit/regexp",
        "msg_mdata",
        "circuit_switch",
        "-snapshot",
        "attic",
        "dist_event_map",
        "logicalprocess",
        "test_packet_train",
        "boost",
        "gui",
        "dumpistats",
        "scf_tascel",
        "flowrackswitch",
        "rackswitch.h", 
        "abstractnodeprocessor.h", 
        "omnetpp", 
        "gtest", 
        "sdma", 
        "testtopo", 
        "mpi/testmpi",
        "common/test/params",
        "mersenne",
        "tests/api",
        "traceanalysis",
        "binbase"
    ]

def append_include_path(my_sstmac_include_dir):
    PathData.include_paths.extend([
        my_sstmac_include_dir,
        os.path.join(my_sstmac_include_dir, "sstmac"), 
        os.path.join(my_sstmac_include_dir,"tools"), 
        os.path.join(my_sstmac_include_dir, "sstmac", "software", "libraries"),
	    os.path.join(my_sstmac_include_dir, "sstmac", "software", "api"),
        os.path.join(my_sstmac_include_dir, "sstmac", "software", "api", "openshmem")
    ])


def get_include_path(include_spec, current_dir, local_first = False):
    if local_first:
        fullpath = os.path.join(current_dir, include_spec)
        if os.path.isfile(fullpath):
            return fullpath

    for path in PathData.include_paths:
        fullpath = os.path.join(path, include_spec)
        if os.path.isfile(fullpath):
            return fullpath

    # loop through the current dir and all subdirs
    subdir = current_dir
    while subdir and not subdir == "/":
        fullpath = os.path.join(subdir, include_spec)
        if os.path.isfile(fullpath):
            return fullpath

        splitter = os.path.split(subdir)
        if len(splitter) > 1:
            subdir = splitter[0]
        else:
            subdir = None

    return None

def skip_file(fullpath):
    for entry in PathData.skip_files:
        if entry in fullpath:
            return True
    return False

