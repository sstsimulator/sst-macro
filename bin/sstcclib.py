import os
import sys

sstmac_libs = [
'-lsprockit',
'-lundumpi',
'-lsstmac',
]

from sstccvars import sstmac_default_ldflags, sstmac_extra_ldflags, sstmac_cppflags
from sstccvars import prefix, exec_prefix, includedir, cc, cxx, cxxflags, cflags
from sstccvars import includedir
from sstccvars import sst_core


sstmac_ldflags = []
sstmac_ldflags.extend(sstmac_default_ldflags)
sstmac_ldflags.extend(sstmac_libs)

cppflags=" ".join(sstmac_cppflags)
ldflags=" ".join(sstmac_ldflags)

def clean_flags(flags):
    return flags.replace("${includedir}", includedir).replace("${exec_prefix}", exec_prefix).replace("${prefix}",prefix)

cppflags = clean_flags(cppflags)
ldflags = clean_flags(ldflags)
ld = cc 
repldir = os.path.join(includedir, "sstmac", "replacements")
repldir = clean_flags(repldir)

import sys
argify = lambda x: "'%s'" % x
sysargs = sys.argv[1:]
args = " ".join(map(argify,sysargs))

so_sysargs = sysargs[:]
exe_target = None
for idx in range(len(so_sysargs)):
  entry = so_sysargs[idx]
  if entry == "-o":
    exe_target = so_sysargs[idx+1]
    newTarget = "lib%s.so" % exe_target
    so_sysargs[idx+1] = newTarget
    break
so_args = " ".join(map(argify, so_sysargs))


src_files = False
obj_files = False
asm_files = False
verbose = False
for arg in args.split():
    sarg = arg.strip().strip("'")
    if sarg.endswith('.o'):
      obj_files = True
    elif sarg.endswith('.cpp') or sarg.endswith('.cc') or sarg.endswith('.c'):
      src_files = True
    elif sarg.endswith('.S'):
      asm_files = True
    elif sarg == "--verbose":
      verbose = True

if os.environ.has_key("SSTMAC_VERBOSE"):
    flag = int(os.environ["SSTMAC_VERBOSE"])
    verbose = verbose or flag

def run(typ, extralibs="", include_main=True, make_library=False, redefine_symbols=True):
    compiler_flags = ""
    compiler = ""
    cmd = ""
    if include_main:
      extralibs += " -lsstmac_main"
    global ldflags
    #always c++ no matter what for now
    if 1: #typ.lower() == "c++":
        compiler_flags = clean_flags(cxxflags)
        ldflags = "%s %s" % (compiler_flags, ldflags)
        compiler = cxx
        ld = cxx
    elif typ.lower() == "c":
        compiler_flags = clean_flags(cflags)
        compiler = cc
        ld = cxx #always use c++ for linking since we are bringing a bunch of sstmac C++ into the game

    extra_cppflags = []
    if redefine_symbols:
        extra_cppflags = [
        "-I%s/include/sumi" % prefix,
        "-DSSTMAC=1",
        "-D__thread=dontallow",
      ]

    if asm_files:
        extra_cppflags = "" #add nothing
    else:
        extra_cppflags = " ".join(extra_cppflags)

    if "--no-integrated-cpp" in sysargs:
        extra_cppflags = "" #add nothing

    ldpath_maker = "-Wl,-rpath,%s/lib" % prefix

    if redefine_symbols: extra_cppflags = "-I%s " % repldir + extra_cppflags
    
    cmd_arr = []
    if '-c' in sysargs or '-E' in sysargs: #compiler
        cmd_arr = [
          compiler, 
          extra_cppflags, 
          cppflags, 
          compiler_flags, 
          args
        ]
    elif obj_files and src_files:
        cmd_arr = [
          compiler, 
          extra_cppflags, 
          cppflags, 
          compiler_flags, 
          args, 
          ldflags, 
          extralibs, 
          ldpath_maker
        ]
    elif obj_files:
        global verbose
        global exe_target
        if sst_core:
          verbose = True
          make_library = True
          ldflags=ldflags.replace("-lsstmac","")

        if make_library:
          if sst_core:
            exe_target="lib%s.so" % exe_target
          os.system("touch %s" % exe_target)
          os.system("chmod +x %s" % exe_target)
          cmd_arr = [
            ld,
            "-shared",
            so_args, 
            ldflags,
            ldpath_maker
          ]
        else: #executable
          cmd_arr = [
            ld,
            args, 
            extralibs,
            ldflags, 
            extralibs, 
            ldpath_maker
          ]
    else: #all in one
        cmd_arr = [
          compiler, 
          args, 
          extra_cppflags, 
          cppflags, 
          compiler_flags, 
          ldflags, 
          extralibs, 
          ldpath_maker
        ]

    cmd = " ".join(cmd_arr)

    if verbose:
        sys.stderr.write("%s\n" % cmd)
    rc = os.system(cmd)
    return rc

