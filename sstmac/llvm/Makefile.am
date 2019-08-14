#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2018 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

AM_CPPFLAGS += ${LLVM_CPPFLAGS}
AM_CXXFLAGS += -fno-rtti -fno-exceptions

#this contains instrumentation functions usable by LLVM
if HAVE_CLANG
noinst_LTLIBRARIES = libsstmac_inst.la
libsstmac_inst_la_SOURCES = \
  meminst.cc

lib_LTLIBRARIES = \
  libmemtrace.la

libmemtrace_la_SOURCES = \
  memtrace2.cc \
	memtraceTools.cc 

libmemtrace_la_LDFLAGS = \
 ${LLVM_LDFLAGS} \
 -module -shared \
 -version-info @SSTMAC_LIBVERSION@ \
 -Wl,-rpath,@CLANG_INSTALL_DIR@/lib 


endif
