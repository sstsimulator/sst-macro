#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsstmac_pthread.la
library_includedir=$(includedir)/sstmac/libraries/pthread

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_pthread_la_LDFLAGS = 

libsstmac_pthread_la_SOURCES = \
  sstmac_pthread.cc \
  sstmac_pthread_runner.cc \
  sstmac_sched.cc 

nodist_library_include_HEADERS = \
  sstmac_sys_types.h

nobase_library_include_HEADERS = \
  sstmac_cpu_set.h \
  sstmac_pthread_impl.h \
  sstmac_pthread_macro.h \
  sstmac_pthread_clear_macros.h \
  sstmac_pthread.h \
  sstmac_pthread_runner.h \
  sstmac_sched.h \
  sstmac_sched_macro.h \
  sstmac_sched_clear_macros.h 

