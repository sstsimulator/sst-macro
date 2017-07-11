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

library_includedir=$(includedir)/sstmac/replacements
nobase_library_includedir=$(includedir)/sstmac/replacements

nobase_nodist_library_include_HEADERS = 
nobase_library_include_HEADERS = 

nobase_library_include_HEADERS += \
  mpi.h \
  omp.h \
  sstCoreElement.h 

if USE_REPLACEMENT_HEADERS
nobase_library_include_HEADERS += \
  linux/mmtimer.h \
  clear_symbol_macros.h \
  return_symbol_macros.h \
  sstmac_pthread_return.h \
  sys/types.h \
  fstream \
  iostream \
  ios \
  ostream \
  istream \
  condition_variable \
  list \
  map \
  memory \
  mutex \
  thread \
	sched.h \
  pthread.h \
  cstring \
  cstdlib \
  stdlib.h \
  string.h \
  queue \
  sstream \
  stack \
  string \
  vector \
  signal.h \
  sstmac_pthread_clear.h \
  sys/time.h \
  sys/signal.h
endif

