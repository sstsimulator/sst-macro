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

library_includedir=$(includedir)/sstmac/replacements
nobase_library_includedir=$(includedir)/sstmac/replacements

nobase_nodist_library_include_HEADERS = 
nobase_library_include_HEADERS = 

nobase_library_include_HEADERS += \
  mpi.h \
  change_mpi_symbols.h \
  omp.h \
	fixIntrinsics.h \
  sstCoreElement.h 

if USE_REPLACEMENT_HEADERS
nobase_library_include_HEADERS += \
  linux/mmtimer.h \
  clear_symbol_macros.h \
  return_symbol_macros.h \
  sstmac_pthread_return.h \
  sys/types.h \
  dlfcn.h \
  fstream \
  iostream \
  ios \
  ostream \
  istream \
  condition_variable \
  list \
  map \
  unordered_map \
  unordered_set \
  memory \
  mutex \
  thread \
	sched.h \
  pthread.h \
  unistd.h \
  cstring \
  cstdlib \
  stdlib.h \
  malloc.h \
  mm_malloc.h \
  string.h \
  queue \
  sstream \
  stack \
  set \
  string \
  vector \
  signal.h \
  sstmac_pthread_clear.h \
  time.h \
  sys/time.h \
  sys/signal.h 
endif

