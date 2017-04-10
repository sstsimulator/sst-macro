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
  sstCoreElement.h 

if USE_REPLACEMENT_HEADERS
nobase_library_include_HEADERS += \
  linux/mmtimer.h \
  sstmac_pthread_return.h \
  sys/types.h 

nobase_nodist_library_include_HEADERS += \
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
  pthread.h \
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

