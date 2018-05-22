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

noinst_LTLIBRARIES = libsstmac_native.la

AM_CPPFLAGS += -I$(top_srcdir)/sstmac/backends -I$(top_builddir)/sstmac/backends
AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi

library_includedir=$(includedir)/sstmac/backends/native
nobase_library_include_HEADERS = \
  serial_runtime.h \
  manager.h \
  manager_fwd.h 

libsstmac_native_la_SOURCES = \
  serial_runtime.cc \
  manager.cc 

if !INTEGRATED_SST_CORE
nobase_library_include_HEADERS += \
  multithreaded_event_container.h \
  clock_cycle_event_container.h 

libsstmac_native_la_SOURCES += \
  multithreaded_event_container.cc \
  clock_cycle_event_container.cc 
endif


  


