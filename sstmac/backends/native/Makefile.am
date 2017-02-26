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

noinst_LTLIBRARIES = libsstmac_native.la

AM_CPPFLAGS += -I$(top_srcdir)/sstmac/backends -I$(top_builddir)/sstmac/backends
AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi

library_includedir=$(includedir)/sstmac/backends/native
nobase_library_include_HEADERS = \
  manager.h \
  manager_fwd.h 

libsstmac_native_la_SOURCES = \
  manager.cc 

if !INTEGRATED_SST_CORE
nobase_library_include_HEADERS += \
  event_map.h  \
  event_calendar.h \
  serial_runtime.h \
  clock_cycle_parallel/thread_barrier.h \
  clock_cycle_parallel/clock_cycle_event_container.h \
  clock_cycle_parallel/multithreaded_event_container.h \
  clock_cycle_parallel/multithreaded_event_container_fwd.h \
  clock_cycle_parallel/multithreaded_subcontainer.h 

libsstmac_native_la_SOURCES += \
  event_map.cc \
  clock_cycle_parallel/thread_barrier.cc \
  clock_cycle_parallel/clock_cycle_event_container.cc \
  clock_cycle_parallel/multithreaded_event_container.cc \
  clock_cycle_parallel/multithreaded_subcontainer.cc \
  serial_runtime.cc 
endif

if HAVE_EVENT_CALENDAR
libsstmac_native_la_SOURCES += event_calendar.cc
endif
  


