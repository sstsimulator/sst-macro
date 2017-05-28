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

noinst_LTLIBRARIES = libsstmac_skeletons.la
library_includedir=$(includedir)/sstmac/skeletons

if HAVE_OTF2
AM_CPPFLAGS += $(OTF2_CPPFLAGS)
AM_LDFLAGS += $(OTF2_LDFLAGS)
endif

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi -I$(top_builddir)/sumi -I$(top_srcdir)/sumi
AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements
AM_LDFLAGS += -lotf2

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  traffic_matrix/main.cc \
  test/compute.cc \
  test/service_test.cc \
  test/app_hello_world.cc \
  test/mpi_coverage_test.cc \
  test/mpi_ping_all.cc \
  test/mpi_all_collectives.cc \
  test/mpi_delay_stats.cc \
  test/mpi_isend_progress.cc \
  undumpi/parsedumpi.cc \
  undumpi/parsedumpi_callbacks.cc \
  test/global_test.cc \
  test/memory_leak_test.cc \
  test/sstmac_mpi_test_all.cc 

if HAVE_OTF2
  libsstmac_skeletons_la_SOURCES += \
  otf2_trace_replay/callbacks.cc \
  otf2_trace_replay/callqueue.cc \
  otf2_trace_replay/otf2_trace_replay.cc \
  otf2_trace_replay/structures.cc
endif

nobase_library_include_HEADERS = \
  undumpi/parsedumpi.h \
  undumpi/parsedumpi_callbacks.h 

if HAVE_OTF2
nobase_library_include_HEADERS += \
 otf2_trace_replay/callbacks.h \
 otf2_trace_replay/callid.h \
 otf2_trace_replay/callqueue.h \
 otf2_trace_replay/mpicall.h \
 otf2_trace_replay/otf2_trace_replay.h \
 otf2_trace_replay/structures.h
endif



