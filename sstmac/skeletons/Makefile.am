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

noinst_LTLIBRARIES = libsstmac_skeletons.la
library_includedir=$(includedir)/sstmac/skeletons

if HAVE_OTF2
AM_CPPFLAGS += $(OTF2_CPPFLAGS)
AM_LDFLAGS += $(OTF2_LDFLAGS)
endif

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi -I$(top_builddir)/sumi -I$(top_srcdir)/sumi
AM_LDFLAGS += -lotf2

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  test/service_test.cc \
  traffic_matrix/main.cc 

if BUILD_SUMI_MPI
libsstmac_skeletons_la_SOURCES += \
  undumpi/parsedumpi.cc \
  undumpi/parsedumpi_callbacks.cc 

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

endif





