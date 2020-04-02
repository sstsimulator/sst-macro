#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2020 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsstmac_skeletons.la
library_includedir=$(includedir)/sstmac/skeletons


AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi -I$(top_builddir)/sumi -I$(top_srcdir)/sumi

nobase_library_include_HEADERS = \
  undumpi/parsedumpi.h \
  undumpi/parsedumpi_callbacks.h 

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  traffic_matrix/main.cc \
  undumpi/parsedumpi.cc \
  undumpi/parsedumpi_callbacks.cc 

libsstmac_skeletons_la_LIBADD =
if HAVE_OTF2
AM_CPPFLAGS += $(OTF2_CPPFLAGS)
AM_LDFLAGS += $(OTF2_LDFLAGS) 
libsstmac_skeletons_la_LIBADD += $(OTF2_LIBS)
libsstmac_skeletons_la_LIBADD += ../../sst-dumpi/dumpi/libotf2dump/libotf2dump.la

libsstmac_skeletons_la_SOURCES += \
  otf2_trace_replay/callbacks.cc \
  otf2_trace_replay/callqueue.cc \
  otf2_trace_replay/otf2_trace_replay.cc \
  otf2_trace_replay/structures.cc


nobase_library_include_HEADERS += \
 otf2_trace_replay/callbacks.h \
 otf2_trace_replay/callid.h \
 otf2_trace_replay/callqueue.h \
 otf2_trace_replay/mpicall.h \
 otf2_trace_replay/otf2_trace_replay.h \
 otf2_trace_replay/structures.h
endif






