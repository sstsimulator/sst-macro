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

AM_CPPFLAGS += -I$(top_srcdir)/sumi -I$(top_builddir)/sumi
AM_CPPFLAGS += -I$(top_srcdir)/sstmac/install -DSSTMAC=1
AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements

noinst_LTLIBRARIES = libsstmac_sumi.la

libsstmac_sumi_la_LDFLAGS = 


libsstmac_sumi_la_SOURCES = \
  distributed_service.cc \
  sumi.cc \
  message.cc \
  sumi_thread.cc \
  sumi_transport.cc

library_includedir=$(includedir)/sstmac/libraries/sumi

nobase_library_include_HEADERS = \
  distributed_service.h \
  sumi.h \
  message.h \
  message_fwd.h \
  sumi_thread.h \
  sumi_transport.h

