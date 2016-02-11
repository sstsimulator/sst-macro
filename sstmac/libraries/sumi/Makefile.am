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

noinst_LTLIBRARIES = libsstmac_sumi.la

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_sumi_la_LDFLAGS = 

libsstmac_sumi_la_SOURCES = \
  sumi_api.cc \
  sumi_thread.cc \
  message.cc 

library_includedir=$(includedir)/sstmac/libraries/sumi

nobase_library_include_HEADERS = \
  message.h \
  sumi_thread.h \
  sumi_api.h \
  sumi_api_fwd.h 


