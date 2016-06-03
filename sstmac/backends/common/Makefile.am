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

noinst_LTLIBRARIES = libsstmac_backends.la

AM_CPPFLAGS += -I$(top_srcdir)/sstmac/backends/common -I$(top_builddir)/sstmac/backends/common
AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi

library_includedir=$(includedir)/sstmac/backends/common

nobase_library_include_HEADERS = \
  sim_partition.h \
  sim_partition_fwd.h \
  parallel_runtime_fwd.h \
  parallel_runtime.h

libsstmac_backends_la_SOURCES = \
  parallel_runtime.cc \
  sim_partition.cc

