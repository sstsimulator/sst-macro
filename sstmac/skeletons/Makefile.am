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

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi -I$(top_builddir)/sumi -I$(top_srcdir)/sumi
AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  traffic_matrix/main.cc \
  test/service_test.cc \
  test/app_hello_world.cc \
  test/mpi_coverage_test.cc \
  test/mpi_ping_all.cc \
  test/mpi_delay_stats.cc \
  test/global_test.cc \
  sumi_undumpi/parsedumpi.cc \
  sumi_undumpi/parsedumpi_callbacks.cc \
  test/sstmac_mpi_test_all.cc 

nobase_library_include_HEADERS = \
  sumi_undumpi/parsedumpi.h \
  sumi_undumpi/parsedumpi_callbacks.h 



