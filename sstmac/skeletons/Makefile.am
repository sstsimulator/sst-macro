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

EXTRA_DIST = test/test.model

noinst_LTLIBRARIES = libsstmac_skeletons.la
library_includedir=$(includedir)/sstmac/skeletons

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi -I$(top_builddir)/sumi -I$(top_srcdir)/sumi

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  traffic_matrix/main.cc \
  test/app_hello_world.cc \
  test/app_unittest.cc \
  test/test_compute.cc \
  test/test_os.cc \
  test/test_compute_c.c \
  test/mpi_ping_all.cc \
  test/global_test.cc 



nobase_library_include_HEADERS = \
  test/app_hello_world.h \
  test/test_compute.h \
  test/global_test.h 

libsstmac_skeletons_la_SOURCES += \
  sumi_undumpi/parsedumpi.cc \
  sumi_undumpi/parsedumpi_callbacks.cc \
  test/sstmac_mpi_test_all.cc \
  test/mpi_print_nodes.cc \
  test/mpi_topology.cc 

nobase_library_include_HEADERS += \
  sumi_undumpi/parsedumpi.h \
  sumi_undumpi/parsedumpi_callbacks.h \
  test/sstmac_mpi_test_all.h 



#  test/mpi_protocol_test.cc 
#  test/mpi_protocol_test.h 



