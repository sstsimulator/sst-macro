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

AM_CPPFLAGS += -I$(top_builddir)/dumpi -I$(top_srcdir)/dumpi

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_skeletons_la_LDFLAGS = 

libsstmac_skeletons_la_SOURCES = \
  test/app_hello_world.cc \
  test/app_unittest.cc \
  test/test_compute.cc \
  test/test_os.cc \
  test/test_compute_c.c \
  test/global_test.cc 

nobase_library_include_HEADERS = \
  test/app_hello_world.h \
  test/test_compute.h \
  test/global_test.h 

if WITH_DEFAULT_MPI
libsstmac_skeletons_la_SOURCES += \
  undumpi/parsedumpi.cc \
  undumpi/parsedumpi_callbacks.cc \
  test/test_mpi_barrier.cc \
  test/test_onesided.cc \
  test/mpi_congestion.cc \
  test/mpi_protocol_test.cc \
  test/sstmac_mpi_test_all.cc \
  test/pdes_test.cc \
  test/test_new_collectives.cc \
  test/test_mpi_algorithms.cc \
  test/mpi_print_nodes.cc \
  test/mpi_test_all.cc \
  test/mpi_pairwise.cc \
  test/mpi_ping_all.cc \
  test/mpi_ping_pong.cc \
  test/mpi_topology.cc 

nobase_library_include_HEADERS += \
  undumpi/parsedumpi.h \
  undumpi/parsedumpi_callbacks.h \
  test/mpi_congestion.h \
  test/mpi_protocol_test.h \
  test/mpi_test_all.h \
  test/mpi_pairwise.h \
  test/mpi_ping_pong.h \
  test/sstmac_mpi_test_all.h \
  test/test_onesided.h \
  test/test_mpi_barrier.h 

if FC_ENABLE
  libsstmac_skeletons_la_SOURCES += \
    test/test_compute_f90.f90 \
    test/test_compute_f.cc

  nobase_library_include_HEADERS += \
    test/test_compute_f.h
endif

endif






