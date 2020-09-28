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

noinst_LTLIBRARIES = libsstmac_test_skeletons.la
library_includedir=$(includedir)/sstmac/test_skeletons


AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements -DSSTMAC=1

if HAVE_OTF2
AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi $(OTF2_CPPFLAGS)
endif

libsstmac_test_skeletons_la_LDFLAGS = 

libsstmac_test_skeletons_la_SOURCES = \
  bw_test.cc \
  mem_bandwidth.cc \
  mem_thrash.cc \
  host_compute.cc \
  partner_stride.cc \
  app_hello_world.cc \
  dfly_worst_case.cc \
  compute.cc \
  mpi_coverage_test.cc \
  mpi_ping_all.cc \
  mpi_tournament.cc \
  mpi_ping_pong.cc \
  mpi_all_collectives.cc \
  mpi_smp_collectives.cc \
  mpi_delay_stats.cc \
  mpi_isend_progress.cc \
  memory_leak_test.cc \
  sstmac_mpi_test_all.cc 


if HAVE_SST_ELEMENTS
libsstmac_test_skeletons_la_SOURCES += ariel_test.cc
endif



