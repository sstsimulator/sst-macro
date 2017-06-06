#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2011 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#
include $(top_srcdir)/Makefile.common

AM_CPPFLAGS += -I$(top_srcdir)/tests/mpi/include -Wp,-w \
  -I$(srcdir) -I$(top_builddir)/pth -I$(top_srcdir)/pth -I$(top_srcdir)/sumi -I$(top_builddir)/sumi \
  -I$(top_builddir)/tests -I$(top_srcdir)/tests

EXTRA_DIST = \
  config.py 

if INTEGRATED_SST_CORE
lib_LTLIBRARIES = libsstmac_api_globals_test.la
libsstmac_api_globals_test_la_SOURCES = testglobals.cc testglobals_eli.cc
else
EXTRA_DIST += testglobals.cc testglobals_eli.cc
check_PROGRAMS = testexec
testexec_SOURCES = exec.cc
testexec_LDADD = \
  $(top_builddir)/sprockit/sprockit/libsprockit.la \
  $(top_builddir)/sstmac/install/libsstmac.la \
  $(top_builddir)/sstmac/main/libsstmac_main.la 


if EXTERNAL_BOOST
  testexec_LDADD += $(BOOST_LDFLAGS)
endif

if USE_MPIPARALLEL
testexec_LDADD += $(top_builddir)/sstmac/backends/mpi/libsstmac_mpi_runtime.la
endif

endif

