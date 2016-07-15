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

if !INTEGRATED_SST_CORE
check_PROGRAMS = test_cpuset

test_cpuset_SOURCES = test_cpuset.cc

AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements \
 -I$(srcdir) \
 -I$(top_builddir) \
 -Dmain=USER_MAIN \
 -DSSTMAC=1 \
 -include sstmac/util.h \
 -include sstmac/skeleton.h \
 -include sstmac/compute.h

test_LDADD = 

if EXTERNAL_BOOST
  test_LDADD += $(BOOST_LDFLAGS)
endif

test_LDADD += \
  $(top_builddir)/sstmac/install/libsstmac.la \
  $(top_builddir)/sstmac/main/libsstmac_main.la \
  $(top_builddir)/sprockit/sprockit/libsprockit.la

if USE_MPIPARALLEL
test_LDADD += $(top_builddir)/sstmac/backends/mpi/libsstmac_mpi_runtime.la
endif

test_cpuset_LDADD = $(test_LDADD) \
 $(top_builddir)/sstmac/libraries/pthread/libsstmac_pthread.la

endif

