#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2018 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsstmac_omp.la
library_includedir=$(includedir)/sstmac/libraries/omp

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_omp_la_LDFLAGS = 

libsstmac_omp_la_SOURCES = \
  libomp.cc


