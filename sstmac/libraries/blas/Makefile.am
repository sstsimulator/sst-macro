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

lib_LTLIBRARIES = libsstmac_blas.la

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_blas_la_SOURCES = \
  ddot.cc \
  dgemm.cc \
  dgemv.cc \
  daxpy.cc \
  cblas.cc \
  blas_api.cc 

library_includedir=$(includedir)/sstmac/libraries/blas

nobase_library_include_HEADERS = \
  cblas.h \
  blas.h \
  blas_api.h

