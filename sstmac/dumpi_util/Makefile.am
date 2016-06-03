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

noinst_LTLIBRARIES = libsstmac_dumpi.la

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_dumpi_la_LDFLAGS = 
libsstmac_dumpi_la_LIBADD = \
  ../../sst-dumpi/dumpi/libundumpi/libundumpi.la

libsstmac_dumpi_la_SOURCES = \
dumpi_meta.cc \
dumpi_util.cc 

library_includedir=$(includedir)/sstmac/dumpi_util

nobase_library_include_HEADERS = \
dumpi_meta.h \
dumpi_type_io.h \
dumpi_util.h 

