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

noinst_LTLIBRARIES = libsstmac_sockets.la

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_sockets_la_SOURCES = \
  lib_sockets.cc \
  socket_api.cc 

library_includedir=$(includedir)/sstmac/libraries/sockets

nobase_library_include_HEADERS = \
  lib_sockets.h \
  socket_api.h 

