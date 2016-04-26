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

noinst_LTLIBRARIES = libsumi_gni.la
libsumi_gni_la_LDFLAGS = 

libsumi_gni_la_SOURCES = \
 cdm.cc \
 cq.cc \
 ep.cc \
 fakegni.cc \
 finalize.cc \
 gather.cc \
 gni_transport.cc \
 init.cc \
 mem.cc \
 progress.cc \
 rdma_get.cc \
 rdma_poll.cc \
 rdma_post.cc \
 rdma_put.cc \
 smsg.cc \
 smsg_poll.cc \
 smsg_recv.cc \
 smsg_send.cc \
 util.cc

library_includedir=$(includedir)/sumi/gni

nobase_library_include_HEADERS = \
 gather.h \
 gni_transport.h \
 rdma.h


