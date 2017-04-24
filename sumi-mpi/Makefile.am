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

noinst_LTLIBRARIES = libsstmac_mpi.la

library_includedir=$(includedir)/sumi-mpi

AM_CPPFLAGS += \
	-I$(top_srcdir)/sumi \
	-I$(top_builddir)/sumi

AM_CPPFLAGS += -I$(top_srcdir)/sstmac/install -DSSTMAC=1
AM_CPPFLAGS += -I$(top_builddir)/sstmac/replacements -I$(top_srcdir)/sstmac/replacements

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_mpi_la_LDFLAGS = 

libsstmac_mpi_la_SOURCES = \
  mpi_comm/mpi_comm.cc \
  mpi_comm/mpi_group.cc \
  mpi_comm/keyval.cc \
  mpi_comm/mpi_comm_factory.cc \
  mpi_comm/mpi_comm_cart.cc \
  mpi_queue/mpi_queue_probe_request.cc \
  mpi_queue/mpi_queue_recv_request.cc \
  mpi_queue/mpi_queue_send_request.cc \
  mpi_queue/mpi_queue.cc \
  mpi_protocol/mpi_protocol.cc \
  mpi_protocol/eager1.cc \
  mpi_protocol/eager0.cc \
  mpi_protocol/rendezvous_rdma.cc \
  mpi_types/mpi_type.cc \
  sstmac_mpi.cc \
  mpi_api.cc \
  mpi_api_collectives.cc \
  mpi_api_comm.cc \
  mpi_api_group.cc \
  mpi_api_probe.cc \
  mpi_api_send_recv.cc \
  mpi_api_test.cc \
  mpi_api_type.cc \
  mpi_api_vcollectives.cc \
  mpi_api_wait.cc \
  mpi_debug.cc \
  mpi_message.cc \
  mpi_request.cc 

nobase_library_include_HEADERS = \
  mpi_comm/keyval.h \
  mpi_comm/keyval_fwd.h \
  mpi_comm/mpi_comm.h \
  mpi_comm/mpi_comm_fwd.h \
  mpi_comm/mpi_comm_data.h \
  mpi_comm/mpi_comm_factory.h \
  mpi_comm/mpi_comm_cart.h \
  mpi_comm/mpi_group.h \
  mpi_queue/mpi_queue_probe_request_fwd.h \
  mpi_queue/mpi_queue_recv_request_fwd.h \
  mpi_queue/mpi_queue_send_request_fwd.h \
  mpi_queue/mpi_queue_probe_request.h \
  mpi_queue/mpi_queue_recv_request.h \
  mpi_queue/mpi_queue_send_request.h \
  mpi_queue/mpi_queue.h \
  mpi_queue/mpi_queue_fwd.h \
  mpi_protocol/mpi_protocol.h \
  mpi_protocol/mpi_protocol_fwd.h \
  mpi_types/mpi_type.h \
  mpi_types/mpi_type_fwd.h \
  mpi_api.h \
  mpi_api_fwd.h \
  mpi_debug.h \
  mpi_message.h \
  mpi_request.h \
  mpi_request_fwd.h \
  mpi_status.h \
  mpi_status_fwd.h \
  mpi_types.h \
  mpi_wrapper.h \
  mpi_integers.h \
  mpi_call.h \
  sstmac_mpi.h \
  sstmac_mpi_macro.h 
  



