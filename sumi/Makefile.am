#
#   This file is part of SST/macroscale:
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2021, NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsumi.la

libsumi_la_LIBADD = 


library_includedir=$(includedir)/sumi

nodist_library_include_HEADERS = 

library_include_HEADERS = \
 sumi.h \
 sumi_thread.h \
 alltoall.h \
 alltoallv.h \
 allgather.h \
 allgatherv.h \
 allreduce.h \
 gather.h \
 gatherv.h \
 bcast.h \
 reduce.h \
 reduce_scatter.h \
 scan.h \
 scatter.h \
 scatterv.h \
 collective.h \
 collective_actor.h \
 collective_actor_fwd.h \
 collective_message.h \
 collective_message_fwd.h \
 comm_functions.h \
 dense_rank_map.h \
 communicator.h \
 communicator_fwd.h \
 message.h \
 message_fwd.h \
 monitor.h \
 options.h \
 timeout.h \
 transport.h \
 sim_transport.h \
 transport_fwd.h 

libsumi_la_SOURCES = \
 sumi.cc \
 sumi_thread.cc \
 alltoall.cc \
 alltoallv.cc \
 sim_transport.cc \
 allgather.cc \
 allgatherv.cc \
 allreduce.cc \
 bcast.cc \
 gather.cc \
 gatherv.cc \
 reduce.cc \
 reduce_scatter.cc \
 scan.cc \
 scatter.cc \
 scatterv.cc \
 collective.cc \
 collective_actor.cc \
 collective_message.cc \
 dense_rank_map.cc \
 communicator.cc \
 message.cc \
 monitor.cc 


