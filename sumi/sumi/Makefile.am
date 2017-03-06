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

if ENABLE_SST_SUBREPO
noinst_LTLIBRARIES = libsumi.la
else
lib_LTLIBRARIES = libsumi.la
libsumi_la_LDFLAGS = -version-info @SUMI_LIBVERSION@
endif

libsumi_la_LIBADD = 


library_includedir=$(includedir)/sumi/sumi

nodist_library_include_HEADERS = config.h sumi_config.h

library_include_HEADERS = \
 alltoall.h \
 alltoallv.h \
 allgather.h \
 allgatherv.h \
 allreduce.h \
 gather.h \
 gatherv.h \
 bcast.h \
 reduce.h \
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
 dynamic_tree_vote.h \
 lockable.h \
 message.h \
 message_fwd.h \
 monitor.h \
 options.h \
 partner_timeout.h \
 ping.h \
 rdma.h \
 rdma_mdata.h \
 serialization.h \
 thread.h \
 thread_lock.h \
 thread_safe_int.h \
 thread_safe_list.h \
 thread_safe_ptr_type.h \
 thread_safe_refcount_ptr.h \
 thread_safe_set.h \
 timeout.h \
 transport.h \
 transport_fwd.h 

libsumi_la_SOURCES = \
 alltoall.cc \
 alltoallv.cc \
 transport.cc \
 allgather.cc \
 allgatherv.cc \
 allreduce.cc \
 bcast.cc \
 gather.cc \
 gatherv.cc \
 reduce.cc \
 scan.cc \
 scatter.cc \
 scatterv.cc \
 collective.cc \
 collective_actor.cc \
 collective_message.cc \
 dense_rank_map.cc \
 communicator.cc \
 dynamic_tree_vote.cc \
 message.cc \
 monitor.cc \
 partner_timeout.cc \
 ping.cc \
 thread_lock.cc \
 thread_safe_set.cc 


if ENABLE_MPI
libsumi_la_LIBADD += ../mpi/libsumi_mpi.la
libsumi_la_SOURCES += \
 active_msg_transport.cc 
library_include_HEADERS += \
 active_msg_transport.h 
endif

if ENABLE_GNI
libsumi_la_LIBADD += ../gni/libsumi_gni.la
libsumi_la_SOURCES += \
 active_msg_transport.cc 
library_include_HEADERS += \
 active_msg_transport.h 
endif
