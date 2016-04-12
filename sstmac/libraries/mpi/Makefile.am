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

library_includedir=$(includedir)/sstmac/libraries/mpi

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi

AM_FCFLAGS = -I$(top_builddir) -I$(top_srcdir) -m32 -m64

libsstmac_mpi_la_LDFLAGS = 

libsstmac_mpi_la_SOURCES = \
  mpi_comm/mpi_comm.cc \
  mpi_comm/mpi_comm_cart.cc \
  mpi_comm/mpi_group.cc \
  mpi_comm/keyval.cc \
  mpi_comm/mpi_comm_factory_pending_keylist.cc \
  mpi_comm/mpi_comm_factory_pending.cc \
  mpi_comm/mpi_comm_factory.cc \
  mpi_comm/mpi_comm_id.cc \
  mpi_queue/mpi_queue_probe_request.cc \
  mpi_queue/mpi_queue_recv_request.cc \
  mpi_queue/mpi_queue_send_request.cc \
  mpi_queue/mpi_queue.cc \
  mpi_queue/user_thread_mpi_queue.cc \
  mpi_implementation/mpi_implementation.cc \
  mpi_implementation/basic_mpi.cc \
  mpi_implementation/rdma_mpi.cc \
  mpi_implementation/simple_mpi.cc \
  mpi_strategies/mpi_collective/engines/mpi_barrier_engine.cc \
  mpi_strategies/mpi_collective/engines/mpi_bcast_engine.cc \
  mpi_strategies/mpi_collective/engines/mpi_btree_gather.cc \
  mpi_strategies/mpi_collective/engines/mpi_btree_scatter.cc \
  mpi_strategies/mpi_collective/engines/mpi_direct_alltoall.cc \
  mpi_strategies/mpi_collective/engines/mpi_direct_gatherv.cc \
  mpi_strategies/mpi_collective/engines/mpi_direct_scatterv.cc \
  mpi_strategies/mpi_collective/engines/mpi_halving_reduce_scatter.cc \
  mpi_strategies/mpi_collective/engines/mpi_rabenseifner.cc \
  mpi_strategies/mpi_collective/engines/mpi_logtwo_virt.cc \
  mpi_strategies/mpi_collective/engines/mpi_ring_allgather.cc \
  mpi_strategies/mpi_collective/engines/mpi_bruck_allgather.cc \
  mpi_strategies/mpi_collective/engines/mpi_ring_allgatherv.cc \
  mpi_strategies/mpi_collective/engines/mpi_simultaneous_btree_scan.cc \
  mpi_strategies/mpi_collective/engines/mpi_linear_barrier_engine.cc \
  mpi_strategies/mpi_collective/engines/mpi_ring_barrier_engine.cc \
  mpi_strategies/mpi_collective/mpi_collective_recver.cc \
  mpi_strategies/mpi_collective/mpi_collective.cc \
  mpi_strategies/mpi_collective/mpi_round_by_round_collective.cc \
  mpi_strategies/mpi_collective/mpi_collective_payload.cc \
  mpi_strategies/mpi_iterator/bt_scatter_iterator.cc \
  mpi_strategies/mpi_iterator/hfm_iterator.cc \
  mpi_strategies/mpi_iterator/rd_iterator.cc \
  mpi_strategies/mpi_core_allgather.cc \
  mpi_strategies/mpi_core_allgatherv.cc \
  mpi_strategies/mpi_core_allreduce.cc \
  mpi_strategies/mpi_core_alltoall.cc \
  mpi_strategies/mpi_core_alltoallv.cc \
  mpi_strategies/mpi_core_barrier.cc \
  mpi_strategies/mpi_core_bcast.cc \
  mpi_strategies/mpi_core_gather.cc \
  mpi_strategies/mpi_core_gatherv.cc \
  mpi_strategies/mpi_core_recv.cc \
  mpi_strategies/mpi_core_reduce.cc \
  mpi_strategies/mpi_core_reduce_scatter.cc \
  mpi_strategies/mpi_core_rsend.cc \
  mpi_strategies/mpi_core_scan.cc \
  mpi_strategies/mpi_core_scatter.cc \
  mpi_strategies/mpi_core_scatterv.cc \
  mpi_strategies/mpi_core_send.cc \
  mpi_strategies/mpi_core_ssend.cc \
  mpi_strategies/mpi_strategy.cc \
  mpi_protocol/mpi_protocol.cc \
  mpi_protocol/eager1_rdma.cc \
  mpi_protocol/eager0.cc \
  mpi_protocol/eager0_socket.cc \
  mpi_protocol/eager0_mmap.cc \
  mpi_protocol/eager0_rdma.cc \
  mpi_protocol/eager_ssend.cc \
  mpi_protocol/rendezvous.cc \
  mpi_protocol/rendezvous_mmap.cc \
  mpi_protocol/rendezvous_socket.cc \
  mpi_protocol/rendezvous_rdma.cc \
  mpi_protocol/rendezvous_rma.cc \
  mpi_types/mpi_id.cc \
  mpi_types/mpi_op.cc \
  mpi_types/mpi_tag.cc \
  mpi_types/mpi_type.cc \
  mpi_api_persistent.cc \
  mpi_api.cc \
  mpi_app.cc \
  mpi_debug.cc \
  mpi_message.cc \
  mpi_request.cc \
  mpi_server.cc \
  mpi_status.cc \
  mpi_payload.cc \
  rma/mpi_rma_message.cc \
  rma/mpi_queue_get_request.cc \
  sstmac_mpi.cc \
  type_operator.cc


nobase_library_include_HEADERS = \
  mpi_comm/keyval.h \
  mpi_comm/keyval_fwd.h \
  mpi_comm/mpi_comm.h \
  mpi_comm/mpi_comm_fwd.h \
  mpi_comm/mpi_comm_cart.h \
  mpi_comm/mpi_comm_data.h \
  mpi_comm/mpi_comm_factory_pending.h \
  mpi_comm/mpi_comm_factory.h \
  mpi_comm/mpi_comm_id.h \
  mpi_comm/mpi_group.h \
  mpi_queue/mpi_queue_probe_request_fwd.h \
  mpi_queue/mpi_queue_recv_request_fwd.h \
  mpi_queue/mpi_queue_send_request_fwd.h \
  mpi_queue/mpi_queue_probe_request.h \
  mpi_queue/mpi_queue_recv_request.h \
  mpi_queue/mpi_queue_send_request.h \
  mpi_queue/mpi_queue.h \
  mpi_queue/mpi_queue_fwd.h \
  mpi_queue/user_thread_mpi_queue.h \
  mpi_implementation/mpi_implementation.h \
  mpi_implementation/mpi_implementation_fwd.h \
  mpi_implementation/basic_mpi.h \
  mpi_implementation/rdma_mpi.h \
  mpi_implementation/simple_mpi.h \
  mpi_protocol/mpi_protocol.h \
  mpi_protocol/mpi_protocol_fwd.h \
  mpi_strategies/mpi_collective/engines/mpi_barrier_engine.h \
  mpi_strategies/mpi_collective/engines/mpi_bcast_engine.h \
  mpi_strategies/mpi_collective/engines/mpi_btree_gather.h \
  mpi_strategies/mpi_collective/engines/mpi_btree_scatter.h \
  mpi_strategies/mpi_collective/engines/mpi_direct_alltoall.h \
  mpi_strategies/mpi_collective/engines/mpi_direct_gatherv.h \
  mpi_strategies/mpi_collective/engines/mpi_direct_scatterv.h \
  mpi_strategies/mpi_collective/engines/mpi_halving_reduce_scatter.h \
  mpi_strategies/mpi_collective/engines/mpi_rabenseifner.h \
  mpi_strategies/mpi_collective/engines/mpi_logtwo_virt.h \
  mpi_strategies/mpi_collective/engines/mpi_rank_map.h \
  mpi_strategies/mpi_collective/engines/mpi_ring_allgather.h \
  mpi_strategies/mpi_collective/engines/mpi_bruck_allgather.h \
  mpi_strategies/mpi_collective/engines/mpi_ring_allgatherv.h \
  mpi_strategies/mpi_collective/engines/mpi_simultaneous_btree_scan.h \
  mpi_strategies/mpi_collective/engines/mpi_linear_barrier_engine.h \
  mpi_strategies/mpi_collective/engines/mpi_ring_barrier_engine.h \
  mpi_strategies/mpi_collective/mpi_collective_recver.h \
  mpi_strategies/mpi_collective/mpi_collective_sender.h \
  mpi_strategies/mpi_collective/mpi_collective.h \
  mpi_strategies/mpi_collective/mpi_collective_fwd.h \
  mpi_strategies/mpi_collective/mpi_round_by_round_collective.h \
  mpi_strategies/mpi_collective/mpi_collective_payload.h \
  mpi_strategies/mpi_iterator/bt_scatter_iterator.h \
  mpi_strategies/mpi_iterator/hfm_iterator.h \
  mpi_strategies/mpi_iterator/rd_iterator.h \
  mpi_strategies/mpi_iterator/topology_iterator.h \
  mpi_strategies/mpi_core_allgather.h \
  mpi_strategies/mpi_core_allgatherv.h \
  mpi_strategies/mpi_core_allreduce.h \
  mpi_strategies/mpi_core_alltoall.h \
  mpi_strategies/mpi_core_alltoallv.h \
  mpi_strategies/mpi_core_barrier.h \
  mpi_strategies/mpi_core_bcast.h \
  mpi_strategies/mpi_core_gather.h \
  mpi_strategies/mpi_core_gatherv.h \
  mpi_strategies/mpi_core_recv.h \
  mpi_strategies/mpi_core_reduce.h \
  mpi_strategies/mpi_core_reduce_scatter.h \
  mpi_strategies/mpi_core_rsend.h \
  mpi_strategies/mpi_core_scan.h \
  mpi_strategies/mpi_core_scatter.h \
  mpi_strategies/mpi_core_scatterv.h \
  mpi_strategies/mpi_core_send.h \
  mpi_strategies/mpi_core_ssend.h \
  mpi_strategies/mpi_core_strategies.h \
  mpi_strategies/mpi_strategies.h \
  mpi_strategies/mpi_strategy.h \
  mpi_types/mpi_id.h \
  mpi_types/mpi_info.h \
  mpi_types/mpi_op.h \
  mpi_types/mpi_tag.h \
  mpi_types/mpi_type.h \
  mpi_api_persistent.h \
  mpi_api.h \
  mpi_api_fwd.h \
  mpi_debug.h \
  mpi_payload.h \
  mpi_app.h \
  mpi_message.h \
  mpi_request.h \
  mpi_request_fwd.h \
  mpi_server.h \
  mpi_server_fwd.h \
  mpi_status.h \
  mpi_status_fwd.h \
  mpi_types.h \
  mpi_wrapper.h \
  rma/mpi_rma_message.h \
  rma/mpi_window.h \
  rma/mpi_queue_get_request.h \
  rma/mpi_queue_get_request_fwd.h \
  sstmac_mpi.h \
  sstmac_mpi_integers.h \
  sstmac_mpi_macro.h \
  type_operator.h
  


