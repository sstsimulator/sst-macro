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

noinst_LTLIBRARIES = libsstmac_hw.la

libsstmac_hw_la_LDFLAGS = 

library_includedir=$(includedir)/sstmac/hardware

nobase_library_include_HEADERS = \
  simple/null_nic.h \
  simple/simple_nic.h \
  simple/simple_switch.h \
  simple/simple_param_expander.h \
  processor/processor.h \
  processor/processor_fwd.h \
  processor/instruction_processor.h \
  processor/simple_processor.h \
  memory/memory_id.h \
  memory/memory_model.h \
  memory/memory_model_fwd.h \
  memory/simple_memory_model.h \
  memory/null_memory_model.h \
  nic/nic_fwd.h \
  nic/nic.h \
  nic/netlink.h \
  nic/netlink_fwd.h \
  noise/noise.h \
  node/node.h \
  node/node_fwd.h \
  node/null_node.h \
  node/simple_node.h \
  common/connection.h \
  common/failable.h \
  common/fail_event.h \
  common/packet.h \
  common/packetizer.h \
  common/packet_fwd.h \
  common/param_expander.h \
  common/recv_cq.h \
  common/unique_id.h \
  packet_flow/packet_allocator.h \
  packet_flow/packet_allocator_fwd.h \
  packet_flow/packet_flow.h \
  packet_flow/packet_flow_fwd.h \
  packet_flow/packet_flow_stats.h \
  packet_flow/packet_flow_stats_fwd.h \
  packet_flow/packet_flow_param_expander.h \
  packet_flow/packet_flow_arbitrator.h \
  packet_flow/packet_flow_buffer.h \
  packet_flow/packet_flow_crossbar.h \
  packet_flow/packet_flow_handler.h \
  packet_flow/packet_flow_nic.h \
  packet_flow/packet_flow_packetizer.h \
  packet_flow/packet_flow_sender.h \
  packet_flow/packet_flow_switch.h \
  packet_flow/packet_flow_tiled_switch.h \
  packet_flow/packet_flow_memory_model.h \
  network/network_id.h \
  network/network_message.h \
  network/network_message_fwd.h \
  interconnect/interconnect.h \
  interconnect/interconnect_fwd.h \
  switch/network_switch.h \
  switch/network_switch_fwd.h \
  switch/dist_dummyswitch.h \
  topology/butterfly.h \
  topology/crossbar.h \
  topology/dragonfly.h \
  topology/fake_topology.h \
  topology/fat_tree.h \
  topology/hdtorus.h \
  topology/multipath_topology.h \
  topology/tiled_torus.h \
  topology/hypercube.h \
  topology/topology.h \
  topology/topology_fwd.h \
  topology/structured_topology_fwd.h \
  topology/index_subset.h \
  topology/index_subset_fwd.h \
  topology/coordinates.h \
  topology/cartesian_topology.h \
  topology/structured_topology.h \
  topology/tiled_dragonfly.h \
  topology/traffic/traffic.h \
  router/router.h \
  router/router_fwd.h \
  router/routable.h \
  router/routable_fwd.h \
  router/routing_enum.h \
  router/minimal_routing.h \
  router/multipath_routing.h \
  router/ugal_routing.h \
  router/valiant_routing.h \
  router/fat_tree_router.h 

libsstmac_hw_la_SOURCES =  \
  simple/null_nic.cc \
  simple/simple_nic.cc \
  simple/simple_switch.cc \
  simple/simple_param_expander.cc \
  processor/processor.cc \
  processor/simple_processor.cc \
  processor/instruction_processor.cc \
  common/packet.cc \
  common/packetizer.cc \
  common/param_expander.cc \
  common/recv_cq.cc \
  memory/memory_model.cc \
  memory/simple_memory_model.cc \
  memory/null_memory_model.cc \
  nic/netlink.cc \
  nic/nic.cc \
  noise/noise.cc \
  node/node.cc \
  node/null_node.cc \
  node/simple_node.cc \
  packet_flow/packet_flow.cc \
  packet_flow/packet_allocator.cc \
  packet_flow/packet_flow_stats.cc \
  packet_flow/packet_flow_arbitrator.cc \
  packet_flow/packet_flow_buffer.cc \
  packet_flow/packet_flow_crossbar.cc \
  packet_flow/packet_flow_handler.cc \
  packet_flow/packet_flow_nic.cc \
  packet_flow/packet_flow_packetizer.cc \
  packet_flow/packet_flow_param_expander.cc \
  packet_flow/packet_flow_sender.cc \
  packet_flow/packet_flow_switch.cc \
  packet_flow/packet_flow_tiled_switch.cc \
  packet_flow/packet_flow_memory_model.cc \
  network/network_message.cc \
  interconnect/interconnect.cc \
  switch/network_switch.cc \
  switch/dist_dummyswitch.cc \
  topology/butterfly.cc \
  topology/crossbar.cc \
  topology/dragonfly.cc \
  topology/fat_tree.cc \
  topology/hdtorus.cc \
  topology/tiled_torus.cc \
  topology/hypercube.cc \
  topology/coordinates.cc \
  topology/tiled_dragonfly.cc \
  topology/traffic/traffic.cc \
  router/router.cc \
  router/routable.cc \
  router/fat_tree_router.cc \
  router/minimal_routing.cc \
  router/multipath_routing.cc \
  router/ugal_routing.cc \
  router/valiant_routing.cc \
  topology/structured_topology.cc \
  topology/cartesian_topology.cc \
  topology/topology.cc 



