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
  analytic/simple/simple_nic.h \
  analytic/simple/simple_interconnect.h \
  analytic/simple/simple_switch.h \
  analytic/simple/simple_topology.h \
  analytic/simple/simple_param_expander.h \
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
  nic/null_nic.h \
  nic/netlink.h \
  nic/netlink_fwd.h \
  noise/noise.h \
  node/node.h \
  node/node_fwd.h \
  node/null_node.h \
  node/simple_node.h \
  common/clonable.h \
  common/connection.h \
  common/failable.h \
  common/fail_event.h \
  common/packet.h \
  common/packet_fwd.h \
  common/param_expander.h \
  common/recv_cq.h \
  common/unique_id.h \
  packet_flow/packet_flow.h \
  packet_flow/packet_flow_stats.h \
  packet_flow/packet_flow_stats_fwd.h \
  packet_flow/packet_flow_param_expander.h \
  packet_flow/packet_flow_arbitrator.h \
  packet_flow/packet_flow_buffer.h \
  packet_flow/packet_flow_crossbar.h \
  packet_flow/packet_flow_endpoint.h \
  packet_flow/packet_flow_handler.h \
  packet_flow/packet_flow_nic.h \
  packet_flow/packet_flow_sender.h \
  packet_flow/packet_flow_switch.h \
  packet_flow/packet_flow_tiled_switch.h \
  packet_flow/packet_flow_memory_model.h \
  network/network_id.h \
  network/network_message.h \
  network/network_message_fwd.h \
  interconnect/interconnect.h \
  interconnect/interconnect_fwd.h \
  interconnect/switch_interconnect.h \
  interconnect/switch_interconnect_fwd.h \
  switch/network_switch.h \
  switch/network_switch_fwd.h \
  switch/dist_dummyswitch.h \
  topology/butterfly.h \
  topology/crossbar.h \
  topology/dragonfly.h \
  topology/fake_topology.h \
  topology/fat_tree.h \
  topology/flattened_butterfly.h \
  topology/hdtorus.h \
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
  router/routable_message.h \
  router/minad_routing.h \
  router/routing_info.h \
  router/routing_enum.h \
  router/minimal_routing.h \
  router/multipath_routing.h \
  router/structured_router.h \
  router/ugal_routing.h \
  router/valiant_routing.h \
  router/fat_tree_router.h 

libsstmac_hw_la_SOURCES =  \
  analytic/simple/simple_nic.cc \
  analytic/simple/simple_interconnect.cc \
  analytic/simple/simple_switch.cc \
  analytic/simple/simple_param_expander.cc \
  analytic/simple/simple_topology.cc \
  processor/processor.cc \
  processor/simple_processor.cc \
  processor/instruction_processor.cc \
  common/failable.cc \
  common/packet.cc \
  common/param_expander.cc \
  common/recv_cq.cc \
  memory/memory_model.cc \
  memory/simple_memory_model.cc \
  memory/null_memory_model.cc \
  nic/netlink.cc \
  nic/nic.cc \
  nic/null_nic.cc \
  noise/noise.cc \
  node/node.cc \
  node/null_node.cc \
  node/simple_node.cc \
  packet_flow/packet_flow.cc \
  packet_flow/packet_flow_stats.cc \
  packet_flow/packet_flow_arbitrator.cc \
  packet_flow/packet_flow_buffer.cc \
  packet_flow/packet_flow_crossbar.cc \
  packet_flow/packet_flow_endpoint.cc \
  packet_flow/packet_flow_handler.cc \
  packet_flow/packet_flow_nic.cc \
  packet_flow/packet_flow_param_expander.cc \
  packet_flow/packet_flow_sender.cc \
  packet_flow/packet_flow_switch.cc \
  packet_flow/packet_flow_tiled_switch.cc \
  packet_flow/packet_flow_memory_model.cc \
  network/network_message.cc \
  interconnect/interconnect.cc \
  interconnect/switch_interconnect.cc \
  switch/network_switch.cc \
  switch/dist_dummyswitch.cc \
  topology/butterfly.cc \
  topology/crossbar.cc \
  topology/dragonfly.cc \
  topology/fat_tree.cc \
  topology/flattened_butterfly.cc \
  topology/hdtorus.cc \
  topology/tiled_torus.cc \
  topology/hypercube.cc \
  topology/coordinates.cc \
  topology/tiled_dragonfly.cc \
  topology/traffic/traffic.cc \
  router/router.cc \
  router/structured_router.cc \
  router/routable_message.cc \
  router/fat_tree_router.cc \
  router/minad_routing.cc \
  router/minimal_routing.cc \
  router/multipath_routing.cc \
  router/ugal_routing.cc \
  router/valiant_routing.cc \
  router/routing_info.cc \
  topology/structured_topology.cc \
  topology/cartesian_topology.cc \
  topology/topology.cc 



