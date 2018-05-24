#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2018 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsstmac_hw.la

libsstmac_hw_la_LDFLAGS = 

library_includedir=$(includedir)/sstmac/hardware

nobase_library_include_HEADERS = \
  logp/logp_nic.h \
  logp/logp_switch.h \
  logp/logp_switch_fwd.h \
  logp/logp_param_expander.h \
  logp/logp_memory_model.h \
  processor/processor.h \
  processor/processor_fwd.h \
  processor/instruction_processor.h \
  processor/simple_processor.h \
  memory/memory_id.h \
  memory/memory_model.h \
  memory/memory_model_fwd.h \
  nic/nic_fwd.h \
  nic/nic.h \
  nic/netlink.h \
  nic/netlink_fwd.h \
  noise/noise.h \
  node/node.h \
  node/node_fwd.h \
  node/simple_node.h \
  common/connection.h \
  common/connection_fwd.h \
  common/failable.h \
  common/packet.h \
  common/packetizer.h \
  common/packet_fwd.h \
  common/param_expander.h \
  common/recv_cq.h \
  common/unique_id.h \
  pisces/pisces.h \
  pisces/pisces_fwd.h \
  pisces/pisces_stats.h \
  pisces/pisces_stats_fwd.h \
  pisces/pisces_param_expander.h \
  pisces/pisces_arbitrator.h \
  pisces/pisces_buffer.h \
  pisces/pisces_branched_switch.h \
  pisces/pisces_crossbar.h \
  pisces/pisces_nic.h \
  pisces/pisces_packetizer.h \
  pisces/pisces_sender.h \
  pisces/pisces_switch.h \
  pisces/pisces_tiled_switch.h \
  pisces/pisces_memory_model.h \
  sculpin/sculpin_nic.h \
  sculpin/sculpin_switch.h \
  sculpin/sculpin_param_expander.h \
  sculpin/sculpin.h \
  network/network_id.h \
  network/network_message.h \
  network/network_message_fwd.h \
  interconnect/interconnect.h \
  interconnect/interconnect_fwd.h \
  switch/network_switch.h \
  switch/network_switch_fwd.h \
  topology/butterfly.h \
  topology/fully_connected.h \
  topology/cascade.h \
  topology/dragonfly.h \
  topology/dragonfly_plus.h \
  topology/fat_tree.h \
  topology/torus.h \
  topology/multipath_topology.h \
  topology/tiled_torus.h \
  topology/hypercube.h \
  topology/topology.h \
  topology/topology_fwd.h \
  topology/structured_topology_fwd.h \
  topology/coordinates.h \
  topology/cartesian_topology.h \
  topology/star.h \
  topology/structured_topology.h \
  topology/tiled_cascade.h \
  topology/traffic/traffic.h \
  router/router.h \
  router/router_fwd.h \
  router/routing_enum.h \
  router/minimal_routing.h \
  router/multipath_routing.h \
  router/ugal_routing.h \
  router/par_routing.h \
  router/valiant_routing.h \
  router/fat_tree_router.h 

libsstmac_hw_la_SOURCES =  \
  logp/logp_nic.cc \
  logp/logp_switch.cc \
  logp/logp_param_expander.cc \
  logp/logp_memory_model.cc \
  processor/processor.cc \
  processor/simple_processor.cc \
  processor/instruction_processor.cc \
  common/connection.cc \
  common/packet.cc \
  common/packetizer.cc \
  common/param_expander.cc \
  common/recv_cq.cc \
  memory/memory_model.cc \
  nic/netlink.cc \
  nic/nic.cc \
  noise/noise.cc \
  node/node.cc \
  node/simple_node.cc \
  pisces/pisces.cc \
  pisces/pisces_stats.cc \
  pisces/pisces_arbitrator.cc \
  pisces/pisces_buffer.cc \
  pisces/pisces_branched_switch.cc \
  pisces/pisces_crossbar.cc \
  pisces/pisces_nic.cc \
  pisces/pisces_packetizer.cc \
  pisces/pisces_param_expander.cc \
  pisces/pisces_sender.cc \
  pisces/pisces_switch.cc \
  pisces/pisces_tiled_switch.cc \
  pisces/pisces_memory_model.cc \
  sculpin/sculpin_nic.cc \
  sculpin/sculpin_switch.cc \
  sculpin/sculpin_param_expander.cc \
  sculpin/sculpin.cc \
  network/network_message.cc \
  interconnect/interconnect.cc \
  switch/network_switch.cc \
  topology/butterfly.cc \
  topology/fully_connected.cc \
  topology/cascade.cc \
  topology/dragonfly.cc \
  topology/dragonfly_plus.cc \
  topology/fat_tree.cc \
  topology/torus.cc \
  topology/tiled_torus.cc \
  topology/hypercube.cc \
  topology/coordinates.cc \
  topology/tiled_cascade.cc \
  topology/traffic/traffic.cc \
  router/router.cc \
  router/dragonfly_minimal.cc \
  router/fat_tree_router.cc \
  router/minimal_routing.cc \
  router/ugal_routing.cc \
  router/par_routing.cc \
  router/valiant_routing.cc \
  topology/star.cc \
  topology/structured_topology.cc \
  topology/cartesian_topology.cc \
  topology/topology.cc 



if INTEGRATED_SST_CORE
libsstmac_hw_la_SOURCES +=  \
  pisces/pisces_simple_network.cc

nobase_library_include_HEADERS += \
  pisces/pisces_simple_network.h
endif

