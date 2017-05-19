/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/topology/multipath_topology.h>
#include <sprockit/util.h>
#include <sprockit/delete.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterDebugSlot(router);
RegisterDebugSlot(routing);
RegisterNamespaces("router");
RegisterKeywords(
"router",
"ugal_threshold",
);

namespace sstmac {
namespace hw {

router::router(sprockit::sim_parameters* params,
  topology *top, network_switch *sw, routing::algorithm_t algo)
 : top_(top), netsw_(sw), algo_(algo), max_num_vc_(0)
{
  init_vc();
  my_addr_ = switch_id(params->get_int_param("id"));
}

router::~router()
{
}

void
router::init_vc()
{
  max_num_vc_ = 0;
  top_->configure_vc_routing(num_vc_lookup_);
  auto iter = num_vc_lookup_.find(algo_);
  if (iter == num_vc_lookup_.end()){
    spkt_throw_printf(sprockit::value_error,
                      "invalid routing algorithm %s for given router",
                      routing::tostr(algo_));
  }
  max_num_vc_ = iter->second;
}

void
router::compatibility_check() const
{
  multipath_topology* mtop = test_cast(multipath_topology, top_);
  if (mtop){
    spkt_abort_printf("chosen router model is not compatible with multipath topologies");
  }
}

switch_id
router::find_ejection_site(node_id node_addr, routable::path &path) const
{
  return top_->node_to_ejection_switch(node_addr, path.outport);
}

void
router::route(packet *pkt)
{
  routable* rtbl = pkt->interface<routable>();
  if (!rtbl) abort();
  routable::path& path = rtbl->current_path();
  switch_id sid = find_ejection_site(pkt->toaddr(), path);
  if (sid == my_addr_){
    rter_debug("Ejecting %s from switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport);
    configure_ejection_path(path);
  }
  else {
    rter_debug("Routing %s to switch %d on port %d",
               pkt->to_string().c_str(), sid, path.outport);
    route_to_switch(sid, path);
  }
}

routing::algorithm_t
router::str_to_algo(const std::string &str)
{
  if (str == "minimal") {
    return routing::minimal;
  }
  else if (str == "valiant") {
    return routing::valiant;
  }
  else if (str == "min_ad") {
    return routing::minimal_adaptive;
  }
  else if (str == "ugal") {
    return routing::ugal;
  }
  else {
    spkt_throw_printf(sprockit::input_error,
                     "invalid routing algorithm %s",
                     str.c_str());
  }
}

}
}