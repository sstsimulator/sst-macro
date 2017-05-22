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

#include <sstmac/libraries/machines/bgp.h>
#include <sstmac/common/runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/util.h>

extern "C"
void
Kernel_GetPersonality(_BGP_Personality_t *p, int size)
{
  sstmac::node_id nid = sstmac::runtime::current_node();
  sstmac::hw::topology* top = sstmac::hw::topology::static_topology(nullptr);

  sstmac::hw::hdtorus* torus = test_cast(sstmac::hw::hdtorus, top);
  if (!torus || torus->ndimensions() != 3){
    spkt_throw(sprockit::value_error,
        "Kernel_GetPersonality for BGP being called, but topology is not a 3D torus");
  }

  std::vector<int> coords = torus->node_coords(nid);
  std::vector<int> dims = torus->dimensions();
  p->Network_Config.Xcoord = coords[0];
  p->Network_Config.Ycoord = coords[1];
  p->Network_Config.Zcoord = coords[2];
  p->Network_Config.Xnodes = dims[0];
  p->Network_Config.Ynodes = dims[1];
  p->Network_Config.Znodes = dims[2];
}