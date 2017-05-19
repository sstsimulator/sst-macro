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

#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
  namespace hw {

cartesian_topology::cartesian_topology(sprockit::sim_parameters *params,
                                       InitMaxPortsIntra i1,
                                       InitGeomEjectID i2) :
  structured_topology(params, i1, i2)
{
  params->get_vector_param("geometry", dimensions_);
  if (dimensions_.size() == 0) {
    spkt_throw_printf(sprockit::value_error, "empty topology vector for hdtorus");
  }

  if (params->has_param("redundant")) {
    params->get_vector_param("redundant", red_);
    if (red_.size() != dimensions_.size()) {
      spkt_throw_printf(sprockit::input_error,
                       "topology::init: wrong number of dimensions in topology_redundant, "
                       "should be %d, got %d\n",
                       dimensions_.size(),
                       red_.size());
    }
  }
  else {
    int ndim = dimensions_.size();
    red_.resize(ndim);
    for (int i = 0; i < ndim; i++) {
      red_[i] = 1;
    }
  }
}

coordinates
cartesian_topology::node_coords(node_id uid) const
{
  if (concentration_ == 1) {
    return switch_coords((switch_id)uid);
  }
  else {
    switch_id swid(uid / concentration_);
    int lastidx = uid % concentration_;
    coordinates coords = switch_coords(swid);
    coords.push_back(lastidx);
    return coords;
  }
}

node_id
cartesian_topology::node_addr(const coordinates& coords) const
{

  int offset = 0;
  if (coords.size() > ndimensions()) {
    //there is no "extra" switch index
    offset = coords[ndimensions()];
  }

  int swid = switch_addr(coords);
  node_id nid = swid * concentration_ + offset;
  return nid;
}

std::string
cartesian_topology::node_label(node_id nid) const
{
  return node_coords(nid).to_string();
}

std::string
cartesian_topology::switch_label(switch_id sid) const
{
  return switch_coords(sid).to_string();
}

  }
}