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

#ifndef CARTESIAN_TOPOLOGY_H
#define CARTESIAN_TOPOLOGY_H

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 * @brief The cartesian_topology class
 * Encapsulates a topology like torus that can be naturally mapped onto
 * an n-dimensional Cartesian (integer) coordinate system
 */
class cartesian_topology :
  public structured_topology
{
 public:
  node_id
  node_addr(const coordinates& coords) const;

  virtual switch_id
  switch_addr(const coordinates& coords) const = 0;

  coordinates
  node_coords(node_id nid) const;

  virtual coordinates
  switch_coords(switch_id) const = 0;

  int
  ndimensions() const {
    return dimensions_.size();
  }

  cartesian_topology*
  cart_topology() const override {
    return const_cast<cartesian_topology*>(this);
  }

  std::string
  node_label(node_id nid) const override;

  std::string
  switch_label(switch_id sid) const override;

 protected:
  cartesian_topology(sprockit::sim_parameters* params,
                     InitMaxPortsIntra i1,
                     InitGeomEjectID i2);

  /**
   * The number of redundant links (ports) comprising a geometric
   * or structure direction in the topology
   */
  std::vector<int> red_;
  std::vector<int> dimensions_;

};

}
}

#endif // CARTESIAN_TOPOLOGY_H