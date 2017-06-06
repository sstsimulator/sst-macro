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

#include <sstmac/software/launch/node_id_allocation.h>

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/software/launch/coordinate_allocation.h>
#include <sprockit/fileio.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords("launch_node_id_allocation_file",
"node_id_allocation_file",
"node_id_file");

namespace sstmac {
namespace sw {

node_id_allocation::node_id_allocation(sprockit::sim_parameters* params) :
  node_allocator(params)
{
  if (params->has_param("node_id_file")){
    coord_file_ = params->get_param("node_id_file");
  } else {
    coord_file_ = params->get_param("node_id_allocation_file");
  }
}

void
node_id_allocation::read_coordinate_file(
  const std::string &file,
  std::vector<node_id> &node_list,
  hw::topology* top)
{
  std::ifstream in;
  sprockit::SpktFileIO::open_file(in, file);

  if (!in.is_open()) {
    spkt_throw_printf(sprockit::input_error,
     "node_id_allocation: could not find node id file %s in current folder or configuration include path",
     file.c_str());
  }

  int num_nodes;
  in >> num_nodes;

  int max_node_id = top->num_nodes();

  node_list.resize(num_nodes);
  for (int nid=0; nid < num_nodes; ++nid){
    int next;
    in >> next;
    if (next >= max_node_id){
      spkt_throw_printf(sprockit::value_error,
        "node_id_allocation: invalid node id %d in file %s - max id is %d",
        next, file.c_str(), max_node_id);
    }
    node_list[nid] = node_id(next);
  }
}

void
node_id_allocation::allocate(int nnode_requested,
  const ordered_node_set& available,
  ordered_node_set &allocation) const
{
  std::vector<node_id> node_list;
  read_coordinate_file(coord_file_, node_list, topology_);

  if (node_list.size() < nnode_requested){
    spkt_throw_printf(sprockit::value_error,
       "application needs %d node, but only %d listed in file %s",
       nnode_requested, node_list.size(), coord_file_.c_str());
  }

  for (int i=0; i < nnode_requested; ++i){
    node_id nid = node_list[i];
    debug_printf(sprockit::dbg::allocation,
        "adding node %d to allocation",
        int(nid));
    allocation.insert(nid);
  }
}

}
}