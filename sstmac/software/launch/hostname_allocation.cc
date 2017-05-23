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

#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/software/launch/hostname_allocation.h>
#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>
#include <sprockit/fileio.h>
#include <sprockit/sim_parameters.h>

RegisterKeywords(
"dumpi_mapname",
"launch_dumpi_mapname",
"launch_hostname_map",
"hostname_map",
"launch_hostname_list",
"hostname_list",
);

namespace sstmac {
namespace sw {

hostname_allocation::nodemap_t hostname_allocation::hostnamemap_;

std::map<long, std::string> hostname_allocation::nodenum_to_host_map_;

hostname_allocation::hostname_allocation(sprockit::sim_parameters* params) :
  node_allocator(params)
{
  if (params->has_param("dumpi_mapname")) {
    mapfile_ = params->deprecated_param("dumpi_mapname");
  }
  else {
    mapfile_ = params->get_param("hostname_map");
  }
}

void
hostname_allocation::read_map_file(
  parallel_runtime* rt,
  const char* here,
  const std::string &mapfile,
  std::map<std::string, std::vector<int> >& hostmap)
{
  debug_printf(sprockit::dbg::allocation,
    "hostname_allocation: reading map file %s",
     mapfile.c_str());

  hostmap.clear();
  std::istream* instr = rt->bcast_file_stream(mapfile);
  std::istream& in = *instr;

  int nnode = -1;
  in >> nnode;
  if (nnode <= 0) {
    spkt_throw_printf(sprockit::value_error,
                     "%s: bad num nodes, %d, in node map file",
                     here, nnode);
  }
  int ncoor = -1;
  in >> ncoor;
  if (ncoor <= 0) {
    spkt_throw_printf(
      sprockit::value_error,
      "%s: bad num coords, %d, in node map file",
      here, ncoor);
  }

  for (int i = 0; i < nnode; i++) {

    std::string hostname;
    in >> hostname;

    if (hostname.size() == 0) {
      spkt_throw_printf(sprockit::value_error,
                       "%s: bad hostname in map file",
                       here);
    }
    std::vector<int> coor(ncoor);
    for (int j = 0; j < ncoor; j++) {
      coor[j] = -1;
      in >> coor[j];
      if (coor[j] < 0) {
        std::stringstream sstr;
        sstr << "[";
        for (int k=0; k < ncoor; k++) {
          sstr << " " << coor[k];
        }
        sstr << " ]";

        spkt_throw_printf(sprockit::value_error,
                         "%s: bad coordinates %s in map file",
                         here, sstr.str().c_str());
      }
    }

    hostmap[hostname] = coor;
  }

  delete instr;
}

void
hostname_allocation::allocate(int nnode_requested,
 const ordered_node_set& available,
 ordered_node_set &allocation) const
{
  std::map<std::string, std::vector<int> > hostmap;
  read_map_file(rt_, "hostname_allocation::allocate", mapfile_, hostmap);

  if (!topology_) {
    spkt_throw_printf(sprockit::value_error, "hostname_allocation::allocate: null topology");
  }

  hw::cartesian_topology* regtop = safe_cast(hw::cartesian_topology, topology_);
  std::map<std::string, std::vector<int> >::iterator it, end = hostmap.end();

  for (it = hostmap.begin(); it != end; it++) {
    std::vector<int> coords = it->second;
    // find node index for this vertex
    node_id nid = regtop->node_addr(coords);
    hostnamemap_[it->first] = nid;
    nodenum_to_host_map_[nid] = it->first;
    allocation.insert(nid);
  }
}

}
}