/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sprockit/keyword_registration.h>
#include <sstream>

RegisterKeywords(
{ "launch_dumpi_mapname", "DEPRECATED: a file containing a line-by-line list of hostnames and their coordintes" },
{ "launch_hostname_map", "DEPRECATED: a file containing a line-by-line list of hostnames and their coordinates" },
{ "hostname_map", "a file containing a line-by-line list of hostnames and their coordintes" },
);

namespace sstmac {
  namespace hw {

CartesianTopology::CartesianTopology(SST::Params& params) :
  StructuredTopology(params)
{
  params.find_array("geometry", dimensions_);
  if (dimensions_.size() == 0) {
    spkt_throw_printf(sprockit::value_error, "empty topology vector for cartesian topology");
  }

  if (params->has_param("redundant")) {
    params.find_array("redundant", red_);
    if (red_.size() != dimensions_.size()) {
      spkt_throw_printf(sprockit::input_error,
                       "topology::init: wrong number of dimensions in topology_redundant, "
                       "should be %d, got %d\n",
                       dimensions_.size(),
                       red_.size());
    }
  } else {
    int ndim = dimensions_.size();
    red_.resize(ndim);
    for (int i = 0; i < ndim; i++) {
      red_[i] = 1;
    }
  }
}

void
CartesianTopology::initHostnameMap(SST::Params& params)
{
  if (params->has_param("hostname_map")){
    readCoordFile(params.find<std::string>("hostname_map"));
  } else {
    Topology::initHostnameMap(params);
  }
}

void
CartesianTopology::readCoordFile(const std::string& fname)
{
  std::ifstream in(fname.c_str());

  int nnode = -1;
  in >> nnode;
  if (nnode <= 0) {
    spkt_abort_printf("topology::read_coord_file: bad num nodes, %d, in node map file", nnode);
  }

  hostmap_.resize(numNodes());

  if (nnode < numNodes()){
    std::cerr << "WARNING: only provided "  << nnode << " hostnames, but topology has "
              << numNodes() << " nodes" << std::endl;
  } else if (nnode > numNodes()){
    spkt_abort_printf("gave %d hostnames in %s, but only have %d nodes",
                      nnode, fname.c_str(), numNodes());
  }



  int ncoor = -1;
  in >> ncoor;
  if (ncoor <= 0) {
    spkt_abort_printf("bad num coords, %d, in node map file", ncoor);
  }

  for (int i = 0; i < nnode; i++) {

    std::string hostname;
    in >> hostname;

    if (hostname.size() == 0) {
      spkt_abort_printf("bad hostname in map file");
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

        spkt_abort_printf("bad coordinates %s in map file", sstr.str().c_str());
      }
    }

    NodeId nid = node_addr(coor);
    if (nid >= hostmap_.size()){
      spkt_abort_printf("bad node number %d - max is %d in file %s",
                        nid, hostmap_.size() -1, fname.c_str());
    }
    hostmap_[nid] = hostname;
    idmap_[hostname] = nid;
  }

}

coordinates
CartesianTopology::node_coords(NodeId uid) const
{
  if (concentration_ == 1) {
    return switchCoords((SwitchId)uid);
  } else {
    SwitchId swid(uid / concentration_);
    int lastidx = uid % concentration_;
    coordinates coords = switchCoords(swid);
    coords.push_back(lastidx);
    return coords;
  }
}

NodeId
CartesianTopology::node_addr(const coordinates& coords) const
{

  int offset = 0;
  if (coords.size() > ndimensions()) {
    //there is no "extra" switch index
    offset = coords[ndimensions()];
  }

  int swid = switchAddr(coords);
  NodeId nid = swid * concentration_ + offset;
  return nid;
}

std::string
CartesianTopology::nodeLabel(NodeId nid) const
{
  return node_coords(nid).toString();
}

std::string
CartesianTopology::switchLabel(SwitchId sid) const
{
  return switchCoords(sid).toString();
}

  }
}
