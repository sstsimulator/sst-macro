/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/software/launch/hostname_allocation.h>
#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>
#include <sprockit/fileio.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("hostname",
            node_allocator,
            hostname_allocation,
            "Given a file containing one hostname/coordinate pair per line, return a node allocation with all hosts in the file");

static const char* deprecated_keywords[] = { "launch_dumpi_mapname" };
static sprockit::StaticKeywordRegister deprecated(1, deprecated_keywords);

hostname_allocation::nodemap_t hostname_allocation::hostnamemap_;

std::map<long, std::string> hostname_allocation::nodenum_to_host_map_;

void
hostname_allocation::init_factory_params(sprockit::sim_parameters* params)
{
  node_allocator::init_factory_params(params);
  if (params->has_param("launch_dumpi_mapname")) {
    mapfile_ = params->deprecated_param("launch_dumpi_mapname");
  }
  else {
    /** sstkeyword {
            gui=path_to_file;
            docstring=A file containing a map of hostnames for allocation.ENDL
            The first line of the hostname file has two integers - the number of hostnames ENDL
            and the number of coordinates in the topology, respectively.ENDL
            The subsequent lines are the hostnames followed by the coordinates of that host.ENDL
            DEPRECATED: launch_dumpi_mapname used to be the name of this parameter.;
        }
    */
    mapfile_ = params->get_param("launch_hostname_map");
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
 const ordered_node_set& available, ordered_node_set &allocation) const
{
  std::map<std::string, std::vector<int> > hostmap;
  read_map_file(rt_, "hostname_allocation::allocate", mapfile_, hostmap);

  if (!topology_) {
    spkt_throw_printf(sprockit::value_error, "hostname_allocation::allocate: null topology");
  }

  hw::structured_topology* regtop = safe_cast(hw::structured_topology, topology_);
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

