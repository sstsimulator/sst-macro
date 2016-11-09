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

#include <sstmac/common/sstmac_config.h>

#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/cartesian_topology.h>

#include <sstmac/software/launch/dumpi_allocation.h>
#include <sstmac/software/launch/node_allocator.h>
#include <sstmac/dumpi_util/dumpi_util.h>
#include <sstmac/dumpi_util/dumpi_meta.h>

#include <dumpi/libundumpi/libundumpi.h>

#include <sprockit/fileio.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {
SpktRegister("dumpi", node_allocator, dumpi_allocation,
            "Allocate nodes directly from the trace files themselves");

dumpi_allocation::dumpi_allocation(sprockit::sim_parameters* params)
 : node_allocator(params)
{
  metafile_ = params->get_param("dumpi_metaname");
}

void
dumpi_allocation::allocate(
  int nnode_requested,
   const ordered_node_set& available,
   ordered_node_set& allocation) const
{
  hw::cartesian_topology* regtop = safe_cast(hw::cartesian_topology, topology_);

  dumpi_meta* meta = new dumpi_meta(metafile_);
  int nrank = meta->num_procs();
  for (int i = 0; i < nrank; i++) {
    std::string fname = dumpi_file_name(i, meta->dirplusfileprefix_);
    dumpi_profile *profile = undumpi_open(fname.c_str());
    if (profile == NULL) {
      spkt_throw(sprockit::io_error, "dumpiallocation::allocate: unable to open ", fname);
    }
    dumpi_header *header = dumpi_build_header();
    if (header == NULL) {
      spkt_throw(sprockit::io_error, "dumpi_allocation::allocate: header is null");
    }
    dumpi_read_header(profile, header);
    if (header->hostname == NULL || *header->hostname == 0) {
      spkt_throw(sprockit::io_error, "dumpi_allocation::allocate: hostname is null or empty");
    }

    if (header->meshdim == 0) {
      spkt_throw_printf(sprockit::input_error,
                       "dumpi_allocation::allocate: trace file %s contains no mesh info. No mesh coordinates found.\n"
                       "To run the trace you will need to use launch_allocation = hostname.\n"
                       "You will also need to give a hostname_map file giving the machine topology.\n"
                       "Alternatively, the trace file may just be corrupted.",
                       fname.c_str());
    }
    hw::coordinates coord_vec(header->meshdim);
    for (int i=0; i < header->meshdim; ++i) {
      coord_vec[i] = header->meshcrd[i];
    }
    node_id nid = regtop->node_addr(coord_vec);

    allocation.insert(nid);
    dumpi_free_header(header);
  }
}

}
}


