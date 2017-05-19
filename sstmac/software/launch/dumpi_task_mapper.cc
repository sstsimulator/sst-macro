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

#include <fstream>
#include <sstream>

#include <sstmac/dumpi_util/dumpi_util.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/software/launch/dumpi_task_mapper.h>
#include <sstmac/software/launch/hostname_allocation.h>
#include <sstmac/common/runtime.h>
#include <dumpi/libundumpi/libundumpi.h>
#include <sprockit/errors.h>
#include <sprockit/util.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

node_id
dumpi_task_mapper::node_id_from_hostname(const std::string& hostname)
{
  auto aptr_it = hostname_allocation::hostnamemap_.find(hostname);
  auto end = hostname_allocation::hostnamemap_.end();

  if (aptr_it == end) {
    std::stringstream sstr;
    sstr << hostname << " from dumpi file does not exist in node map.";

    auto it = hostname_allocation::hostnamemap_.begin();

    if (it == end) {
      sstr << " No hostnames are registered with the allocator."
           " DUMPI traces do not contain topology information."
           " You must use launch_allocation=hostname and provide a hostname map.";
    }
    else {
      sstr << std::endl << "Valid hostnames are: ";
      for ( ; it != end; ++it) {
        sstr << std::endl << it->first;
      }
      sstr << std::endl << std::endl
           << "Are you sure the dumpi file and node map"
           " are from the same machine?";
    }
    spkt_throw(sprockit::input_error, sstr.str());
  }
  return aptr_it->second;
}

node_id
dumpi_task_mapper::node_id_from_coordinates(int ncoord, int *coords)
{
  hw::coordinates coord_vec(ncoord);
  for (int i=0; i < ncoord; ++i) {
    coord_vec[i] = coords[i];
  }
  return regtop_->node_addr(coord_vec);
}

dumpi_task_mapper::dumpi_task_mapper(sprockit::sim_parameters *params) :
  task_mapper(params)
{
  metaname_ = params->get_param("dumpi_metaname");
  regtop_ = safe_cast(hw::cartesian_topology, topology_);
}

void
dumpi_task_mapper::map_ranks(
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  dumpi_meta* meta = new dumpi_meta(metaname_);

  int nrank = getnumprocs(meta);
  result.resize(nrank);
  cout0 << "nrank: " << nrank << std::endl;

  for (int i = 0; i < nrank; i++) {
    std::string fname = dumpi_file_name(i, meta->dirplusfileprefix_);
    dumpi_profile *profile = undumpi_open(fname.c_str());
    if (profile == NULL) {
      spkt_throw(sprockit::io_error, "dumpiindexer::allocate: unable to open ", fname);
    }
    dumpi_header *header = dumpi_build_header();
    if (header == NULL) {
      spkt_throw(sprockit::io_error, "dumpiindexer::allocate: header is null");
    }
    dumpi_read_header(profile, header);
    if (header->hostname == NULL || *header->hostname == 0) {
      spkt_throw(sprockit::io_error, "dumpiindexer::allocate: hostname is null or empty");
    }

    std::string hostname(header->hostname);
    node_id nid;
    if (header->meshdim == 0) {
      nid = node_id_from_hostname(hostname);
    }
    else {
      nid = node_id_from_coordinates(header->meshdim, header->meshcrd);
    }

    dumpi_free_header(header);

    debug_printf(sprockit::dbg::indexing,
        "dumpi_task_mapper: rank %d is on hostname %s at nid=%d",
        i, hostname.c_str(), int(nid));

    result[i] = nid;
  }
}

}
}