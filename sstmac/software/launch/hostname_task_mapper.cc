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

#include <fstream>
#include <sstream>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/software/launch/hostname_task_mapper.h>
#include <sstmac/software/launch/hostname_allocation.h>
#include <sstmac/software/launch/launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/common/runtime.h>
#include <sprockit/fileio.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("hostname", task_mapper, hostname_task_mapper,
            "assigns tasks to nodes based on hostname map of topology and hostname list in file");

void
hostname_task_mapper::init_factory_params(sprockit::sim_parameters *params)
{
    task_mapper::init_factory_params(params);
  listfile_ = params->get_param("launch_hostname_list");
}

void
hostname_task_mapper::map_ranks(
  const app_id& aid,
  const ordered_node_set& nodes,
  int ppn,
  std::vector<node_id> &result,
  int nproc)
{
  int nrank = nproc;
  result.resize(nrank);

  std::istream* nodelistptr = rt_->bcast_file_stream(listfile_);
  std::istream& nodelist = *nodelistptr;

  std::stringstream sstr;
  for (int i = 0; i < nrank; i++) {
    std::string hostname;
    nodelist >> hostname;

    sstr << hostname << "\n";

    hostname_allocation::nodemap_t::const_iterator
        nid_it = hostname_allocation::hostnamemap_.find(hostname),
    end = hostname_allocation::hostnamemap_.end();

    if (nid_it == end) {
      std::stringstream sstr;
      sstr << hostname << " from file " << listfile_ <<
           " does not exist in node map.";

      hostname_allocation::nodemap_t::const_iterator
      it = hostname_allocation::hostnamemap_.begin();

      if (it == end) {
        sstr << " No hostnames are registered with hostname_allocation."
             << " This is perhaps not surprising then."
             << " Maybe check that launch_allocation is set to hostname.";
      }
      else {
        sstr << std::endl << "Valid hostnames are: ";
        for ( ; it != end; ++it) {
          sstr << std::endl << it->first;
        }
        sstr << std::endl << std::endl
             << "Are you sure the hostname file and node map"
             " are from the same machine?";
      }
      spkt_throw(sprockit::input_error, sstr.str());
    }

    node_id nid = nid_it->second;

    debug_printf(sprockit::dbg::indexing,
        "hostname_task_mapper: rank %d is on hostname %s at nid=%d",
        i, hostname.c_str(), int(nid));

    result[i] = nid;

  }

  delete nodelistptr;
}
}
}

