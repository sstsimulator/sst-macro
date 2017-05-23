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

#include <sstmac/common/runtime.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/launch/launch_request.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>
#include <sprockit/stl_string.h>
#include <sprockit/basic_string_tokenizer.h>
#include <unistd.h>
#include <getopt.h>


RegisterKeywords(
"launch_cmd",
"allocation",
"indexing",
"start",
"size",
"launch_allocation",
"launch_indexing",
);

namespace sstmac {
namespace sw {

software_launch_request::software_launch_request(sprockit::sim_parameters *params) :
  indexed_(false),
  num_finished_(0)
{
  if (params->has_param("core_affinities")) {
    params->get_vector_param("core_affinities", core_affinities_);
  }

  time_ = params->get_optional_time_param("start", 0);

  if (params->has_param("launch_cmd")){
    parse_launch_cmd(params);
  } else if (params->has_param("dumpi_metaname")){
    std::string metafile = params->get_param("dumpi_metaname");
    sw::dumpi_meta dm(metafile);
    nproc_ = dm.num_procs();
  } else {
    nproc_ = params->get_int_param("size");
    procs_per_node_ = params->get_optional_int_param("tasks_per_node", 1);
  }

  allocator_ = sw::node_allocator::factory
                ::get_optional_param("allocation", "first_available", params);

  indexer_ = sw::task_mapper::factory
                ::get_optional_param("indexing", "block", params);
}

software_launch_request::~software_launch_request()
{
  delete allocator_;
  delete indexer_;
}

app_launch_request::~app_launch_request()
{
}

app_launch_request::app_launch_request(sprockit::sim_parameters* params,
                       app_id aid,
                       const std::string& app_namespace) :
  software_launch_request(params),
  aid_(aid),
  app_namespace_(app_namespace),
  app_params_(params)
{
}

void
software_launch_request::index_allocation(
  hw::topology* top,
  const ordered_node_set& allocation,
  std::vector<node_id>& rank_to_node_indexing,
  std::vector<std::list<int>>& node_to_rank_indexing)
{
  indexer_->map_ranks(allocation,
               procs_per_node_,
               rank_to_node_indexing,
               nproc_);

  if (sprockit::debug::slot_active(sprockit::dbg::indexing)){
    cout0 << sprockit::printf("Allocated and indexed %d nodes\n",
                rank_to_node_indexing.size());
    int num_nodes = rank_to_node_indexing.size();

    hw::cartesian_topology* regtop =
        test_cast(hw::cartesian_topology, top);

    for (int i=0; i < num_nodes; ++i){
      node_id nid = rank_to_node_indexing[i];
      if (regtop){
        hw::coordinates coords = regtop->node_coords(nid);
        cout0 << sprockit::printf("Rank %d -> nid%d %s\n",
            i, int(nid), stl_string(coords).c_str());
      } else {
         cout0 << sprockit::printf("Rank %d -> nid%d\n", i, int(nid));
      }
    }

  }

  int num_nodes = top->num_nodes();
  node_to_rank_indexing.resize(num_nodes);
  int num_ranks = rank_to_node_indexing.size();
  for (int i=0; i < num_ranks; ++i){
    node_id nid = rank_to_node_indexing[i];
    node_to_rank_indexing[nid].push_back(i);
  }

  indexed_ = true;
}

void
software_launch_request::request_allocation(
  const sw::ordered_node_set& available,
  sw::ordered_node_set& allocation)
{
  int num_nodes = nproc_ / procs_per_node_;
  int remainder = nproc_ % procs_per_node_;
  if (remainder) {
    ++num_nodes;
  }
  allocator_->allocate(num_nodes, available, allocation);
}

void
software_launch_request::parse_launch_cmd(
  sprockit::sim_parameters* params,
  int& nproc,
  int& procs_per_node,
  std::vector<int>& affinities)
{
  if (params->has_param("launch_cmd")) {
    /** Check for an aprun launch */
    std::string launch_cmd = params->get_param("launch_cmd");
    size_t pos = launch_cmd.find_first_of(' ');
    std::string launcher;
    if (pos != std::string::npos) {
      launcher = launch_cmd.substr(0, pos);
    }
    else {
      launcher = launch_cmd;
    }

    if (launcher == "aprun") {
      parse_aprun(launch_cmd, nproc, procs_per_node, affinities);
      if (procs_per_node == -1) { //nothing given
        int ncores = params->get_optional_int_param("node_cores", 1);
        procs_per_node = ncores > nproc ? nproc : ncores;
      }
    }
    else {
      spkt_throw_printf(sprockit::value_error,
                        "invalid launcher %s given", launcher.c_str());
    }
  }
  else { //standard launch
    try {
      nproc = params->get_long_param("size");
      procs_per_node = params->get_optional_long_param("concentration", 1);
    }
    catch (sprockit::input_error& e) {
      cerr0 << "Problem reading app size parameter in app_launch_request.\n"
               "If this is a DUMPI trace, set app name to dumpi.\n";
      throw e;
    }
  }
}

void
software_launch_request::parse_launch_cmd(sprockit::sim_parameters* params)
{
  parse_launch_cmd(params, nproc_, procs_per_node_, core_affinities_);
}

void
software_launch_request::parse_aprun(
  const std::string &cmd,
  int &nproc, int &nproc_per_node,
  std::vector<int>& core_affinities)
{
  int aprun_numa_containment = 0;
  option aprun_gopt[] = {
    { "n", required_argument, NULL, 'n' },
    { "N", required_argument, NULL, 'N' },
    { "ss", no_argument, &aprun_numa_containment, 1 },
    { "cc", required_argument, NULL, 'C' },
    { "S", required_argument, NULL, 'S' },
    { "d", required_argument, NULL, 'd' },
    { NULL, 0, NULL, '\0' }
  };

  char cmdline_str[200];
  ::strcpy(cmdline_str, cmd.c_str());
  char *argv[50];
  int argc = 0;
  char* pch = strtok(cmdline_str, " ");
  while (pch != 0) {
    argv[argc] = pch;
    ++argc;
    pch = strtok(0, " ");
  }

  int _nproc = -1;
  int _nproc_per_node = -1;
  int ch;
  int option_index = 1;
  std::string core_aff_str;
  optind = 1;
  while ((ch = getopt_long_only(argc, argv, "n:N:S:d:", aprun_gopt,
                                &option_index)) != -1) {
    switch (ch) {
      case 0: //this set an input flag
        break;
      case 'n':
        _nproc = atoi(optarg);
        break;
      case 'N':
        _nproc_per_node = atoi(optarg);
        break;
      case 'S':
        break;
      case 'C':
        core_aff_str = optarg;
        break;
      default:
        break;
    }
  }
  if (_nproc <= 0)
    throw sprockit::input_error(
      "aprun allocator did not receive a valid -n specification");

  if (_nproc_per_node == 0 || _nproc_per_node > _nproc)
    throw sprockit::input_error(
      "aprun allocator did not receive a valid -N specification");



  if (core_aff_str.size() > 0) { //we got a core affinity spec
    if (!core_affinities.empty()) {
      spkt_throw_printf(sprockit::illformed_error,
                       "app_launch_request::parse_aprun: core affinities already assigned, cannot use -cc option");
    }

    std::string tosep = core_aff_str;
    std::deque<std::string> tok;
    std::string space = ",";
    pst::BasicStringTokenizer::tokenize(tosep, tok, space);
    for (auto& core : tok){
      core_affinities.push_back(atoi(core.c_str()));
    }
  }

  nproc = _nproc;
  nproc_per_node = _nproc_per_node;
}



}
}