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

#include <sstmac/main/sstmac.h>

#include <iostream>
#include <sstream>
#include <iterator>
#include <stdint.h>
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <signal.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/native/manager.h>
#include <sstmac/software/process/app.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/util.h>


using namespace sstmac;

int
try_top_info_main(int argc, char **argv)
{
  sprockit::output::init_out0(&std::cout);
  sprockit::output::init_err0(&std::cerr);
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);

  //set up the search path
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_SRC_INCLUDE_PATH);
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);

  opts oo;
  int parse_status = parse_opts(argc, argv, oo);
  if (parse_status == PARSE_OPT_EXIT_SUCCESS) {
    return 0;
  }
  else if (parse_status == PARSE_OPT_EXIT_FAIL) {
    return 1;
  }

  if (oo.configfile == "") {
    oo.configfile = "parameters.ini"; //default
  }


  //use the config file to set up file search paths
  size_t pos = oo.configfile.find_last_of('/');
  if (pos != std::string::npos) {
    std::string dir = oo.configfile.substr(0, pos + 1);
    sprockit::SpktFileIO::add_path(dir);
  }

  sprockit::sim_parameters* params = new sprockit::sim_parameters(oo.configfile);
  sstmac::env::params = params;
  if (oo.params) {
    // there were command-line overrides
    oo.params->combine_into(params);
  }

  /** DO NOT CHANGE THE ORDER OF THE INIT FUNCTIONS BELOW - JJW
   *  they actually depend on each other */

  //if we have environmental variables that we need to map
  //to SST parameter names
  map_env_params(params);

  //do some cleanup and processing of params
  remap_params(params);

  //at this point, we have read in parameters - init malloc system
  //set the global parameters object
  sprockit::sprockit_init_cxx_heap(params);

  params->print_params();

  sprockit::sim_parameters* top_params = params->get_namespace("topology");
  hw::topology* thetop = hw::topology::factory::get_param("name", top_params);
  hw::cartesian_topology* top = test_cast(hw::cartesian_topology, thetop);

  std::cout << "Number of nodes:         " << top->num_nodes() << std::endl;
  std::cout << "Number of switches:      " << top->num_switches() << std::endl;

  if (top){
    while (1){
      std::string next;
      std::cout << "Next input: ";
      std::getline(std::cin, next);
      next = sprockit::trim_str(next);
      std::deque<std::string> tokens;
      pst::BasicStringTokenizer::tokenize(next, tokens);
      int nentry = tokens.size();
      if (nentry == 1){
        //this is a switch id - return coordinates
        switch_id sid(atoi(tokens[0].c_str()));
        hw::coordinates coords = top->switch_coords(sid);
        std::cout << "Switch ID maps to coordinates " << coords.to_string() << std::endl;
      } else if (nentry > 1){
        //these are coordinates, return node id
        hw::coordinates coords(nentry);
        for (int i=0; i < nentry; ++i){
          coords[i] = atoi(tokens[i].c_str());
        }
        switch_id nid = top->switch_addr(coords);
        std::cout << "Coordinates map to switch ID " << nid << std::endl;
      } else {
        std::cerr << "Invalid input" << std::endl;
      }
    }
  }


  delete params;

  sprockit::statics::finish();

  sprockit::sprockit_finalize_cxx_heap();

  return 0;
}

int
main(int argc, char **argv)
{
  int rc;
  try {
    rc = try_top_info_main(argc, argv);
  }
  catch (const std::exception &e) {
    std::cout.flush();
    std::cerr.flush();
    std::cerr << argv[0] << ": caught exception while setting up simulation:\n"
              << e.what() << "\n";
    return 1;
  }
  catch (...) {
    std::cerr << argv[0]
              << ": caught unknown exception while setting up simulation\n";
    return 1;
  }

  return rc;
}