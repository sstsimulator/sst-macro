/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2010 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
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
#include <sstmac/software/launch/app_launch.h>
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
  else if (oo.configfile == "") {
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
  hw::topology* thetop = hw::topology_factory::get_param("name", top_params);
  hw::cartesian_topology* top = test_cast(hw::cartesian_topology, thetop);

  std::cout << "Number of nodes:         " << top->num_nodes() << std::endl;
  std::cout << "Number of leaf switches: " << top->num_leaf_switches() << std::endl;
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



