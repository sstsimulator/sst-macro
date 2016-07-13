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

#ifndef SSTMAC_SSTMAC_H_INCLUDED
#define SSTMAC_SSTMAC_H_INCLUDED

#include <sprockit/sim_parameters_fwd.h>
#include <sstmac/backends/native/manager_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <string>

#define PARSE_OPT_SUCCESS 0
#define PARSE_OPT_EXIT_SUCCESS 1
#define PARSE_OPT_EXIT_FAIL 2

struct opts {
  int help;
  std::string debug;
  std::string configfile;
  sprockit::sim_parameters* params;
  bool print_walltime;
  bool print_params;
  bool low_res_timer;
  std::string cpu_affinity;

  opts() :
    help(0),
    debug(""),
    params(0),
    configfile(""),
    low_res_timer(false),
    print_walltime(true),
    print_params(false),
    cpu_affinity("") {
  }

  ~opts();
};

struct sim_stats {
  double wallTime;
  double simulatedTime;
  int numResults;
  sim_stats() : 
    wallTime(0), 
    simulatedTime(0), 
    numResults(-1) 
  {}
};

int
parse_opts(int argc, char **argv, opts &oo);

void
print_help(int argc, char **argv);


void
resize_topology(int max_nproc, sprockit::sim_parameters* params);

void
map_env_params(sprockit::sim_parameters* params);

namespace sstmac {

parallel_runtime* init();

void finalize(parallel_runtime* rt);

void init_opts(opts& oo, int argc, char** argv);

void
init_params(parallel_runtime* rt, opts& oo, sprockit::sim_parameters* params, bool parallel);

void
remap_deprecated_params(sprockit::sim_parameters* params);

void
process_init_params(sprockit::sim_parameters* params, bool remap_params);

void
run(opts& oo,
    sstmac::parallel_runtime* rt,
    sprockit::sim_parameters* params,
    sim_stats& stats,
    bool params_only);

void
try_main(sprockit::sim_parameters* params,
   int argc, char **argv,
   bool params_only = false);

void
run_params(parallel_runtime* rt,
  sprockit::sim_parameters* params,
   sim_stats& stats);

void
init_first_run(parallel_runtime* rt,
    sprockit::sim_parameters* params);

}

#endif

