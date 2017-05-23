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
  bool got_config_file;
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
    got_config_file(false),
    low_res_timer(false),
    print_walltime(true),
    print_params(false),
    cpu_affinity("") {
  }

  ~opts();
};

std::ostream&
operator<<(std::ostream &os, const opts &oo);

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
resize_topology(int max_nproc, sprockit::sim_parameters* params, bool verbose = true);

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
remap_params(sprockit::sim_parameters* params, bool verbose = true);

void
run(opts& oo,
    sstmac::parallel_runtime* rt,
    sprockit::sim_parameters* params,
    sim_stats& stats);

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