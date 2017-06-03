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

#include <errno.h>
#include <getopt.h>
#include <sstmac/main/sstmac.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/debug.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <cstdlib>

void
activate_debugs(const std::string& debug_list)
{
  std::deque<std::string> tok;
  std::string space = ",";
  pst::BasicStringTokenizer::tokenize(debug_list, tok, space);
  for (auto& item : tok){
    sprockit::debug::turn_on(item);
  }
}

std::ostream&
operator<<(std::ostream &os, const opts &oo)
{
  if (oo.help) {
    os << "  --help \\\n";
  }
  os << "  --debug=\"" << oo.debug << "\" \\\n";
  os << "  --configfile=\"" << oo.configfile << "\" \\\n";
  return os;
}

void
machine_already_configured(){
  spkt_abort_printf("conflicting machine declarations: cannot combine"
    "--inf, --auto/-a, --debug/-d, --pisces, or --macrels flags");
}

int
parse_opts(int argc, char **argv, opts &oo)
{
  int no_congestion = 0;
  int pisces_debug = 0;
  int macrels_debug = 0;
  int dompitest = 0;
  int printnodes = 0;
  int no_wall_time = 0;
  int print_params = 0;
  int debugflags = 0;
  int dodumpi = 0;
  int lowrestimer = 0;
  int run_ping_all = 0;
  int infinite_network = 0;
  bool need_config_file = true;
  bool machine_configured = false;
  option gopt[] = {
    { "help", no_argument, NULL, 'h' },
    { "include", required_argument, NULL, 'i' },
    { "debug", required_argument, NULL, 'd' },
    { "inf", no_argument, &infinite_network, 1},
    { "configfile", required_argument, NULL, 'f' },
    { "auto", no_argument, NULL, 'a' },
    { "app", required_argument, NULL, 'A' },
    { "nproc", required_argument, NULL, 'n' },
    { "param", required_argument, NULL, 'p' },
    { "srun", required_argument, NULL, 's' },
    { "dumpi", no_argument, &dodumpi, 1 },
    { "debug-flags", no_argument, &debugflags, 1},
    { "mpitest", no_argument, &dompitest, 1 },
    { "print-nodes", no_argument, &printnodes, 1 },
    { "pisces", no_argument, &pisces_debug, 1 },
    { "macrels", no_argument, &macrels_debug, 1 },
    { "ping-all", no_argument, &run_ping_all, 1 },
    { "no-congestion", no_argument, &no_congestion, 1 },
    { "low-res-timer", no_argument, &lowrestimer, 1 },
    { "print-params", no_argument, &print_params, 1 },
    { "no-wall-time", no_argument, &no_wall_time, 1 },
    { "cpu-affinity", required_argument, NULL, 'c' },
    { NULL, 0, NULL, '\0' }
  };
  int ch;
  bool errorflag = false;
  std::list<std::pair<std::string, std::string> > paramlist;
  oo.params = new sprockit::sim_parameters;
  optind = 1;
  while ((ch = getopt_long(argc, argv, "Phad:f:t:p:m:n:u:i:c:", gopt, NULL))
         != -1) {
    switch (ch) {
      case 0:
        //this set an input flag
        break;
      case 'h':
        oo.help = 1;
        break;
      case 'd':
        activate_debugs(optarg);
        break;
      case 'n' :
        oo.params->add_param_override("node.app1.launch_cmd", sprockit::printf("aprun -n %s -N 1", optarg));
        break;
      case 'A' :
        oo.params->add_param_override("node.app1.name", optarg);
        break;
      case 'f':
        oo.configfile = optarg;
        oo.got_config_file = true;
        break;
      case 'a': {
        need_config_file = false;
        sprockit::sim_parameters params("debug.ini");
        params.combine_into(oo.params);
        machine_configured = true;
        break;
      }
      case 'i': {
        sprockit::sim_parameters params(optarg);
        params.combine_into(oo.params);
      }
      break;
      case 'p': {
        //overwrite anything existing
        oo.params->parse_line(optarg, false, true);
        break;
      }
      case 'c': {
        //overwrite anything existing
        std::string param_line = std::string("cpu_affinity = ") + std::string(optarg);
        oo.params->parse_line(param_line, false, true);
        break;
      }
      default:
        cerr0 << "Unhandled input flag" << std::endl;
        errorflag = true;
        break;
    }
    if (oo.help) {
      print_help(argc, argv);
      return PARSE_OPT_EXIT_SUCCESS;
    }
    if (errorflag) {
      return PARSE_OPT_EXIT_FAIL;
    }
  }

  if (infinite_network) {
    sprockit::sim_parameters params("infinite.ini");
    params.combine_into(oo.params);
    need_config_file = false;
    if (machine_configured){
      machine_already_configured();
    }
    machine_configured = true;
  }

  if (dodumpi) {
    if (!machine_configured){
      sprockit::sim_parameters params("debug.ini");
      params.combine_into(oo.params);
      machine_configured = true;
    }
    need_config_file = false;
    oo.params->add_param("node.app1.name", "parsedumpi");
  }

  if (oo.configfile == "" && need_config_file){
    spkt_abort_printf("need to specify input file with -f flag");
  }

  if (print_params) {
    oo.print_params = true;
  }

  if (no_congestion) {
    oo.params->add_param("switch.arbitrator", "null");
  }

  if (pisces_debug) {
    if (machine_configured){
      machine_already_configured();
    }
    oo.configfile = "pisces.ini";
  }

  if (macrels_debug) {
    if (machine_configured){
      machine_already_configured();
    }
    oo.configfile = "macrels.ini";
  }

  if (run_ping_all){
    oo.params->add_param("node.app1.name", "mpi_ping_all");
  }

  if (debugflags){
    sprockit::debug::print_all_debug_slots(std::cout);
    return PARSE_OPT_EXIT_SUCCESS;
  }
  /** check to see if we should do an mpitestall */
  if (dompitest) {
    if (oo.configfile != "") {
      cerr0 << "Cannot set config file with --configfile and also request --mpitest" << std::endl;
      return PARSE_OPT_EXIT_FAIL;
    }
    oo.configfile = "mpi_test_all.ini";
  }
  if (printnodes) {
    oo.params->add_param_override("node.app1.name", "mpi_printnodes");
  }

  if (lowrestimer){
    oo.low_res_timer = true;
  }

  /** double negative, sorry about that */
  oo.print_walltime = !no_wall_time;


  return PARSE_OPT_SUCCESS;
}
