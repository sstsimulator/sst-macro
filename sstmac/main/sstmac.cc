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
#include <sstmac/common/logger.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/backends/native/manager.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/time.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/sprockit/spkt_config.h>
#include <sumi/sumi/sumi_config.h>

#if SSTMAC_REPO_BUILD
#include <sstmac_repo.h>
#endif

#if SPKT_REPO_BUILD
#include <sprockit/sprockit_repo.h>
#endif

#if SUMI_REPO_BUILD
#include <sumi/sumi_repo.h>
#endif

namespace sstmac {

class runtime_param_bcaster :
  public sprockit::param_bcaster
{
 public:
  runtime_param_bcaster(parallel_runtime* rt) : rt_(rt) {}

  void
  bcast(void *buf, int size, int me, int root){
    rt_->bcast(buf, size, root);
  }

 private:
  parallel_runtime* rt_;
};

static void
print_finish(std::ostream& os, double wall_time)
{
#if SSTMAC_REPO_BUILD
  os << sprockit::printf("SSTMAC   repo:   %s\n", sstmac_REPO_HEADER);
#else
  os << sprockit::printf("SSTMAC   %s\n", SSTMAC_VERSION);
#endif
  os << sprockit::printf("SST/macro ran for %12.4f seconds\n", wall_time);
}

parallel_runtime*
init()
{
#if SSTMAC_INTEGRATED_SST_CORE
  spkt_throw(sprockit::unimplemented_error,
    "parallel_runtime* init: should not be called in integrated core");
#else
  //create a set of parameters from env variables
  //this is best way to piggy-back on process manager to configure things
  //in a parallel environment
  sprockit::sim_parameters cmdline_params;
  const char* env_str = getenv("SSTMAC_RUNTIME");
  if (env_str){
    cmdline_params["runtime"] = env_str;
  }
  else {
    cmdline_params["runtime"] = SSTMAC_DEFAULT_RUNTIME_STRING;
  }

  //used when using fake sst compilers in configure/test-suite
  //automatically uses a basic parameter file
  bool fake_build = false;
  env_str = getenv("SSTMAC_FAKE_BUILD");
  if (env_str){
    fake_build = atoi(env_str);
  }


  parallel_runtime* rt = parallel_runtime_factory::get_param("runtime", &cmdline_params);

  return rt;
#endif
}

void
finalize(parallel_runtime* rt)
{
  rt->finalize();
  sstmac::sw::operating_system::simulation_done();
  sprockit::statics::finish();
  sprockit::sprockit_finalize_cxx_heap();
}

void
init_opts(opts& oo, int argc, char** argv)
{
  int parse_status = parse_opts(argc, argv, oo);
  if (parse_status == PARSE_OPT_EXIT_SUCCESS) {
    exit(0);
  } else if (parse_status == PARSE_OPT_SUCCESS){
    //do nothing
  } else {
    spkt_throw(sprockit::value_error, "unknown error parsing command line options");
  }
}

/**
 * Read command line options and initialize the parameters
 * @param params  An already allocated parameter object
 */
void
init_params(parallel_runtime* rt, opts& oo, sprockit::sim_parameters* params, bool parallel)
{
  if (oo.configfile == "")
    oo.configfile = "parameters.ini";

  //use the config file to set up file search paths
  size_t pos = oo.configfile.find_last_of('/');
  if (pos != std::string::npos) {
    std::string dir = oo.configfile.substr(0, pos + 1);
    sprockit::SpktFileIO::add_path(dir);
  }

  if (parallel){
    runtime_param_bcaster bcaster(rt);
    sprockit::sim_parameters::parallel_build_params(params, rt->me(), rt->nproc(), oo.configfile, &bcaster);
  } else {
    params->parse_file(oo.configfile, false, true);
  }

  if (oo.params) {
    // there were command-line overrides
    oo.params->combine_into(params);
  }

  /** DO NOT CHANGE THE ORDER OF THE INIT FUNCTIONS BELOW - JJW
   *  they actually depend on each other */

  //if we have environmental variables that we need to map
  //to SST parameter names
  map_env_params(params);

  //at this point, we have read in parameters - init malloc system
  //set the global parameters object
  sprockit::sprockit_init_cxx_heap(params);
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
init_first_run(parallel_runtime* rt, sprockit::sim_parameters* params)
{
  std::string rank = sprockit::printf("%d", rt->me());
  std::string nproc = sprockit::printf("%d", rt->nproc());
  params->add_param_override("sst_rank", rank);
  params->add_param_override("sst_nproc", nproc);
  //and pass along to topology as well

  //we didn't have all the runtime params available when we first built this
  rt->init_runtime_params(params);

  std::string nworkers = sprockit::printf("%d", rt->nproc()*rt->nthread());
  //don't fail on existing, but ovewrite whatever is there
  params->parse_keyval("topology.nworkers", nworkers, false, true, true);
  rt->init_partition_params(params);
}

void
run_params(parallel_runtime* rt, sprockit::sim_parameters* params, sim_stats& stats)
{
  native::manager* mgr = new native::macro_manager(rt);
  mgr->init_factory_params(params);

  double start = sstmac_wall_time();
  timestamp stop_time = params->get_optional_time_param("stop_time", -1.);
  timestamp runtime;
  try {
    runtime = mgr->run(stop_time);

    std::cout.flush();
    std::cerr.flush();
    fflush(stdout);
    fflush(stderr);

    mgr->interconn()->deadlock_check();
    runtime::check_deadlock();

    mgr->finish();

    sstmac::env::params = 0;

    delete mgr;
  } // try
  catch (const std::exception& e) {
    if (mgr) {
      mgr->stop();
    }
    std::cout.flush();
    std::cerr.flush();
    throw;
  } // catch

  bool strict_params_test = params->get_optional_bool_param("strict_params", false);
  if (strict_params_test){
    bool unread_params = params->print_unread_params();
    if (unread_params)
      spkt_throw(sprockit::illformed_error, "simulation finished with unread parameters - abort");
  }

  // now that we finished running, print the parameters that we used

  double stop = sstmac_wall_time();
  stats.wallTime = stop - start;
  stats.simulatedTime = runtime.sec();
}

#endif

void
run(opts& oo,
  parallel_runtime* rt,
  sprockit::sim_parameters* params,
  sim_stats& stats,
  bool params_only = false)
{
  sstmac::env::params = params;
  sstmac::env::rt = rt;

  if (oo.debug != "") {
    logger::set_user_param(oo.debug);
  }

  int max_nproc = native::manager::compute_max_nproc(params);
  if (max_nproc == 0){
    params->pretty_print_params(std::cerr);
    spkt_throw(sprockit::value_error,
               "computed max nproc=0 from parameters");
  }
  resize_topology(max_nproc, params);
  if (params_only)
      return;

#if !SSTMAC_INTEGRATED_SST_CORE
  init_first_run(rt, params);
  run_params(rt, params, stats);
#endif
}

void
try_main(sprockit::sim_parameters* params, int argc, char **argv, bool params_only)
{
  //set up the search path
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_SRC_INCLUDE_PATH);

  sstmac::parallel_runtime* rt;
  if (params_only){
    rt = 0;
  } else {
    rt = sstmac::init();
  }

  opts oo;
  sim_stats stats;
  sstmac::init_opts(oo, argc, argv);
  bool parallel = rt && rt->nproc() > 1;
  sstmac::init_params(rt, oo, params, parallel);

  //do some cleanup and processing of params
  sstmac::process_init_params(params, true/*remap deprecated params*/);

  sstmac::run(oo, rt, params, stats, params_only);
  if (params_only){
    return;
  }


  if (oo.low_res_timer){
    cout0 << sprockit::printf("Estimated total runtime of %8.2f seconds\n", stats.simulatedTime);
  } else {
    cout0 << sprockit::printf("Estimated total runtime of %20.8f seconds\n", stats.simulatedTime);
  }

  if (oo.print_params) {
    params->print_params();
  }

  if (oo.print_walltime) {
    sstmac::print_finish(cout0, stats.wallTime);
  }

  sstmac::finalize(rt);
}


}





opts::~opts()
{
  if (params) delete params;
}

