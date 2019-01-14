/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sstmac/common/runtime.h>
#include <sstmac/hardware/common/flow.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/backends/native/manager.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/time.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/malloc.h>
#include <sprockit/spkt_new.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/sprockit/spkt_config.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/simple_node.h>

#if SSTMAC_REPO_BUILD
#include <sstmac_repo.h>
#endif

#if SPKT_REPO_BUILD
#include <sprockit/sprockit_repo.h>
#endif

RegisterKeywords(
 { "external_libs", "a list of external .so files to load" },
);

namespace sstmac {

class RuntimeParamBcaster :
  public sprockit::param_bcaster
{
 public:
  RuntimeParamBcaster(ParallelRuntime* rt) : rt_(rt) {}

  void bcast(void *buf, int size, int me, int root){
    rt_->bcast(buf, size, root);
  }

 private:
  ParallelRuntime* rt_;
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

ParallelRuntime* init()
{
#if SSTMAC_INTEGRATED_SST_CORE
  sprockit::abort("parallel_runtime* init: should not be called in integrated core");
  return nullptr;
#else
  //create a set of parameters from env variables
  //this is best way to piggy-back on process manager to configure things
  //in a parallel environment
  sprockit::sim_parameters cmdline_params;
  const char* env_str = getenv("SSTMAC_RUNTIME");
  if (env_str){
    cmdline_params["runtime"] = env_str;
  } else {
    cmdline_params["runtime"] = SSTMAC_DEFAULT_RUNTIME_STRING;
  }

  //used when using fake sst compilers in configure/test-suite
  //automatically uses a basic parameter file
  bool fake_build = false;
  env_str = getenv("SSTMAC_FAKE_BUILD");
  if (env_str){
    fake_build = atoi(env_str);
  }

  ParallelRuntime* rt = ParallelRuntime::staticRuntime(&cmdline_params);
  return rt;
#endif
}

void
finalize(ParallelRuntime* rt)
{
  rt->finalize();
  sstmac::sw::OperatingSystem::simulationDone();
  sprockit::statics::finish();
  sprockit::sprockit_finalize_cxx_heap();
  delete rt;
}

void
initOpts(opts& oo, int argc, char** argv)
{
  int parse_status = parseOpts(argc, argv, oo);
  if (parse_status == PARSE_OPT_EXIT_SUCCESS) {
    exit(0);
  } else if (parse_status == PARSE_OPT_SUCCESS){
    //do nothing
  } else {
    sprockit::abort("unknown error parsing command line options");
  }
}

/**
 * Read command line options and initialize the parameters
 * @param params  An already allocated parameter object
 */
void
initParams(ParallelRuntime* rt, opts& oo, sprockit::sim_parameters* params, bool parallel)
{
  //use the config file to set up file search paths
  size_t pos = oo.configfile.find_last_of('/');
  if (pos != std::string::npos) {
    std::string dir = oo.configfile.substr(0, pos + 1);
    sprockit::SpktFileIO::add_path(dir);
  }

  if (oo.got_config_file){
    if (parallel){
      RuntimeParamBcaster bcaster(rt);
      sprockit::sim_parameters::parallel_build_params(params, rt->me(), rt->nproc(),
                                                      oo.configfile, &bcaster, true);
    } else {
      if (oo.got_config_file) params->parse_file(oo.configfile, false, true);
    }
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

#if !SSTMAC_INTEGRATED_SST_CORE
  std::string rank = sprockit::printf("%d", rt->me());
  std::string nproc = sprockit::printf("%d", rt->nproc());
  params->add_param_override("sst_rank", rank);
  params->add_param_override("sst_nproc", nproc);
#endif
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
initFirstRun(ParallelRuntime* rt, sprockit::sim_parameters* params)
{
}

void
runParams(opts& oo,
           ParallelRuntime* rt,
           sprockit::sim_parameters* params,
           sim_stats& stats)
{
  //we didn't have all the runtime params available when we first built this
  rt->initRuntimeParams(params);

  rt->initPartitionParams(params);

  native::Manager* mgr = new native::Manager(params, rt);

  //dumping the output graph can be activated either on the command line
  //or activated by a parameter inside the topology
  //it is safe to call this function event if output_grapvhiz is empty
  //the topology will check and not dump if neither command line
  //nor parameter file has activated it
  mgr->interconnect()->topology()->outputGraphviz(oo.outputGraphviz);

  //same story applies for xyz file
  mgr->interconnect()->topology()->outputXYZ(oo.outputXYZ);

  double start = sstmacWallTime();
  Timestamp stop_time = params->get_optional_time_param("stop_time", 0);
  Timestamp runtime;
  try {
    runtime = mgr->run(stop_time);

    std::cout.flush();
    std::cerr.flush();
    fflush(stdout);
    fflush(stderr);

    //don't do this here anymore - interconn deleted by manager
    //mgr->interconnect()->deadlock_check();
    Runtime::checkDeadlock();

    mgr->finish();

    sstmac::Env::params = nullptr;

    delete mgr;
  } catch (const std::exception& e) {
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
      sprockit::abort("simulation finished with unread parameters - abort");
  }

  // now that we finished running, print the parameters that we used

  double stop = sstmacWallTime();
  stats.wallTime = stop - start;
  stats.simulatedTime = runtime.sec();

  sstmac::Runtime::deleteStatics();
}

#endif

void
run(opts& oo,
  ParallelRuntime* rt,
  sprockit::sim_parameters* params,
  sim_stats& stats)
{

  sstmac::Env::params = params;
  sstmac::Env::rt = rt;

#if !SSTMAC_INTEGRATED_SST_CORE
  runParams(oo, rt, params, stats);
#endif
}

static void tokenize(const std::string& in, std::set<std::string>& tokens){
  std::stringstream sstr(in);
  std::string item;
  while (std::getline(sstr, item, ',')){
    tokens.insert(item);
  }
}

int
runStandalone(int argc, char** argv)
{
  std::cerr << "WARNING: running standalone executable as-is. This usually happens\n"
            << "WARNING: when running configure scripts. I hope this is what you want"
            << std::endl;
  //oh, hmm, we are running inside configure
  //this means we actually just want to run a compiled program
  //and get the hell out of dodge
  sstmac::Timestamp::initStamps(1);
  sprockit::sim_parameters null_params;

  sprockit::sim_parameters* nic_params = null_params.get_optional_namespace("nic");
  nic_params->add_param_override("name", "null");

  sprockit::sim_parameters* mem_params = null_params.get_optional_namespace("memory");
  mem_params->add_param_override("name", "null");

  sprockit::sim_parameters* proc_params = null_params.get_optional_namespace("proc");
  proc_params->add_param_override("frequency", "1ghz");
  proc_params->add_param_override("ncores", 1);

  null_params.add_param_override("id", 1);
  null_params.add_param_override("name", "sstmac_app_name");
  sstmac::sw::SoftwareId id(0,0);
  sstmac::native::SerialRuntime rt(&null_params);
#if SSTMAC_INTEGRATED_SST_CORE
  sstmac::EventManager mgr(uint32_t(0));
#else
  sstmac::EventManager mgr(&null_params, &rt);
#endif
  sstmac::hw::SimpleNode node(&null_params, 1);
  sstmac::sw::OperatingSystem os(&null_params, &node);

  std::stringstream argv_sstr;
  for (int i=1; i < argc; ++i){
    argv_sstr << " " << argv[i];
  }
  null_params.add_param("argv", argv_sstr.str());

  null_params.add_param_override("notify", "false");
  sstmac::sw::App* a = sstmac::sw::App::factory::get_value(
        "sstmac_app_name", &null_params, id, &os);
  os.startApp(a, "");
  return a->rc();
}

int
tryMain(sprockit::sim_parameters* params,
         int argc, char **argv, bool params_only)
{
  //set up the search path
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);
  sprockit::SpktFileIO::add_path(SSTMAC_CONFIG_SRC_INCLUDE_PATH);

  sstmac::ParallelRuntime* rt;
  if (params_only){
    rt = nullptr;
  } else {
    rt = sstmac::init();
  }

  const char* list = getenv("SSTMAC_WHITELIST");
  if (list != nullptr){
    std::set<std::string> tokens;
    tokenize(list, tokens);
    std::string appName = argv[0];
    //normalize to chop off ./
    if (appName.at(0) == '.') appName.substr(2);
    auto lastSlash = appName.find_last_of("/");
    if (lastSlash != std::string::npos){
      appName = appName.substr(lastSlash + 1);
    }
    if (tokens.find(appName) != tokens.end()){
      //this executable is whitelisted
      return runStandalone(argc, argv);
    }
  }

  opts oo;
  sim_stats stats;
  sstmac::initOpts(oo, argc, argv);

  bool parallel = rt && rt->nproc() > 1;
  sstmac::initParams(rt, oo, params, parallel);

  if (!oo.benchmark.empty()){
    benchmark* bm = benchmark::factory::get_value(oo.benchmark, params);
    bm->run();
    return 0;
  }

  //do some cleanup and processing of params
  sstmac::remapParams(params);

  if (params->has_param("external_libs")){
    std::string pathStr = loadExternPathStr();
    std::vector<std::string> libraries;
    params->get_vector_param("external_libs", libraries);
    for (auto&& lib : libraries){
      loadExternLibrary(lib, pathStr);
    }
  }

  if (params_only)
    return 0;

#if !SSTMAC_INTEGRATED_SST_CORE
    if (rt && rt->me() == 0){
      cerr0 << std::string(argv[0]) << "\n" << oo << std::endl;
    }
#endif

  sstmac::run(oo, rt, params, stats);


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

  if (!oo.params_dump_file.empty()) {
    std::ofstream ofs(oo.params_dump_file.c_str());
    params->print_params(ofs);
    ofs.close();
  }

  sstmac::finalize(rt);

  return 0;
}


}





opts::~opts()
{
  if (params) delete params;
}
