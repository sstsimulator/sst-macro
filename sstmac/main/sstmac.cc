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
#include <sstmac/software/launch/task_mapping.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/topology.h>
#include <sprockit/fileio.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/keyword_registration.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/node/simple_node.h>

#if SSTMAC_REPO_BUILD
#include <sstmac_repo.h>
#endif

RegisterKeywords(
 { "external_libs", "a list of external .so files to load" },
);

namespace sstmac {

class RuntimeParamBcaster :
  public sprockit::ParamBcaster
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
  SST::Params cmdline_params;
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

  ParallelRuntime* rt = ParallelRuntime::staticRuntime(cmdline_params);
  return rt;
#endif
}

void
finalize(ParallelRuntime* rt)
{
  rt->finalize();
  sstmac::sw::OperatingSystem::simulationDone();
  sprockit::Statics::finish();
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
initParams(ParallelRuntime* rt, opts& oo, sprockit::SimParameters::ptr params, bool parallel)
{
  //use the config file to set up file search paths
  size_t pos = oo.configfile.find_last_of('/');
  if (pos != std::string::npos) {
    std::string dir = oo.configfile.substr(0, pos + 1);
    sprockit::SpktFileIO::addPath(dir);
  }

  if (oo.got_config_file){
    params->parseFile(oo.configfile, false, true);
  }


  if (oo.params) {
    // there were command-line overrides
    oo.params->combineInto(params);
  }

  /** DO NOT CHANGE THE ORDER OF THE INIT FUNCTIONS BELOW - JJW
   *  they actually depend on each other */


#if !SSTMAC_INTEGRATED_SST_CORE
  std::string rank = sprockit::printf("%d", rt->me());
  std::string nproc = sprockit::printf("%d", rt->nproc());
  params->addParamOverride("sst_rank", rank);
  params->addParamOverride("sst_nproc", nproc);
#endif
}

#if !SSTMAC_INTEGRATED_SST_CORE
void
initFirstRun(ParallelRuntime* rt, SST::Params& params)
{
}

void
runParams(opts& oo,
           ParallelRuntime* rt,
           SST::Params& params,
           SimStats& stats)
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
  Timestamp stop_time(params.find<SST::UnitAlgebra>("stop_time", "0s").getValue().toDouble());
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

    delete mgr;
  } catch (const std::exception& e) {
    if (mgr) {
      mgr->stop();
    }
    std::cout.flush();
    std::cerr.flush();
    throw;
  } // catch

  double stop = sstmacWallTime();
  stats.wallTime = stop - start;
  stats.simulatedTime = runtime.sec();

  sstmac::Runtime::deleteStatics();
}

#endif

void
run(opts& oo,
  ParallelRuntime* rt,
  SST::Params& params,
  SimStats& stats)
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
tryMain(sprockit::SimParameters::ptr params,
        int argc, char **argv, bool params_only)
{
  //set up the search path
  sprockit::SpktFileIO::addPath(SSTMAC_CONFIG_INSTALL_INCLUDE_PATH);
  sprockit::SpktFileIO::addPath(SSTMAC_CONFIG_SRC_INCLUDE_PATH);

  sstmac::ParallelRuntime* rt;
  if (params_only){
    rt = nullptr;
  } else {
    rt = sstmac::init();
  }

  opts oo;
  SimStats stats;
  sstmac::initOpts(oo, argc, argv);

  bool parallel = rt && rt->nproc() > 1;
  sstmac::initParams(rt, oo, params, parallel);

  //do some cleanup and processing of params
  sstmac::remapParams(params);

  if (params->hasParam("external_libs")){
    std::string pathStr = loadExternPathStr();
    std::vector<std::string> libraries;
    params->getVectorParam("external_libs", libraries);
    for (auto&& lib : libraries){
      loadExternLibrary(lib, pathStr);
    }
  }

  if (params_only)
    return 0;

#if SSTMAC_INTEGRATED_SST_CORE
#else
  SST::Params mainParams(params);
  if (!oo.benchmark.empty()){
    Benchmark* bm = Benchmark::getBuilderLibrary("macro")
        ->getBuilder(oo.benchmark)->create();
    bm->run();
    return 0;
  }

  if (rt && rt->me() == 0){
    cerr0 << std::string(argv[0]) << "\n" << oo << std::endl;
  }
  sstmac::run(oo, rt, mainParams, stats);


  if (oo.low_res_timer){
    cout0 << sprockit::printf("Estimated total runtime of %8.2f seconds\n", stats.simulatedTime);
  } else {
    cout0 << sprockit::printf("Estimated total runtime of %20.8f seconds\n", stats.simulatedTime);
  }

  if (oo.print_params) {
    params->printParams();
  }

  if (oo.print_walltime) {
    sstmac::print_finish(cout0, stats.wallTime);
  }

  if (!oo.params_dump_file.empty()) {
    std::ofstream ofs(oo.params_dump_file.c_str());
    params->printParams(ofs);
    ofs.close();
  }

  sstmac::finalize(rt);
#endif
  if (oo.use_app_rc){
    return sstmac::sw::App::appRC();
  } else {
    return 0;
  }
}


}

opts::~opts()
{
}
