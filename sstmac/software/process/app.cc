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

#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <inttypes.h>

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/common/thread_lock.h>
#include <dlfcn.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/software/api/api.h>
#include <sstmac/main/sstmac.h>

static sprockit::need_deleteStatics<sstmac::sw::UserAppCxxFullMain> del_app_statics;

RegisterKeywords(
 { "host_compute_timer", "whether to use the time elapsed on the host machine in compute modeling" },
 { "min_op_cutoff", "the minimum number of operations in a compute before detailed modeling is perfromed" },
 { "notify", "whether the app should send completion notifications to job root" },
 { "globals_size", "the size of the global variable segment to allocate" },
 { "OMP_NUM_THREADS", "environment variable for configuring openmp" },
 { "exe", "an optional exe .so file to load for this app" },
);

MakeDebugSlot(app_compute);

void sstmac_app_loaded(int aid){}

namespace sstmac {
namespace sw {

std::map<std::string, App::main_fxn>*
  UserAppCxxFullMain::main_fxns_ = nullptr;
std::map<std::string, App::empty_main_fxn>*
  UserAppCxxEmptyMain::empty_main_fxns_ = nullptr;
std::map<AppId, UserAppCxxFullMain::argv_entry> UserAppCxxFullMain::argv_map_;

std::map<int, App::dlopen_entry> App::dlopens_;

int
App::allocateTlsKey(destructor_fxn fxn)
{
  int next = next_tls_key_;
  tls_key_fxns_[next] = fxn;
  ++next_tls_key_;
  return next;
}

static char* get_data_segment(SST::Params& params,
                              const char* param_name, GlobalVariableContext& ctx)
{
  int allocSize = ctx.allocSize();
  if (params->has_param(param_name)){
    allocSize = params.find<int>(param_name);
    if (ctx.allocSize() != allocSize){
      ctx.setAllocSize(allocSize);
    }
  }
  if (allocSize != 0){
    char* segment = new char[allocSize];
    ::memcpy(segment, ctx.globalInit(), ctx.globalsSize());
    return segment;
  } else {
    return nullptr;
  }
}


static thread_lock dlopen_lock;

void
App::dlopenCheck(int aid, SST::Params& params)
{
  if (params->has_param("exe")){
    dlopen_lock.lock();
    std::string libname = params.find<std::string>("exe");
    dlopen_entry& entry = dlopens_[aid];
    entry.name = libname;
    if (entry.refcount == 0){
      entry.handle = loadExternLibrary(libname, loadExternPathStr());
    }
    void* name = dlsym(entry.handle, "exe_main_name");
    if (name){
      const char* str_name = (const char*) name;
      if (params->has_param("name")){
        if (params.find<std::string>("name") != std::string(str_name)){
          spkt_abort_printf("if given both exe= and name= parameters for app%d, they must agree\n"
                            "%s != %s", aid, str_name, params.find<std::string>("name").c_str());
        }
      } else {
        params->add_param("name", str_name);
      }
    }
    ++entry.refcount;
    sstmac_app_loaded(aid);
    dlopen_lock.unlock();
  }
}


void
App::dlcloseCheck(int aid)
{
  dlopen_lock.lock();
  auto iter = dlopens_.find(aid);
  if (iter != dlopens_.end()){
    dlopen_entry& entry = iter->second;
    --entry.refcount;
    if (entry.refcount == 0){
      std::cerr << "Unloading library " << entry.name << std::endl;
      unloadExternLibrary(entry.handle);
      dlopens_.erase(iter);
    }
  }
  dlopen_lock.unlock();
}

char*
App::allocateDataSegment(bool tls)
{
  if (tls){
    return get_data_segment(params_, "tls_size", GlobalVariable::tlsCtx);
  } else {
    return get_data_segment(params_, "globals_size", GlobalVariable::glblCtx);
  }
}

App::App(SST::Params& params, SoftwareId sid,
         OperatingSystem* os) :
  Thread(params, sid, os),
  compute_lib_(nullptr),
  params_(params),
  next_tls_key_(0),
  next_condition_(0),
  notify_(true),
  next_mutex_(0),
  min_op_cutoff_(0),
  globals_storage_(nullptr),
  rc_(0)
{
  if (!params_){
    spkt_abort_printf("app got null parameters");
  }

  globals_storage_ = allocateDataSegment(false); //not tls
  min_op_cutoff_ = params.find<long>("min_op_cutoff", 1000);
  bool host_compute = params.find<bool>("host_compute_timer", false);
  if (host_compute){
    host_timer_ = new HostTimer;
  }

  notify_ = params.find<bool>("notify", true);

  SST::Params env_params = params.find_prefix_params("env");
  omp_contexts_.emplace_back();
  omp_context& active = omp_contexts_.back();
  active.max_num_subthreads = active.requested_num_subthreads =
    env_params.find<int>("OMP_NUM_THREADS", 1);
  active.level = 0;
  active.num_threads = 1;

  for (auto iter=env_params->begin(); iter != env_params->end(); ++iter){
    env_[iter->first] = iter->second.value;
  }

  for (auto iter=os->env_begin(); iter != os->env_end(); ++iter){
    auto my_iter = env_.find(iter->first);
    if (my_iter == env_.end()){
      //don't overwrite - app env taks precedence
      env_[iter->first] = iter->second;
    }
  }
}

App::~App()
{
  /** These get deleted by unregister */
  //sprockit::delete_vals(apis_);
  if (compute_lib_) delete compute_lib_;
  if (globals_storage_) delete[] globals_storage_;
}

int
App::putenv(char* input)
{
  spkt_abort_printf("app::putenv: not implemented - cannot put %d",
                    input);
  return 0;
}

int
App::setenv(const std::string &name, const std::string &value, int overwrite)
{
  if (overwrite){
    env_[name] = value;
  } else {
    auto iter = env_.find(name);
    if (iter == env_.end()){
      env_[name] = value;
    }
  }
  return 0;
}

char*
App::getenv(const std::string &name) const
{
  char* my_buf = const_cast<char*>(env_string_);
  auto iter = env_.find(name);
  if (iter == env_.end()){
    return nullptr;
  } else {
    auto& val = iter->second;
    if (val.size() >= sizeof(env_string_)){
      spkt_abort_printf("Environment variable %s=%s is too long - need less than %d",
                        name.c_str(), val.c_str(), int(val.size()));
    }
    ::strcpy(my_buf, val.data());
  }
  //ugly but necessary
  return my_buf;
}

LibComputeMemmove*
App::computeLib()
{
  if(!compute_lib_) {
    compute_lib_ = new LibComputeMemmove(params_, sid_, os_);
  }
  return compute_lib_;
}

void
App::deleteStatics()
{
}

void
App::cleanup()
{
  //okay, the app is dying
  //it may be that we have subthreads that are still active
  //all of these subthreads must be cancelled and never start again
  for (auto& pair : subthreads_){
    pair.second->cancel();
  }
  subthreads_.clear();

  Thread::cleanup();
}

void
App::sleep(Timestamp time)
{
  computeLib()->sleep(time);
}

void
App::compute(Timestamp time)
{
  computeLib()->compute(time);
}

void
App::computeInst(ComputeEvent* cmsg)
{
  computeLib()->computeInst(cmsg);
}

void
App::computeLoop(uint64_t num_loops,
  int nflops_per_loop,
  int nintops_per_loop,
  int bytes_per_loop)
{
  computeLib()->LibComputeInst::computeLoop(
          num_loops, nflops_per_loop, nintops_per_loop, bytes_per_loop);
}

void
App::computeDetailed(uint64_t flops, uint64_t nintops, uint64_t bytes, int nthread)
{
  static const uint64_t overflow = 18006744072479883520ull;
  if (flops > overflow || bytes > overflow){
    spkt_abort_printf("flops/byte counts for compute overflowed");
  }
  if ((flops+nintops) < min_op_cutoff_){
    return;
  }

  debug_printf(sprockit::dbg::app_compute,
               "Rank %d for app %d: detailed compute for flops=%" PRIu64 " intops=%" PRIu64 " bytes=%" PRIu64,
               sid_.task_, sid_.app_, flops, nintops, bytes);

  computeLib()->computeDetailed(flops, nintops, bytes, nthread);
}

void
App::computeBlockRead(uint64_t bytes)
{
  computeLib()->read(bytes);
}

void
App::computeBlockWrite(uint64_t bytes)
{
  computeLib()->write(bytes);
}

SST::Params
App::getParams()
{
  return OperatingSystem::currentThread()->parentApp()->params();
}

void
App::computeBlockMemcpy(uint64_t bytes)
{
  computeLib()->copy(bytes);
}

API*
App::_get_api(const char* name)
{
  // an underlying thread may have built this
  API* my_api = apis_[name];
  if (!my_api) {
    SST::Params api_params = params_.find_prefix_params(name);
    API* new_api = API::factory::get_value(name, api_params, sid_, os_);
    apis_[name] = new_api;
    return new_api;
  } else {
    return my_api;
  }
}

void
App::run()
{
  SSTMACBacktrace(main);
  os_->incrementAppRefcount();
  endAPICall(); //this initializes things, "fake" api call at beginning
  rc_ = skeletonMain();
  //we are ending but perform the equivalent
  //to a start api call to flush any compute
  startAPICall();
  for (auto& pair : apis_){
    delete pair.second;
  }
  apis_.clear();

  //now we have to send a message to the job launcher to let it know we are done
  os_->decrementAppRefcount();
  //for now assume that the application has finished with a barrier - which is true of like everything
  if (sid_.task_ == 0 && notify_){
    int launchRoot = os_->node()->launchRoot();
    JobStopEvent* lev = new JobStopEvent(os_->node()->allocateUniqueId(),
                                             sid_.app_, unique_name_, launchRoot, os_->addr());
    os_->nicCtrlIoctl()(lev);
  }
  TaskMapping::removeGlobalMapping(sid_.app_, unique_name_);
  ThreadInfo::deregisterUserSpaceVirtualThread(stack_);
  dlcloseCheck();
}

void
App::addSubthread(Thread *thr)
{
  if (thr->threadId() == Thread::main_thread){
    thr->initId();
  }
  subthreads_[thr->threadId()] = thr;
}

Thread*
App::getSubthread(uint32_t id)
{
  auto it = subthreads_.find(id);
  if (it==subthreads_.end()){
    spkt_throw_printf(sprockit::value_error,
      "unknown thread id %u",
      id);
  }
  return it->second;
}

void
App::removeSubthread(uint32_t id)
{
  subthreads_.erase(id);
}

void
App::removeSubthread(Thread *thr)
{
  subthreads_.erase(thr->threadId());
}

bool
App::eraseMutex(int id)
{
  std::map<int,mutex_t>::iterator it = mutexes_.find(id);
  if (it == mutexes_.end()){
    return false;
  } else {
    mutexes_.erase(id);
    return true;
  }
}

bool
App::eraseCondition(int id)
{
  std::map<int,condition_t>::iterator it = conditions_.find(id);
  if (it == conditions_.end()){
    return false;
  } else {
    conditions_.erase(id);
    return true;
  }
}

mutex_t*
App::getMutex(int id)
{
  std::map<int,mutex_t>::iterator it = mutexes_.find(id);
  if (it==mutexes_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

int
App::allocateMutex()
{
  int id = next_mutex_++;
  mutexes_[id]; //implicit make
  return id;
}

int
App::allocateCondition()
{
  int id = next_condition_++;
  conditions_[id]; //implicit make
  return id;
}

condition_t*
App::getCondition(int id)
{
  std::map<int,condition_t>::iterator it = conditions_.find(id);
  if (it==conditions_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

void
UserAppCxxFullMain::deleteStatics()
{
  for (auto& pair : argv_map_){
    argv_entry& entry = pair.second;
    char* main_buffer = entry.argv[0];
    delete[] main_buffer;
    delete[] entry.argv;
  }
  argv_map_.clear();
  if (main_fxns_) delete main_fxns_;
  main_fxns_ = nullptr;
}

UserAppCxxFullMain::UserAppCxxFullMain(SST::Params& params, SoftwareId sid,
                                       OperatingSystem* os) :
  App(params, sid, os)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  std::string name = params.find<std::string>("name");
  std::map<std::string, main_fxn>::iterator it = main_fxns_->find(name);
  if (it == main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
UserAppCxxFullMain::registerMainFxn(const char *name, App::main_fxn fxn)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  (*main_fxns_)[name] = fxn;
  App::factory::register_alias("UserAppCxxFullMain", name);
}

void
UserAppCxxFullMain::initArgv(argv_entry& entry)
{
  std::string appname = params_.find<std::string>("name");
  std::string argv_str = params_->get_optional_param("argv", "");
  std::deque<std::string> argv_param_dq;
  pst::BasicStringTokenizer::tokenize(argv_str, argv_param_dq, std::string(" "));
  int argc = argv_param_dq.size() + 1;
  char* argv_buffer = new char[256 * argc];
  char* argv_buffer_ptr = argv_buffer;
  char** argv = new char*[argc+1];
  argv[0] = argv_buffer;
  ::strcpy(argv_buffer, appname.c_str());
  int i=1;
  for (auto& src_str : argv_param_dq){
    ::strcpy(argv_buffer_ptr, src_str.c_str());
    argv[i] = argv_buffer_ptr;
    //increment pointer for next strcpy
    argv_buffer_ptr += src_str.size() + 1; //+1 for null terminator
    ++i;
  }
  argv[argc] = nullptr; //missing nullptr - Issue #269
  entry.argc = argc;
  entry.argv = argv;
}

int
UserAppCxxFullMain::skeletonMain()
{
  static thread_lock argv_lock;
  argv_lock.lock();
  argv_entry& entry = argv_map_[sid_.app_];
  if (entry.argv == 0){
    initArgv(entry);
  }
  argv_lock.unlock();
  return (*fxn_)(entry.argc, entry.argv);
}

UserAppCxxEmptyMain::UserAppCxxEmptyMain(SST::Params& params, SoftwareId sid,
                                         OperatingSystem* os) :
  App(params, sid, os)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  std::string name = params.find<std::string>("name");
  std::map<std::string, empty_main_fxn>::iterator it = empty_main_fxns_->find(name);
  if (it == empty_main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
UserAppCxxEmptyMain::registerMainFxn(const char *name, App::empty_main_fxn fxn)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  (*empty_main_fxns_)[name] = fxn;
  App::factory::register_alias("UserAppCxxEmptyMain", name);
}

int
UserAppCxxEmptyMain::skeletonMain()
{
  return (*fxn_)();
}

void computeTime(double tsec)
{
  Thread* t = OperatingSystem::currentThread();
  App* a = safe_cast(App, t,
     "cannot cast current thread to app in compute_time function");
  a->compute(Timestamp(tsec));
}

}
}
