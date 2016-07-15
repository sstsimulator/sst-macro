/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/software/process/app.h>
#include <sstmac/software/api/api.h>
#include <sstmac/software/launch/app_launch.h>
#include <sstmac/software/process/api.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/logger.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::sw::app);

namespace sstmac {
namespace sw {


SpktRegister("user_app_cxx_full_main", app, user_app_cxx_full_main);
SpktRegister("user_app_cxx_empty_main", app, user_app_cxx_empty_main);

std::map<std::string, app::main_fxn>*
  user_app_cxx_full_main::main_fxns_ = 0;
std::map<std::string, app::empty_main_fxn>*
  user_app_cxx_empty_main::empty_main_fxns_ = 0;
std::map<app_id, user_app_cxx_full_main::argv_entry> user_app_cxx_full_main::argv_map_;

void
app_factory::print_apps()
{
  std::map<std::string, sprockit::SpktDesc_base*>::const_iterator
  it = object_map_->begin(), end = object_map_->end();
  cout0 << "Valid SST/macro apps are:\n";
  for ( ; it != end; ++it) {
    cout0 << it->first << "\n";
  }
  cout0 << std::endl;
}

int
app::allocate_tls_key(destructor_fxn fxn)
{
  int next = next_tls_key_;
  tls_key_fxns_[next] = fxn;
  ++next_tls_key_;
  return next;
}

void
app_factory::clear_apps()
{
  std::map<std::string, sprockit::SpktDesc_base*>::const_iterator
  it = object_map_->begin(), end = object_map_->end();
  for ( ; it != end; ++it) {
    it->second->clear();
  }
}

void
app::init_factory_params(sprockit::sim_parameters *params)
{
  params_ = params;
  consume_params(params);
}

app::app() :
  compute_inst_(0),
  compute_time_(0),
  compute_mem_move_(0),
  compute_loops_(0),
  sleep_lib_(0),
  params_(0),
  next_tls_key_(0),
  next_condition_(0),
  next_mutex_(0)
{
}

app*
app::clone(software_id newid)
{
  app* cln = clone_type();
  cln->params_ = params_;
  cln->set_sid(newid);
  cln->id_ = newid;
  cln->consume_params(params_);
  return cln;
}

app::~app()
{
  /** These get deleted by unregister */
  //sprockit::delete_vals(apis_);
  //if (compute_inst_) delete compute_inst_;
  //if (compute_time_) delete compute_time_;
  //if (compute_mem_move_) delete compute_mem_move_;
  //if (compute_loops_) delete compute_loops_;
  //if (sleep_lib_) delete sleep_lib_;
}

lib_compute_loops*
app::compute_loops_lib()
{
  if(!compute_loops_) {
    compute_loops_ = new lib_compute_loops(id_);
    register_lib(compute_loops_);
  }

  return compute_loops_;
}

void
app::delete_statics()
{
}

void
app::kill()
{
  //okay, the app is dying
  //it may be that we have subthreads that are still active
  //all of these subthreads must be cancelled and never start again
  std::map<long,thread*>::iterator it, end = subthreads_.end();
  for (it=subthreads_.begin(); it != subthreads_.end(); ++it){
    thread* thr = it->second;
    thr->cancel();
  }
  subthreads_.clear();

  thread::kill();
}

void
app::sleep(timestamp time)
{
  if (!sleep_lib_) {
    sleep_lib_ = new lib_sleep(id_);
    register_lib(sleep_lib_);
  }
  sleep_lib_->sleep(time);
}

void
app::compute(timestamp time)
{
  if (!compute_time_) {
    compute_time_ = new lib_compute_time(id_);
    register_lib(compute_time_);
  }
  compute_time_->compute(time);
}

void
app::compute_inst(compute_event* cmsg)
{
  if (!compute_inst_) {
    compute_inst_ = new lib_compute_inst(id_);
    register_lib(compute_inst_);
  }
  compute_inst_->compute_inst(cmsg);
}

void
app::compute_loop(long num_loops,
  int nflops_per_loop,
  int nintops_per_loop,
  int bytes_per_loop)
{
  if (!compute_inst_){
    compute_inst_ = new lib_compute_inst(id_);
    register_lib(compute_inst_);
  }
  compute_inst_->compute_loop(num_loops, nflops_per_loop, nintops_per_loop, bytes_per_loop);
}

void
app::compute_detailed(long flops, long nintops, long bytes)
{
  if (!compute_inst_){
    compute_inst_ = new lib_compute_inst(id_);
    register_lib(compute_inst_);
  }
  compute_inst_->compute_detailed(flops, nintops, bytes);
}

void
app::compute_block_read(long bytes)
{
  if (!compute_mem_move_) {
    init_mem_lib();
  }

  compute_mem_move_->read(bytes);
}

void
app::compute_block_write(long bytes)
{
  if (!compute_mem_move_) {
    init_mem_lib();
  }

  compute_mem_move_->write(bytes);
}

sprockit::sim_parameters*
app::get_params()
{
  return operating_system::current_thread()->parent_app()->params();
}

void
app::init_mem_lib()
{
  compute_mem_move_ = new lib_compute_memmove(id_);
  register_lib(compute_mem_move_);
}

void
app::compute_block_memcpy(long bytes)
{
  if (!compute_mem_move_) {
    init_mem_lib();
  }

  compute_mem_move_->copy(bytes);
}

api*
app::build_api(int aid, const std::string &name)
{
  // an underlying thread may have built this
  api* my_api = apis_[aid];
  if (!my_api) {
    bool new_params = params_->has_namespace(name);
    sprockit::sim_parameters* app_params = params_;
    if (new_params)
      app_params = params_->get_namespace(name);
    api* new_api = api_factory::get_value(name, app_params, id_);
    register_lib(new_api);
    apis_[aid] = new_api;
    return new_api;
  }
  else {
   return my_api;
  }
}

void
app::init_os(operating_system* os)
{
}

std::string
app::compute_name()
{

  if (!compute_time_) {
    compute_time_ = lib_compute_time::construct(id_);
    register_lib(compute_time_);
  }
  return compute_time_->lib_name();
}

std::string
app::compute_inst_name()
{
  if (!compute_inst_) {
    compute_inst_ = new lib_compute_inst(id_);
    register_lib(compute_inst_);
  }
  return compute_inst_->lib_name();
}

std::string
app::compute_memmove_name()
{
  if (!compute_mem_move_) {
    compute_mem_move_ = new lib_compute_memmove(id_);
    register_lib(compute_mem_move_);
  }
  return compute_mem_move_->lib_name();
}

void
app::run()
{
  logger::check_user_params();
  skeleton_main();
}

void
app::add_subthread(thread *thr)
{
  if (thr->thread_id() == thread::main_thread){
    thr->init_id();
  }
  subthreads_[thr->thread_id()] = thr;
}

void
app::set_subthread_done(thread* thr)
{
  subthreads_[thr->thread_id()] = 0;
}

thread*
app::get_subthread(long id)
{
  std::map<long,thread*>::iterator it = subthreads_.find(id);
  if (it==subthreads_.end()){
    spkt_throw_printf(sprockit::value_error,
      "unknown thread id %ld",
      id);
  }
  return it->second;
}

void
app::clear_subthread_from_parent_app()
{
}

void
app::remove_subthread(long id)
{
  subthreads_.erase(id);
}

void
app::remove_subthread(thread *thr)
{
  subthreads_.erase(thr->thread_id());
}

bool
app::erase_mutex(int id)
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
app::erase_condition(int id)
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
app::get_mutex(int id)
{
  std::map<int,mutex_t>::iterator it = mutexes_.find(id);
  if (it==mutexes_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

int
app::allocate_mutex()
{
  int id = next_mutex_++;
  mutexes_[id]; //implicit make
  return id;
}

int
app::allocate_condition()
{
  int id = next_condition_++;
  conditions_[id]; //implicit make
  return id;
}

condition_t*
app::get_condition(int id)
{
  std::map<int,condition_t>::iterator it = conditions_.find(id);
  if (it==conditions_.end()){
    return 0;
  } else {
    return &it->second;
  }
}

void
user_app_cxx_full_main::consume_params(sprockit::sim_parameters *params)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  std::string name = params->get_param("name");
  std::map<std::string, main_fxn>::iterator it = main_fxns_->find(name);
  if (it == main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
user_app_cxx_full_main::register_main_fxn(const char *name, app::main_fxn fxn)
{
  if (!main_fxns_) main_fxns_ = new std::map<std::string, main_fxn>;

  (*main_fxns_)[name] = fxn;
  app_factory::register_alias("user_app_cxx_full_main", name);
}

void
user_app_cxx_full_main::init_argv(argv_entry& entry)
{
  std::string appname = params_->get_param("name");
  std::vector<std::string> argv_param_vec;
  argv_param_vec.push_back(appname);
  if (params_->has_param("argv")) {
    params_->get_vector_param("argv", argv_param_vec);
  }
  int argc = argv_param_vec.size();
  char* argv_buffer = new char[256 * argc];
  char* argv_buffer_ptr = argv_buffer;
  char** argv = new char*[argc];
  for (int i = 0; i < argc; ++i) {
    const std::string& src_str = argv_param_vec[i];
    ::strcpy(argv_buffer_ptr, src_str.c_str());
    argv[i] = argv_buffer_ptr;
    //increment pointer for next strcpy
    argv_buffer_ptr += src_str.size() + 1; //+1 for null terminator
  }
  entry.argc = argc;
  entry.argv = argv;
}

void
user_app_cxx_full_main::skeleton_main()
{
  argv_entry& entry = argv_map_[id_.app_];
  if (entry.argv == 0){
    init_argv(entry);
  }
  (*fxn_)(entry.argc, entry.argv);
}


void
user_app_cxx_empty_main::consume_params(sprockit::sim_parameters *params)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  std::string name = params->get_param("name");
  std::map<std::string, empty_main_fxn>::iterator it = empty_main_fxns_->find(name);
  if (it == empty_main_fxns_->end()){
    spkt_throw_printf(sprockit::value_error,
                     "no user app with the name %s registered",
                     name.c_str());
  }
  fxn_ = it->second;
}

void
user_app_cxx_empty_main::register_main_fxn(const char *name, app::empty_main_fxn fxn)
{
  if (!empty_main_fxns_)
    empty_main_fxns_ = new std::map<std::string, empty_main_fxn>;

  (*empty_main_fxns_)[name] = fxn;
  app_factory::register_alias("user_app_cxx_empty_main", name);
}

void
user_app_cxx_empty_main::skeleton_main()
{
  (*fxn_)();
}

void compute_time(double tsec)
{
  thread* t = operating_system::current_thread();
  app* a = safe_cast(app, t,
     "cannot cast current thread to app in compute_time function");
  a->compute(timestamp(tsec));
}

}
}

