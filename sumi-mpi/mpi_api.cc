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

#include <sstream>
#include <time.h>
#include <climits>
#include <cmath>

#include <sstmac/common/runtime.h>
#include <sstmac/common/messages/sleep_event.h>

#include <sumi-mpi/mpi_queue/mpi_queue.h>

#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_status.h>
#include <sumi-mpi/mpi_request.h>

#include <sstmac/hardware/node/node.h>

#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/software/launch/job_launcher.h>

#include <sumi-mpi/mpi_protocol/mpi_protocol.h>
#include <sumi-mpi/mpi_comm/mpi_comm_factory.h>
#include <sumi-mpi/mpi_types.h>

#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/malloc.h>
#include <sprockit/keyword_registration.h>

DeclareDebugSlot(mpi_check)
RegisterDebugSlot(mpi_check,
    "validation flag that performs various sanity checks to ensure MPI application"
    " runs and terminates cleanly");

sprockit::StaticNamespaceRegister mpi_ns_reg("mpi");
sprockit::StaticNamespaceRegister queue_ns_reg("queue");

namespace sumi {

RegisterAPI("mpi", mpi_api);

key::category mpi_api::default_key_category("MPI");
key::category mpi_api::poll_key_category("MPI Poll");
key::category mpi_api::memcpy_key_category("MPI Memcpy");


static sprockit::need_delete_statics<mpi_api> del_statics;

mpi_api*
sstmac_mpi()
{
  sstmac::sw::thread* t = operating_system::current_thread();
  return t->get_api<mpi_api> ();
}

//
// Build a new mpiapi.
//
mpi_api::mpi_api(sprockit::sim_parameters* params,
                 sstmac::sw::software_id sid,
                 sstmac::sw::operating_system* os) :
  status_(is_fresh),
  next_type_id_(0),
  next_op_id_(first_custom_op_id),
  group_counter_(MPI_GROUP_WORLD+1),
  req_counter_(0),
  queue_(nullptr),
  comm_factory_(nullptr),
  worldcomm_(nullptr),
  selfcomm_(nullptr),
  sstmac::sumi_transport(params, "mpi", sid, os)
{
  sprockit::sim_parameters* queue_params = params->get_optional_namespace("queue");
  queue_ = new mpi_queue(queue_params, sid, this);

  double probe_delay_s = params->get_optional_time_param("iprobe_delay", 0);
  iprobe_delay_us_ = probe_delay_s * 1e6;

  double test_delay_s = params->get_optional_time_param("test_delay", 0);
  test_delay_us_ = test_delay_s * 1e6;
}

void
mpi_api::delete_statics()
{
}

mpi_api::~mpi_api()
{
  //MUST DELETE HERE
  //cannot delete in finalize
  //this is weird with context switching
  //an unblock finishes finalize... so finalize is called while the DES thread is still inside the queue
  //the queue outlives mpi_api::finalize!
  if (queue_) delete queue_;
  if (comm_factory_) delete comm_factory_;

  //these are often not cleaned up correctly by app
  for (auto& pair : grp_map_){
    mpi_group* grp = pair.second;
    delete grp;
  }

  //these are often not cleaned up correctly by app
  //do not delete this one
  comm_map_.erase(MPI_COMM_NULL);
  for (auto& pair : comm_map_){
    mpi_comm* comm = pair.second;
    delete comm;
  }

  //people can be sloppy cleaning up requests
  //clean up for them
  for (auto& pair : req_map_){
    mpi_request* req = pair.second;
    delete req;
  }
}

void
mpi_api::abort(MPI_Comm comm, int errcode)
{
  spkt_throw_printf(sprockit::value_error,
    "MPI rank %d exited with code %d", rank_, errcode);
}

int
mpi_api::comm_rank(MPI_Comm comm, int *rank)
{
  *rank = get_comm(comm)->rank();
  return MPI_SUCCESS;
}

int
mpi_api::do_init(int* argc, char*** argv)
{
  if (status_ == is_initialized){
    spkt_throw(sprockit::value_error,
               "MPI_Init cannot be called twice");
  }

  start_mpi_call("MPI_Init");

  sumi_transport::init();

  if (!os_) {
    spkt_throw(sprockit::null_error,
               "mpiapi::init: os has not been initialized yet");
  }

  comm_factory_ = new mpi_comm_factory(sid().app_, this);
  comm_factory_->init(rank_, nproc_);

  //printf("Initialized %p with %p\n", this, comm_factory_);

  worldcomm_ = comm_factory_->world();
  selfcomm_ = comm_factory_->self();
  comm_map_[MPI_COMM_WORLD] = worldcomm_;
  comm_map_[MPI_COMM_SELF] = selfcomm_;
  grp_map_[MPI_GROUP_WORLD] = worldcomm_->group();

  mpi_api_debug(sprockit::dbg::mpi, "MPI_Init()");

  /** Make sure all the default types are known */
  commit_builtin_types();

  status_ = is_initialized;

  barrier(MPI_COMM_WORLD);

  return MPI_SUCCESS;
}

void
mpi_api::check_init()
{
  if (status_ != is_initialized){
    spkt_abort_printf("MPI Rank %d calling functions before calling MPI_Init", rank_);
  }
}

//
// Finalize MPI.
//
int
mpi_api::do_finalize()
{  
  start_mpi_call("MPI_Finalize");
  barrier(MPI_COMM_WORLD);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Finalize()");

  status_ = is_finalized;

  if (worldcomm_->rank() == 0) {
    debug_printf(sprockit::dbg::mpi_check,
      "MPI application with ID %s passed barrier in finalize on Rank 0\n"
      "at simulation time %10.6e seconds. This generally validates the \n"
      "simulation meaning everyhing has cleanly terminated\n",
      sid().to_string().c_str(),
      os_->now().sec());
  }

  delete comm_factory_;
  comm_factory_ = nullptr;

  transport::finalize();

  return MPI_SUCCESS;
}

//
// Get current time.
//
double
mpi_api::wtime()
{
  start_mpi_call("MPI_Wtime");
  return os_->now().sec();
}

int
mpi_api::get_count(const MPI_Status *status, MPI_Datatype datatype, int *count)
{
  *count = status->count;
  return MPI_SUCCESS;
}

const char*
mpi_api::op_str(MPI_Op op)
{
#define op_case(x) case x: return #x;
 switch(op)
 {
 op_case(MPI_MAX);
 op_case(MPI_MIN);
 op_case(MPI_SUM);
 op_case(MPI_PROD);
 op_case(MPI_LAND);
 op_case(MPI_BAND);
 op_case(MPI_LOR);
 op_case(MPI_BOR);
 op_case(MPI_LXOR);
 op_case(MPI_BXOR);
 op_case(MPI_MAXLOC);
 op_case(MPI_MINLOC);
 op_case(MPI_REPLACE);
 default:
  return "CUSTOM";
 }
}

std::string
mpi_api::type_str(MPI_Datatype mid)
{
  mpi_type* ty = type_from_id(mid);
  switch(ty->type())
  {
    case mpi_type::PRIM:
      return sprockit::printf("%s=%d", ty->label.c_str(), mid);
    case mpi_type::PAIR:
      return sprockit::printf("PAIR=%d", mid);
    case mpi_type::VEC:
      return sprockit::printf("VEC=%d", mid);
    case mpi_type::IND:
      return sprockit::printf("IND=%d", mid);
    case mpi_type::NONE:
      return sprockit::printf("NONE=%d", mid);
  }
}

std::string
mpi_api::comm_str(MPI_Comm comm)
{
  if (comm == worldcomm_->id()){
    return "MPI_COMM_WORLD";
  }
  else if (comm == selfcomm_->id()){
    return "MPI_COMM_SELF";
  }
  else if (comm == mpi_comm::comm_null->id()){
    return "MPI_COMM_NULL";
  }
  else {
    return sprockit::printf("COMM=%d", int(comm));
  }
}

std::string
mpi_api::comm_str(mpi_comm* comm)
{
  if (comm == worldcomm_){
    return "MPI_COMM_WORLD";
  }
  else if (comm == selfcomm_){
    return "MPI_COMM_SELF";
  }
  else if (comm == mpi_comm::comm_null){
    return "MPI_COMM_NULL";
  }
  else {
    return sprockit::printf("COMM=%d", int(comm->id()));
  }
}

std::string
mpi_api::tag_str(int tag)
{
  if (tag==MPI_ANY_TAG){
    return "int_ANY";
  }
  else {
    return sprockit::printf("%d", int(tag));
  }
}

std::string
mpi_api::src_str(int id)
{
  if (id == MPI_ANY_SOURCE){
    return "MPI_SOURCE_ANY";
  }
  else {
    return sprockit::printf("%d", int(id));
  }
}

std::string
mpi_api::src_str(mpi_comm* comm, int id)
{
  if (id == MPI_ANY_SOURCE){
    return "MPI_SOURCE_ANY";
  }
  else {
    return sprockit::printf("%d:%d", int(id), int(comm->peer_task(id)));
  }
}

mpi_comm*
mpi_api::get_comm(MPI_Comm comm)
{
  spkt_unordered_map<MPI_Comm, mpi_comm*>::iterator it
    = comm_map_.find(comm);
  if (it == comm_map_.end()) {
    if (comm == MPI_COMM_WORLD){
      cerrn << "Could not find MPI_COMM_WORLD! "
            << "Are you sure you called MPI_Init" << std::endl;
    }
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi communicator %d for rank %d",
        comm, int(rank_));
  }
  return it->second;
}

mpi_group*
mpi_api::get_group(MPI_Group grp)
{
  spkt_unordered_map<MPI_Group, mpi_group*>::iterator it
    = grp_map_.find(grp);
  if (it == grp_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi group %d for rank %d",
        grp, int(rank_));
  }
  return it->second;
}

void
mpi_api::add_keyval(int key, keyval*keyval)
{
  keyvals_[key] = keyval;
}

keyval*
mpi_api::get_keyval(int key)
{
  check_key(key);
  return keyvals_[key];
}

mpi_request*
mpi_api::get_request(MPI_Request req)
{
  if (req == MPI_REQUEST_NULL){
    return 0;
  }

  spkt_unordered_map<MPI_Request, mpi_request*>::iterator it
    = req_map_.find(req);
  if (it == req_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi request %d for rank %d",
        req, int(rank_));
  }

  return it->second;
}

MPI_Comm
mpi_api::add_comm_ptr(mpi_comm* ptr)
{
  MPI_Comm comm = ptr->id();
  comm_map_[comm] = ptr;
  return comm;
}

void
mpi_api::erase_comm_ptr(MPI_Comm comm)
{
  if (comm != MPI_COMM_WORLD && comm != MPI_COMM_SELF && comm != MPI_COMM_NULL) {
    comm_ptr_map::iterator it = comm_map_.find(comm);
    if (it == comm_map_.end()) {
      spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi communicator %d for rank %d",
        comm, int(rank_));
    }
    comm_map_.erase(it);
  }
}

void
mpi_api::add_group_ptr(MPI_Group grp, mpi_group* ptr)
{
  grp_map_[grp] = ptr;
}


MPI_Group
mpi_api::add_group_ptr(mpi_group* ptr)
{
  MPI_Group grp = group_counter_++;
  grp_map_[grp] = ptr;
  ptr->set_id(grp);
  return grp;
}

void
mpi_api::erase_group_ptr(MPI_Group grp)
{
  if (grp != MPI_GROUP_EMPTY && grp != comm_grp_map_[MPI_COMM_WORLD]
      && grp != comm_grp_map_[MPI_COMM_SELF]) {
    group_ptr_map::iterator it = grp_map_.find(grp);
    if (it == grp_map_.end()) {
      spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi group %d for rank %d",
        grp, int(rank_));
    }
    grp_map_.erase(it);
  }
}

MPI_Request
mpi_api::add_request_ptr(mpi_request* ptr)
{
  MPI_Request req = req_counter_++;
  req_map_[req] = ptr;
  return req;
}

void
mpi_api::erase_request_ptr(MPI_Request req)
{
  req_ptr_map::iterator it = req_map_.find(req);
  if (it == req_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi request %d for rank %d",
        req, int(rank_));
  }
  delete it->second;
  req_map_.erase(it);
}

void
mpi_api::add_comm_grp(MPI_Comm comm, MPI_Group grp)
{
  comm_grp_map_[comm] = grp;
}

void
mpi_api::check_key(int key)
{
  if (keyvals_.find(key) == keyvals_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "mpi_api::check_key: could not find keyval %d in key_map", key);
  }
}

int
mpi_api::error_string(int errorcode, char *str, int *resultlen)
{
  static const char* errorstr = "mpi error";
  *resultlen = ::strlen(errorstr);
  ::strcpy(str, errorstr);
  return MPI_SUCCESS;
}

}


