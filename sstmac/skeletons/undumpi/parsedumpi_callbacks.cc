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

#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/skeletons/undumpi/parsedumpi_callbacks.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_types.h>
#include <sstmac/libraries/mpi/type_operator.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <cstring>

namespace sstmac {
namespace sw {


/// The shared callback pointer array.
libundumpi_callbacks *parsedumpi_callbacks::cbacks_ = NULL;

/// Populate callbacks.
parsedumpi_callbacks::
parsedumpi_callbacks(parsedumpi *parent,
          parsedumpi_callbacks::if_unimplemented action,
          bool pessimistic) :
  parent_(parent),
  action_(action),
  pessimistic_(pessimistic)
{
  static thread_lock lock;
  lock.lock();
  if(cbacks_ == NULL) {
    set_callbacks();
  }
  trace_compute_start_.sec = -1;
  init_maps();
  memset(&datatype_sizes_, 0, sizeof(dumpi_sizeof));
  lock.unlock();
}

/// Start parsing.
void
parsedumpi_callbacks::parse_stream(const std::string &fname,
  bool print_progress,
  double percent_terminate)
{
  static const std::string here("parsedumpi_callbacks::parse_stream");
  if(parent_ == NULL) {
    throw sprockit::null_error(here + ":  NULL parent pointer.");
  }
  dumpi_profile *profile = undumpi_open(fname.c_str());
  if(profile == NULL) {
    throw sprockit::io_error(here + ":  Unable to open \"" + fname + "\" for reading.");
  }
  datatype_sizes_ = undumpi_read_datatype_sizes(profile);
  int retval = undumpi_read_stream_full(profile, cbacks_, this, print_progress, percent_terminate);
  if(retval != 1) {
    spkt_throw(sprockit::io_error, here + ":  Failed reading dumpi stream.\n");
  }
  undumpi_close(profile);
}

/// Initialize maps.
void parsedumpi_callbacks::init_maps()
{
  // Null requests.
  request_[DUMPI_REQUEST_ERROR] = 0;
  request_[DUMPI_REQUEST_NULL] = 0;
  // Built-in mpitypes.
  mpitype_[DUMPI_DATATYPE_ERROR] = mpi_type::mpi_null->id;
  mpitype_[DUMPI_DATATYPE_NULL] = mpi_type::mpi_null->id;
  mpitype_[DUMPI_CHAR] = mpi_type::mpi_char->id;
  mpitype_[DUMPI_SIGNED_CHAR] = mpi_type::mpi_signed_char->id;
  mpitype_[DUMPI_UNSIGNED_CHAR] = mpi_type::mpi_unsigned_char->id;
  mpitype_[DUMPI_BYTE] = mpi_type::mpi_byte->id;
  mpitype_[DUMPI_WCHAR] = mpi_type::mpi_wchar->id;
  mpitype_[DUMPI_SHORT] = mpi_type::mpi_short->id;
  mpitype_[DUMPI_UNSIGNED_SHORT] = mpi_type::mpi_unsigned_short->id;
  mpitype_[DUMPI_INT] = mpi_type::mpi_int->id;
  mpitype_[DUMPI_UNSIGNED] = mpi_type::mpi_unsigned->id;
  mpitype_[DUMPI_LONG] = mpi_type::mpi_long->id;
  mpitype_[DUMPI_UNSIGNED_LONG] = mpi_type::mpi_unsigned_long->id;
  mpitype_[DUMPI_FLOAT] = mpi_type::mpi_float->id;
  mpitype_[DUMPI_DOUBLE] = mpi_type::mpi_double->id;
  mpitype_[DUMPI_LONG_DOUBLE] = mpi_type::mpi_long_double->id;
  mpitype_[DUMPI_LONG_LONG_INT] = mpi_type::mpi_long_long_int->id;
  mpitype_[DUMPI_UNSIGNED_LONG_LONG] = mpi_type::mpi_unsigned_long_long->id;
  mpitype_[DUMPI_LONG_LONG] = mpi_type::mpi_long_long->id;
  mpitype_[DUMPI_PACKED] = mpi_type::mpi_packed->id;
  mpitype_[DUMPI_LB] = mpi_type::mpi_lb->id;
  mpitype_[DUMPI_UB] = mpi_type::mpi_ub->id;
  mpitype_[DUMPI_FLOAT_INT] = mpi_type::mpi_float_int->id;
  mpitype_[DUMPI_DOUBLE_INT] = mpi_type::mpi_double_int->id;
  mpitype_[DUMPI_LONG_INT] = mpi_type::mpi_long_int->id;
  mpitype_[DUMPI_SHORT_INT] = mpi_type::mpi_short_int->id;
  mpitype_[DUMPI_2INT] = mpi_type::mpi_2int->id;
  mpitype_[DUMPI_LONG_DOUBLE_INT] = mpi_type::mpi_long_double_int->id;
  // MPI communicators.
  mpicomm_[DUMPI_COMM_NULL]  = mpi_comm::comm_null; 
  mpicomm_[DUMPI_COMM_WORLD] = parent_->mpi()->comm_world();
  mpicomm_[DUMPI_COMM_SELF]  = parent_->mpi()->comm_self();
  mpigroups_[DUMPI_FIRST_USER_GROUP] = parent_->mpi()->comm_world()->group();
  // MPI operations
  mpiop_[DUMPI_MAX] = mpi_op::max;
  mpiop_[DUMPI_MIN] = mpi_op::min;
  mpiop_[DUMPI_SUM] = mpi_op::sum;
  mpiop_[DUMPI_PROD] = mpi_op::prod;
  mpiop_[DUMPI_LAND] = mpi_op::land;
  mpiop_[DUMPI_BAND] = mpi_op::band;
  mpiop_[DUMPI_LOR] = mpi_op::lor;
  mpiop_[DUMPI_BOR] = mpi_op::bor;
  mpiop_[DUMPI_LXOR] = mpi_op::lxor;
  mpiop_[DUMPI_BXOR] = mpi_op::bxor;
  mpiop_[DUMPI_MINLOC] = mpi_op::minloc;
  mpiop_[DUMPI_MAXLOC] = mpi_op::maxloc;
  mpiop_[DUMPI_REPLACE] = mpi_op::replace;
  mpiop_[DUMPI_OP_NULL] = mpi_op::op_null;
}


// Convert a dumpi time difference into a timestamp.
inline timestamp deltat(const dumpi_clock &left, const dumpi_clock &right)
{
  static const int64_t billion(1e9);
  return timestamp::exact_nsec(billion*(left.sec-right.sec) +
                               (left.nsec - right.nsec));
}

/// Indicate that we are starting an MPI call.
void parsedumpi_callbacks::
start_mpi(const dumpi_time *cpu, const dumpi_time *wall,
          const dumpi_perfinfo *perf)
{
  const bool use_walltime = true;
  const dumpi_time &usetime = (use_walltime ? *wall : *cpu);
  if(trace_compute_start_.sec >= 0) {
    // This is not the first MPI call -- simulate a compute.
    if(perf != NULL && perf->count > 0) {
      spkt_throw(sprockit::unimplemented_error,
        "DUMPI perfctr compute: only compatible with time");

      // We get here if we are using the processor model.
      if(size_t(perf->count) != perfctr_compute_start_.size())
        spkt_throw(sprockit::illformed_error, "parsedumpi_callbacks::start_mpi:  "
                              "Number of active perfcounters changed "
                              "between calls.");
      compute_event* evts = new compute_event;
      for(int i = 0; i < perf->count; ++i) {
        int64_t evtval = perf->invalue[i] - perfctr_compute_start_[i];
        if(evtval < 0) {
          spkt_throw(sprockit::illformed_error, "parsedumpi_callbacks::start_mpi:  "
                                "Performance counter moved backward "
                                "between calls.");
        }
        /**
         this needs to change
         evts->set_event_value(perf->counter_tag[i], evtval);
        */
      }
      parent_->compute_inst(evts);
      delete evts;
    }
    else {
      // We get here if we are not using processor modeling.
      timestamp thetime = (parent_->timescaling_ *
                           deltat(usetime.start, trace_compute_start_));

      double time1 = usetime.start.sec + (usetime.start.nsec * 1e-9);
      double time2 = trace_compute_start_.sec + (trace_compute_start_.nsec * 1e-9);
      if(thetime.ticks() < 0) {
        // It turns out that RSQ can actually screw up on times
        // (both wall time and cpu time are wrong for final MPI barrier
        // in 016-4x2x2_run01/dumpi0014.bin collected on 08/07/09
        thetime = timestamp(0);
      }
      parent_->compute(thetime);
    }
  }
}

/// Indicate that we have completed an MPI call.
void parsedumpi_callbacks::
end_mpi(const dumpi_time *cpu, const dumpi_time *wall,
        const dumpi_perfinfo *perf)
{
  const bool use_walltime = true;
  const dumpi_time &usetime = (use_walltime ? *wall : *cpu);
  trace_compute_start_ = usetime.stop;
  if(perf) {
    perfctr_compute_start_.resize(perf->count);
    for(int i = 0; i < perf->count; ++i) {
      perfctr_compute_start_[i] = perf->outvalue[i];
    }
  }
}

/// Access the mpiapi.
mpi_api* parsedumpi_callbacks::getmpi()
{
  return parent_->mpi();
}



/// Store a single request handle.
void parsedumpi_callbacks::
store_request(dumpi_request id, mpi_request* request)
{
  if(id >= DUMPI_FIRST_USER_REQUEST) {
    request_[id] = request;
  }
}

/// Get a single request handle.
mpi_request*&
parsedumpi_callbacks::get_request(dumpi_request id)
{
  request_map_t::iterator it = request_.find(id);
  if(it == request_.end()){
    spkt_throw(sprockit::value_error, 
       "parsedumpi_callbacks::get_request %d on node unmapped request id %d",
        int(id), int(parent_->mpi()->comm_world()->rank()));
  }
  return it->second;
}

//
// Get a group of request handles.
//
void parsedumpi_callbacks::
get_requests(int count, const dumpi_request *dumpireq,
             std::vector<mpi_request*> &simreq)
{
  if(count < 0)
    throw sprockit::value_error("parsedumpi_callbacks::get_requests:  "
                      "Negative request count.");
  simreq.resize(count);
  for(int i = 0; i < count; ++i) {
    simreq[i] = get_request(dumpireq[i]);
  }
}

/// Remove a request from the map.
void parsedumpi_callbacks::
complete_request(dumpi_request id)
{
  return; // until I find a better way to deal with persistent requests.
  if(id >= DUMPI_FIRST_USER_REQUEST) {
    request_map_t::iterator it = request_.find(id);
    if(it != request_.end()) {
      request_.erase(it);
    }
  }
}

/// Complete multiple requests.
template <typename Iter>
void parsedumpi_callbacks::complete_requests(Iter begin, Iter end)
{
  for(Iter it = begin; it != end; ++it) {
    complete_request(*it);
  }
}

void parsedumpi_callbacks::
nullify_request(dumpi_request rid)
{
  request_[rid] = 0;
}

void parsedumpi_callbacks::
nullify_requests(const dumpi_request* dumpi_reqs, const std::vector<mpi_request*>& sst_reqs)
{
  int num = sst_reqs.size();
  for (int i=0; i < num; ++i){
    if (sst_reqs[i] == 0){
      nullify_request(dumpi_reqs[i]);
    }
  }
}

/// The simulator finished a different request than the tracefile.
/// Swap request indices so they agree on which request is done.
void parsedumpi_callbacks::
remap_request(dumpi_request done_tracefile,
              dumpi_request done_simulator)
{
  if(done_tracefile < DUMPI_FIRST_USER_REQUEST ||
      done_simulator < DUMPI_FIRST_USER_REQUEST) {
    return;
  }
  else if(done_tracefile == done_simulator) {
    //do nothing
  }
  else {
    mpi_request* tmp = request_[done_simulator];
    request_[done_simulator] = request_[done_tracefile];
    request_[done_tracefile] = tmp;
  }
}

void parsedumpi_callbacks::
remap_request(int total_num_requests,
              int dumpi_req_idx, int sst_req_idx,
              const dumpi_request* tracefile_requests)
{
  std::vector<int> sst_completed(1, sst_req_idx);
  int num_completed_dumpi = 1;
  //just use the waitsome version of remap requests
  remap_requests(total_num_requests, num_completed_dumpi,
        &dumpi_req_idx, sst_completed, tracefile_requests);
}

/// The simulator and tracefile each completed some number of requests,
/// but these requests may not be the same. This method is capable
/// of handling null requests from mpi_waitsome
void parsedumpi_callbacks::
remap_requests(int total_num_requests,
                int num_done_tracefile,
                const dumpi_request* dumpi_completed,
                const std::vector<int>& sst_completed,
                const dumpi_request* tracefile_requests)
{
  spkt_throw(sprockit::unimplemented_error, "parsedumpi::remap_requests");

  if (total_num_requests == num_done_tracefile)
    return;  //no need to do any processing - all reqs finished

  int num_not_done_dumpi = total_num_requests - num_done_tracefile;
  int num_not_done_sst = total_num_requests - sst_completed.size();
  // we can only process the minimum number not done
  int num_done = std::min(num_not_done_dumpi, num_not_done_sst);
  int dumpi_idx = 0;
  int sst_idx = 0;
  int sst_req_num = 0;
  int dumpi_req_num = 0;
  int num_processed = 0;
  while (num_processed < num_done){
    while (sst_completed[sst_idx] == sst_req_num){ ++sst_idx; ++sst_req_num; }
    while (dumpi_completed[dumpi_idx] == dumpi_req_num) { ++dumpi_idx; ++dumpi_req_num; }

    //we have found two requests that are not done
    //match these up
    remap_request(tracefile_requests[dumpi_req_num], tracefile_requests[sst_req_num]);

    ++sst_req_num;
    ++dumpi_req_num;
    ++num_processed;
  }
}

/// Get an mpiid.
/// Special handling for MPI_ROOT and MPI_ANY_SOURCE.
mpi_id parsedumpi_callbacks::get_mpiid(dumpi_source id)
{
  if(id == DUMPI_ANY_SOURCE) {
    return mpi::any_source;
  }
  else if(id == DUMPI_ROOT) {
    return mpi::root;
  }
  else {
    return mpi_id(id);
  }
}

/// Get an mpi tag.
/// Special handling for MPI_ANY_TAG.
mpi_tag parsedumpi_callbacks::get_mpitag(dumpi_tag tag)
{
  if(tag == DUMPI_ANY_TAG) {
    return mpi::any_tag;
  }
  else {
    return mpi_tag(tag);
  }
}

/// Add a new mpi type.
void parsedumpi_callbacks::
add_mpitype(dumpi_datatype id, mpi_type_id mpit)
{
  if(id < DUMPI_FIRST_USER_DATATYPE){
    spkt_throw_printf(sprockit::value_error, 
       "parsedumpi_callbacks::add_mpitype: %d trying to redefine built-in datatype index",
       int(id));
  }
  mpitype_[id] = mpit;
}

/// Erase the mapping for an mpi type.  Does not erase built-in mpi types.
void parsedumpi_callbacks::erase_mpitype(dumpi_datatype id)
{
  if(id >= DUMPI_FIRST_USER_DATATYPE) {
    mpitype_map_t::iterator it = mpitype_.find(id);
    if(it != mpitype_.end()) {
      mpitype_.erase(it);
    }
  }
}

/// Access mpi type.
/// \throw sprockit::value_error if no mapping exists for this datatype.
mpi_type_id
parsedumpi_callbacks::get_mpitype(int count, dumpi_datatype id)
{
  if((count) > 0 && (id == DUMPI_DATATYPE_ERROR)) {
    spkt_throw(sprockit::value_error,
        "parsedumpi_callbacks::get_mpitype: "
        "cannot send non-zero count of MPI_DATATYPE_ERROR");
  }
  mpitype_map_t::iterator it = mpitype_.find(id);
  if(it == mpitype_.end()) {
    if(id < datatype_sizes_.count) {
      int size = datatype_sizes_.size[id];
      mpitype_[id] = mpi_type::builtins[size].id;
    }
    else {
      cerrn << sprockit::printf("Warning: no match for datatype id %d - assuming double\n", int(id));
      mpitype_[id] = mpi_type::mpi_double->id;
    }
    return mpitype_[id];
  }
  return it->second;
}

/// Access an mpi communicator.
mpi_comm* parsedumpi_callbacks::get_mpicomm(dumpi_comm id)
{
  if(id == DUMPI_COMM_ERROR)
    throw sprockit::value_error("parsedumpi_callbacks::get_mpicomm:  "
                      "Cannot operate on MPI_COMM_ERROR.");
  mpicomm_map_t::iterator it = mpicomm_.find(id);
  if(it == mpicomm_.end())
    spkt_throw_printf(sprockit::value_error,
     "parsedumpi_callbacks::get_mpicomm: No match for communicator index %d",
     int(id));
  return it->second;
}

/// Add a new mpi comm.
void parsedumpi_callbacks::add_mpicomm(dumpi_comm id,
                                        mpi_comm* comm)
{
  if(id < DUMPI_FIRST_USER_COMM) {
    spkt_throw_printf(sprockit::value_error,
     "parsedumpi_callbacks::add_mpicomm: trying to redefine built-in comm index %d",
     int(id));
  }
  mpicomm_[id] = comm;
}

/// Erase the mapping for an mpi comm.  Does not erase MPI_COMM_WORLD.
void parsedumpi_callbacks::erase_mpicomm(dumpi_comm id)
{
  if(id >= DUMPI_FIRST_USER_COMM) {
    mpicomm_map_t::iterator it = mpicomm_.find(id);
    if(it != mpicomm_.end()) {
      mpicomm_.erase(it);
    }
  }
}

/// Access an mpi group.
mpi_group* parsedumpi_callbacks::get_mpigroup(dumpi_group id)
{
  if(id == DUMPI_COMM_ERROR)
    throw sprockit::value_error("parsedumpi_callbacks::get_mpicomm:  "
                      "Cannot operate on MPI_COMM_ERROR.");
  mpigroup_map_t::iterator it = mpigroups_.find(id);
  if(it == mpigroups_.end())
    spkt_throw_printf(sprockit::value_error,
     "parsedumpi_callbacks::get_mpigroup: no match for communicator index %d", 
      int(id));
  return it->second;
}

/// Add a new mpi comm.
void parsedumpi_callbacks::add_mpigroup(dumpi_group id,
    mpi_group*comm)
{
  if(id < DUMPI_FIRST_USER_COMM) {
    spkt_throw_printf(sprockit::value_error,
     "parsedumpi_callbacks::add_mpigroup: trying to redefine built-in comm index %d",
     int(id));
  }
  mpigroups_[id] = comm;
}

/// Erase the mapping for an mpi comm.  Does not erase MPI_COMM_WORLD.
void parsedumpi_callbacks::erase_mpigroup(dumpi_group id)
{
  if(id >= DUMPI_FIRST_USER_COMM) {
    mpigroup_map_t::iterator it = mpigroups_.find(id);
    if(it != mpigroups_.end()) {
      mpigroups_.erase(it);
    }
  }
}

/// Add a new mpi op.
void parsedumpi_callbacks::add_mpiop(dumpi_op id, mpi_op* op)
{
  if(id < DUMPI_FIRST_USER_OP) {
    spkt_throw_printf(sprockit::value_error,
       "parsedumpi_callbacks::add_mpiop: trying to redefine built-in operation index %d",
       int(id));
  }
  mpiop_[id] = op;
}

/// Erase the mapping for an mpi op.  Does not erase built-in operations.
void parsedumpi_callbacks::erase_mpiop(dumpi_op id)
{
  if(id >= DUMPI_FIRST_USER_OP) {
    mpiop_map_t::iterator it = mpiop_.find(id);
    if(it != mpiop_.end()) {
      mpiop_.erase(it);
    }
  }
}

/// Access an mpi opunicator.
mpi_op*
parsedumpi_callbacks::get_mpiop(dumpi_op id)
{
  if(id == DUMPI_OP_ERROR) {
    spkt_throw_printf(sprockit::value_error, "Cannot operate on MPI_OP_ERROR");
  }
  mpiop_map_t::iterator it = mpiop_.find(id);
  if(it == mpiop_.end()){
    //you know what - I don't care - just return sum
    return get_mpiop(DUMPI_SUM);
    //spkt_throw_printf(sprockit::value_error, "no match for mpi operation index %d", id);
  }
  return it->second;
}

/// Set all callbacks.
void parsedumpi_callbacks::set_callbacks()
{
  static thread_lock lock;
  lock.lock();
  //libundumpi_clear_callbacks(cbacks_);
  if(cbacks_ == NULL) {
    cbacks_ = new libundumpi_callbacks;
    libundumpi_clear_callbacks(cbacks_);
  }
  cbacks_->on_send                      = on_MPI_Send                     ;
  cbacks_->on_recv                      = on_MPI_Recv                     ;
  cbacks_->on_get_count                 = on_MPI_Get_count                ;
  cbacks_->on_bsend                     = on_MPI_Bsend                    ;
  cbacks_->on_ssend                     = on_MPI_Ssend                    ;
  cbacks_->on_rsend                     = on_MPI_Rsend                    ;
  cbacks_->on_buffer_attach             = on_MPI_Buffer_attach            ;
  cbacks_->on_buffer_detach             = on_MPI_Buffer_detach            ;
  cbacks_->on_isend                     = on_MPI_Isend                    ;
  cbacks_->on_ibsend                    = on_MPI_Ibsend                   ;
  cbacks_->on_issend                    = on_MPI_Issend                   ;
  cbacks_->on_irsend                    = on_MPI_Irsend                   ;
  cbacks_->on_irecv                     = on_MPI_Irecv                    ;
  cbacks_->on_wait                      = on_MPI_Wait                     ;
  cbacks_->on_test                      = on_MPI_Test                     ;
  cbacks_->on_request_free              = on_MPI_Request_free             ;
  cbacks_->on_waitany                   = on_MPI_Waitany                  ;
  cbacks_->on_testany                   = on_MPI_Testany                  ;
  cbacks_->on_waitall                   = on_MPI_Waitall                  ;
  cbacks_->on_testall                   = on_MPI_Testall                  ;
  cbacks_->on_waitsome                  = on_MPI_Waitsome                 ;
  cbacks_->on_testsome                  = on_MPI_Testsome                 ;
  cbacks_->on_iprobe                    = on_MPI_Iprobe                   ;
  cbacks_->on_probe                     = on_MPI_Probe                    ;
  cbacks_->on_cancel                    = on_MPI_Cancel                   ;
  cbacks_->on_test_cancelled            = on_MPI_Test_cancelled           ;
  cbacks_->on_send_init                 = on_MPI_Send_init                ;
  cbacks_->on_bsend_init                = on_MPI_Bsend_init               ;
  cbacks_->on_ssend_init                = on_MPI_Ssend_init               ;
  cbacks_->on_rsend_init                = on_MPI_Rsend_init               ;
  cbacks_->on_recv_init                 = on_MPI_Recv_init                ;
  cbacks_->on_start                     = on_MPI_Start                    ;
  cbacks_->on_startall                  = on_MPI_Startall                 ;
  cbacks_->on_sendrecv                  = on_MPI_Sendrecv                 ;
  cbacks_->on_sendrecv_replace          = on_MPI_Sendrecv_replace         ;
  cbacks_->on_type_contiguous           = on_MPI_Type_contiguous          ;
  cbacks_->on_type_vector               = on_MPI_Type_vector              ;
  cbacks_->on_type_hvector              = on_MPI_Type_hvector             ;
  cbacks_->on_type_indexed              = on_MPI_Type_indexed             ;
  cbacks_->on_type_hindexed             = on_MPI_Type_hindexed            ;
  cbacks_->on_type_struct               = on_MPI_Type_struct              ;
  cbacks_->on_address                   = on_MPI_Address                  ;
  cbacks_->on_type_extent               = on_MPI_Type_extent              ;
  cbacks_->on_type_size                 = on_MPI_Type_size                ;
  cbacks_->on_type_lb                   = on_MPI_Type_lb                  ;
  cbacks_->on_type_ub                   = on_MPI_Type_ub                  ;
  cbacks_->on_type_commit               = on_MPI_Type_commit              ;
  cbacks_->on_type_free                 = on_MPI_Type_free                ;
  cbacks_->on_get_elements              = on_MPI_Get_elements             ;
  cbacks_->on_pack                      = on_MPI_Pack                     ;
  cbacks_->on_unpack                    = on_MPI_Unpack                   ;
  cbacks_->on_pack_size                 = on_MPI_Pack_size                ;
  cbacks_->on_barrier                   = on_MPI_Barrier                  ;
  cbacks_->on_bcast                     = on_MPI_Bcast                    ;
  cbacks_->on_gather                    = on_MPI_Gather                   ;
  cbacks_->on_gatherv                   = on_MPI_Gatherv                  ;
  cbacks_->on_scatter                   = on_MPI_Scatter                  ;
  cbacks_->on_scatterv                  = on_MPI_Scatterv                 ;
  cbacks_->on_allgather                 = on_MPI_Allgather                ;
  cbacks_->on_allgatherv                = on_MPI_Allgatherv               ;
  cbacks_->on_alltoall                  = on_MPI_Alltoall                 ;
  cbacks_->on_alltoallv                 = on_MPI_Alltoallv                ;
  cbacks_->on_reduce                    = on_MPI_Reduce                   ;
  cbacks_->on_op_create                 = on_MPI_Op_create                ;
  cbacks_->on_op_free                   = on_MPI_Op_free                  ;
  cbacks_->on_allreduce                 = on_MPI_Allreduce                ;
  cbacks_->on_reduce_scatter            = on_MPI_Reduce_scatter           ;
  cbacks_->on_scan                      = on_MPI_Scan                     ;
  cbacks_->on_group_size                = on_MPI_Group_size               ;
  cbacks_->on_group_rank                = on_MPI_Group_rank               ;
  cbacks_->on_group_translate_ranks     = on_MPI_Group_translate_ranks    ;
  cbacks_->on_group_compare             = on_MPI_Group_compare            ;
  cbacks_->on_comm_group                = on_MPI_Comm_group               ;
  cbacks_->on_group_union               = on_MPI_Group_union              ;
  cbacks_->on_group_intersection        = on_MPI_Group_intersection       ;
  cbacks_->on_group_difference          = on_MPI_Group_difference         ;
  cbacks_->on_group_incl                = on_MPI_Group_incl               ;
  cbacks_->on_group_excl                = on_MPI_Group_excl               ;
  cbacks_->on_group_range_incl          = on_MPI_Group_range_incl         ;
  cbacks_->on_group_range_excl          = on_MPI_Group_range_excl         ;
  cbacks_->on_group_free                = on_MPI_Group_free               ;
  cbacks_->on_comm_size                 = on_MPI_Comm_size                ;
  cbacks_->on_comm_rank                 = on_MPI_Comm_rank                ;
  cbacks_->on_comm_compare              = on_MPI_Comm_compare             ;
  cbacks_->on_comm_dup                  = on_MPI_Comm_dup                 ;
  cbacks_->on_comm_create               = on_MPI_Comm_create              ;
  cbacks_->on_comm_split                = on_MPI_Comm_split               ;
  cbacks_->on_comm_free                 = on_MPI_Comm_free                ;
  cbacks_->on_comm_test_inter           = on_MPI_Comm_test_inter          ;
  cbacks_->on_comm_remote_size          = on_MPI_Comm_remote_size         ;
  cbacks_->on_comm_remote_group         = on_MPI_Comm_remote_group        ;
  cbacks_->on_intercomm_create          = on_MPI_Intercomm_create         ;
  cbacks_->on_intercomm_merge           = on_MPI_Intercomm_merge          ;
  cbacks_->on_keyval_create             = on_MPI_Keyval_create            ;
  cbacks_->on_keyval_free               = on_MPI_Keyval_free              ;
  cbacks_->on_attr_put                  = on_MPI_Attr_put                 ;
  cbacks_->on_attr_get                  = on_MPI_Attr_get                 ;
  cbacks_->on_attr_delete               = on_MPI_Attr_delete              ;
  cbacks_->on_topo_test                 = on_MPI_Topo_test                ;
  cbacks_->on_cart_create               = on_MPI_Cart_create              ;
  cbacks_->on_dims_create               = on_MPI_Dims_create              ;
  cbacks_->on_graph_create              = on_MPI_Graph_create             ;
  cbacks_->on_graphdims_get             = on_MPI_Graphdims_get            ;
  cbacks_->on_graph_get                 = on_MPI_Graph_get                ;
  cbacks_->on_cartdim_get               = on_MPI_Cartdim_get              ;
  cbacks_->on_cart_get                  = on_MPI_Cart_get                 ;
  cbacks_->on_cart_rank                 = on_MPI_Cart_rank                ;
  cbacks_->on_cart_coords               = on_MPI_Cart_coords              ;
  cbacks_->on_graph_neighbors_count     = on_MPI_Graph_neighbors_count    ;
  cbacks_->on_graph_neighbors           = on_MPI_Graph_neighbors          ;
  cbacks_->on_cart_shift                = on_MPI_Cart_shift               ;
  cbacks_->on_cart_sub                  = on_MPI_Cart_sub                 ;
  cbacks_->on_cart_map                  = on_MPI_Cart_map                 ;
  cbacks_->on_graph_map                 = on_MPI_Graph_map                ;
  cbacks_->on_get_processor_name        = on_MPI_Get_processor_name       ;
  cbacks_->on_get_version               = on_MPI_Get_version              ;
  cbacks_->on_errhandler_create         = on_MPI_Errhandler_create        ;
  cbacks_->on_errhandler_set            = on_MPI_Errhandler_set           ;
  cbacks_->on_errhandler_get            = on_MPI_Errhandler_get           ;
  cbacks_->on_errhandler_free           = on_MPI_Errhandler_free          ;
  cbacks_->on_error_string              = on_MPI_Error_string             ;
  cbacks_->on_error_class               = on_MPI_Error_class              ;
  cbacks_->on_wtime                     = on_MPI_Wtime                    ;
  cbacks_->on_wtick                     = on_MPI_Wtick                    ;
  cbacks_->on_init                      = on_MPI_Init                     ;
  cbacks_->on_finalize                  = on_MPI_Finalize                 ;
  cbacks_->on_initialized               = on_MPI_Initialized              ;
  cbacks_->on_abort                     = on_MPI_Abort                    ;
  cbacks_->on_close_port                = on_MPI_Close_port               ;
  cbacks_->on_comm_accept               = on_MPI_Comm_accept              ;
  cbacks_->on_comm_connect              = on_MPI_Comm_connect             ;
  cbacks_->on_comm_disconnect           = on_MPI_Comm_disconnect          ;
  cbacks_->on_comm_get_parent           = on_MPI_Comm_get_parent          ;
  cbacks_->on_comm_join                 = on_MPI_Comm_join                ;
  cbacks_->on_comm_spawn                = on_MPI_Comm_spawn               ;
  cbacks_->on_comm_spawn_multiple       = on_MPI_Comm_spawn_multiple      ;
  cbacks_->on_lookup_name               = on_MPI_Lookup_name              ;
  cbacks_->on_open_port                 = on_MPI_Open_port                ;
  cbacks_->on_publish_name              = on_MPI_Publish_name             ;
  cbacks_->on_unpublish_name            = on_MPI_Unpublish_name           ;
  cbacks_->on_accumulate                = on_MPI_Accumulate               ;
  cbacks_->on_get                       = on_MPI_Get                      ;
  cbacks_->on_put                       = on_MPI_Put                      ;
  cbacks_->on_win_complete              = on_MPI_Win_complete             ;
  cbacks_->on_win_create                = on_MPI_Win_create               ;
  cbacks_->on_win_fence                 = on_MPI_Win_fence                ;
  cbacks_->on_win_free                  = on_MPI_Win_free                 ;
  cbacks_->on_win_get_group             = on_MPI_Win_get_group            ;
  cbacks_->on_win_lock                  = on_MPI_Win_lock                 ;
  cbacks_->on_win_post                  = on_MPI_Win_post                 ;
  cbacks_->on_win_start                 = on_MPI_Win_start                ;
  cbacks_->on_win_test                  = on_MPI_Win_test                 ;
  cbacks_->on_win_unlock                = on_MPI_Win_unlock               ;
  cbacks_->on_win_wait                  = on_MPI_Win_wait                 ;
  cbacks_->on_alltoallw                 = on_MPI_Alltoallw                ;
  cbacks_->on_exscan                    = on_MPI_Exscan                   ;
  cbacks_->on_add_error_class           = on_MPI_Add_error_class          ;
  cbacks_->on_add_error_code            = on_MPI_Add_error_code           ;
  cbacks_->on_add_error_string          = on_MPI_Add_error_string         ;
  cbacks_->on_comm_call_errhandler      = on_MPI_Comm_call_errhandler     ;
  cbacks_->on_comm_create_keyval        = on_MPI_Comm_create_keyval       ;
  cbacks_->on_comm_delete_attr          = on_MPI_Comm_delete_attr         ;
  cbacks_->on_comm_free_keyval          = on_MPI_Comm_free_keyval         ;
  cbacks_->on_comm_get_attr             = on_MPI_Comm_get_attr            ;
  cbacks_->on_comm_get_name             = on_MPI_Comm_get_name            ;
  cbacks_->on_comm_set_attr             = on_MPI_Comm_set_attr            ;
  cbacks_->on_comm_set_name             = on_MPI_Comm_set_name            ;
  cbacks_->on_file_call_errhandler      = on_MPI_File_call_errhandler     ;
  cbacks_->on_grequest_complete         = on_MPI_Grequest_complete        ;
  cbacks_->on_grequest_start            = on_MPI_Grequest_start           ;
  cbacks_->on_init_thread               = on_MPI_Init_thread              ;
  cbacks_->on_is_thread_main            = on_MPI_Is_thread_main           ;
  cbacks_->on_query_thread              = on_MPI_Query_thread             ;
  cbacks_->on_status_set_cancelled      = on_MPI_Status_set_cancelled     ;
  cbacks_->on_status_set_elements       = on_MPI_Status_set_elements      ;
  cbacks_->on_type_create_keyval        = on_MPI_Type_create_keyval       ;
  cbacks_->on_type_delete_attr          = on_MPI_Type_delete_attr         ;
  cbacks_->on_type_dup                  = on_MPI_Type_dup                 ;
  cbacks_->on_type_free_keyval          = on_MPI_Type_free_keyval         ;
  cbacks_->on_type_get_attr             = on_MPI_Type_get_attr            ;
  cbacks_->on_type_get_contents         = on_MPI_Type_get_contents        ;
  cbacks_->on_type_get_envelope         = on_MPI_Type_get_envelope        ;
  cbacks_->on_type_get_name             = on_MPI_Type_get_name            ;
  cbacks_->on_type_set_attr             = on_MPI_Type_set_attr            ;
  cbacks_->on_type_set_name             = on_MPI_Type_set_name            ;
  cbacks_->on_type_match_size           = on_MPI_Type_match_size          ;
  cbacks_->on_win_call_errhandler       = on_MPI_Win_call_errhandler      ;
  cbacks_->on_win_create_keyval         = on_MPI_Win_create_keyval        ;
  cbacks_->on_win_delete_attr           = on_MPI_Win_delete_attr          ;
  cbacks_->on_win_free_keyval           = on_MPI_Win_free_keyval          ;
  cbacks_->on_win_get_attr              = on_MPI_Win_get_attr             ;
  cbacks_->on_win_get_name              = on_MPI_Win_get_name             ;
  cbacks_->on_win_set_attr              = on_MPI_Win_set_attr             ;
  cbacks_->on_win_set_name              = on_MPI_Win_set_name             ;
  cbacks_->on_alloc_mem                 = on_MPI_Alloc_mem                ;
  cbacks_->on_comm_create_errhandler    = on_MPI_Comm_create_errhandler   ;
  cbacks_->on_comm_get_errhandler       = on_MPI_Comm_get_errhandler      ;
  cbacks_->on_comm_set_errhandler       = on_MPI_Comm_set_errhandler      ;
  cbacks_->on_file_create_errhandler    = on_MPI_File_create_errhandler   ;
  cbacks_->on_file_get_errhandler       = on_MPI_File_get_errhandler      ;
  cbacks_->on_file_set_errhandler       = on_MPI_File_set_errhandler      ;
  cbacks_->on_finalized                 = on_MPI_Finalized                ;
  cbacks_->on_free_mem                  = on_MPI_Free_mem                 ;
  cbacks_->on_get_address               = on_MPI_Get_address              ;
  cbacks_->on_info_create               = on_MPI_Info_create              ;
  cbacks_->on_info_delete               = on_MPI_Info_delete              ;
  cbacks_->on_info_dup                  = on_MPI_Info_dup                 ;
  cbacks_->on_info_free                 = on_MPI_Info_free                ;
  cbacks_->on_info_get                  = on_MPI_Info_get                 ;
  cbacks_->on_info_get_nkeys            = on_MPI_Info_get_nkeys           ;
  cbacks_->on_info_get_nthkey           = on_MPI_Info_get_nthkey          ;
  cbacks_->on_info_get_valuelen         = on_MPI_Info_get_valuelen        ;
  cbacks_->on_info_set                  = on_MPI_Info_set                 ;
  cbacks_->on_pack_external             = on_MPI_Pack_external            ;
  cbacks_->on_pack_external_size        = on_MPI_Pack_external_size       ;
  cbacks_->on_request_get_status        = on_MPI_Request_get_status       ;
  cbacks_->on_type_create_darray        = on_MPI_Type_create_darray       ;
  cbacks_->on_type_create_hindexed      = on_MPI_Type_create_hindexed     ;
  cbacks_->on_type_create_hvector       = on_MPI_Type_create_hvector      ;
  cbacks_->on_type_create_indexed_block = on_MPI_Type_create_indexed_block;
  cbacks_->on_type_create_resized       = on_MPI_Type_create_resized      ;
  cbacks_->on_type_create_struct        = on_MPI_Type_create_struct       ;
  cbacks_->on_type_create_subarray      = on_MPI_Type_create_subarray     ;
  cbacks_->on_type_get_extent           = on_MPI_Type_get_extent          ;
  cbacks_->on_type_get_true_extent      = on_MPI_Type_get_true_extent     ;
  cbacks_->on_unpack_external           = on_MPI_Unpack_external          ;
  cbacks_->on_win_create_errhandler     = on_MPI_Win_create_errhandler    ;
  cbacks_->on_win_get_errhandler        = on_MPI_Win_get_errhandler       ;
  cbacks_->on_win_set_errhandler        = on_MPI_Win_set_errhandler       ;
  cbacks_->on_file_open                 = on_MPI_File_open                ;
  cbacks_->on_file_close                = on_MPI_File_close               ;
  cbacks_->on_file_delete               = on_MPI_File_delete              ;
  cbacks_->on_file_set_size             = on_MPI_File_set_size            ;
  cbacks_->on_file_preallocate          = on_MPI_File_preallocate         ;
  cbacks_->on_file_get_size             = on_MPI_File_get_size            ;
  cbacks_->on_file_get_group            = on_MPI_File_get_group           ;
  cbacks_->on_file_get_amode            = on_MPI_File_get_amode           ;
  cbacks_->on_file_set_info             = on_MPI_File_set_info            ;
  cbacks_->on_file_get_info             = on_MPI_File_get_info            ;
  cbacks_->on_file_set_view             = on_MPI_File_set_view            ;
  cbacks_->on_file_get_view             = on_MPI_File_get_view            ;
  cbacks_->on_file_read_at              = on_MPI_File_read_at             ;
  cbacks_->on_file_read_at_all          = on_MPI_File_read_at_all         ;
  cbacks_->on_file_write_at             = on_MPI_File_write_at            ;
  cbacks_->on_file_write_at_all         = on_MPI_File_write_at_all        ;
  cbacks_->on_file_iread_at             = on_MPI_File_iread_at            ;
  cbacks_->on_file_iwrite_at            = on_MPI_File_iwrite_at           ;
  cbacks_->on_file_read                 = on_MPI_File_read                ;
  cbacks_->on_file_read_all             = on_MPI_File_read_all            ;
  cbacks_->on_file_write                = on_MPI_File_write               ;
  cbacks_->on_file_write_all            = on_MPI_File_write_all           ;
  cbacks_->on_file_iread                = on_MPI_File_iread               ;
  cbacks_->on_file_iwrite               = on_MPI_File_iwrite              ;
  cbacks_->on_file_seek                 = on_MPI_File_seek                ;
  cbacks_->on_file_get_position         = on_MPI_File_get_position        ;
  cbacks_->on_file_get_byte_offset      = on_MPI_File_get_byte_offset     ;
  cbacks_->on_file_read_shared          = on_MPI_File_read_shared         ;
  cbacks_->on_file_write_shared         = on_MPI_File_write_shared        ;
  cbacks_->on_file_iread_shared         = on_MPI_File_iread_shared        ;
  cbacks_->on_file_iwrite_shared        = on_MPI_File_iwrite_shared       ;
  cbacks_->on_file_read_ordered         = on_MPI_File_read_ordered        ;
  cbacks_->on_file_write_ordered        = on_MPI_File_write_ordered       ;
  cbacks_->on_file_seek_shared          = on_MPI_File_seek_shared         ;
  cbacks_->on_file_get_position_shared  = on_MPI_File_get_position_shared ;
  cbacks_->on_file_read_at_all_begin    = on_MPI_File_read_at_all_begin   ;
  cbacks_->on_file_read_at_all_end      = on_MPI_File_read_at_all_end     ;
  cbacks_->on_file_write_at_all_begin   = on_MPI_File_write_at_all_begin  ;
  cbacks_->on_file_write_at_all_end     = on_MPI_File_write_at_all_end    ;
  cbacks_->on_file_read_all_begin       = on_MPI_File_read_all_begin      ;
  cbacks_->on_file_read_all_end         = on_MPI_File_read_all_end        ;
  cbacks_->on_file_write_all_begin      = on_MPI_File_write_all_begin     ;
  cbacks_->on_file_write_all_end        = on_MPI_File_write_all_end       ;
  cbacks_->on_file_read_ordered_begin   = on_MPI_File_read_ordered_begin  ;
  cbacks_->on_file_read_ordered_end     = on_MPI_File_read_ordered_end    ;
  cbacks_->on_file_write_ordered_begin  = on_MPI_File_write_ordered_begin ;
  cbacks_->on_file_write_ordered_end    = on_MPI_File_write_ordered_end   ;
  cbacks_->on_file_get_type_extent      = on_MPI_File_get_type_extent     ;
  cbacks_->on_register_datarep          = on_MPI_Register_datarep         ;
  cbacks_->on_file_set_atomicity        = on_MPI_File_set_atomicity       ;
  cbacks_->on_file_get_atomicity        = on_MPI_File_get_atomicity       ;
  cbacks_->on_file_sync                 = on_MPI_File_sync                ;
  cbacks_->on_iotest                    = on_MPIO_Test                    ;
  cbacks_->on_iowait                    = on_MPIO_Wait                    ;
  cbacks_->on_iotestall                 = on_MPIO_Testall                 ;
  cbacks_->on_iowaitall                 = on_MPIO_Waitall                 ;
  cbacks_->on_iotestany                 = on_MPIO_Testany                 ;
  cbacks_->on_iowaitany                 = on_MPIO_Waitany                 ;
  cbacks_->on_iowaitsome                = on_MPIO_Waitsome                ;
  cbacks_->on_iotestsome                = on_MPIO_Testsome                ;
  lock.unlock();
}

/// This is what happens during unimplemented functions.
int parsedumpi_callbacks::handle_unimplemented(const char*funcname)
{
  static const char *format = ("parsedumpi_callbacks::handle_unimplemented."
                               "  Unimplemented simulator function %s");
  char buf[128];
  switch(action_) {
    case ABORT:
      snprintf(buf, 127, format, funcname);
      buf[127] = '\0';
      throw sprockit::unimplemented_error(buf);
    case PRINT:
      fprintf(stderr, format, funcname);
    case IGNORE:
    default:
      return 1;
  }
}
int parsedumpi_callbacks::handle_unimplemented(const std::string &funcname)
{
  return handle_unimplemented(funcname.c_str());
}


#undef PRFX
static const std::string PRFX("parsedumpi_callbacks::on_");

int parsedumpi_callbacks::
on_MPI_Send(const dumpi_send *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Send");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->send(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                     cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                     cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Recv(const dumpi_recv *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Recv");
  mpi_status stat;
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->recv(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                     cb->get_mpiid(prm->source), cb->get_mpitag(prm->tag),
                     cb->get_mpicomm(prm->comm), &stat);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Get_count(const dumpi_get_count *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get_count");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // We deliberately ignore get_count -- there is no reason to emulate it.
  (void)prm;
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Bsend(const dumpi_bsend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Bsend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->bsend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                      cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Ssend(const dumpi_ssend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Ssend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->ssend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                      cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Rsend(const dumpi_rsend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Rsend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->rsend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                      cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Buffer_attach(const dumpi_buffer_attach *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Buffer_attach");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->buffer_attach(prm->size);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Buffer_detach(const dumpi_buffer_detach *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Buffer_detach");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  int tmp;
  cb->getmpi()->buffer_detach(tmp);
  // We are deliberately not using the trace file arguments.
  (void)prm;
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Isend(const dumpi_isend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Isend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->isend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                      cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Ibsend(const dumpi_ibsend *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Ibsend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->ibsend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                       cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                       cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Issend(const dumpi_issend *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Issend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->issend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                       cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                       cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Irsend(const dumpi_irsend *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Irsend");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->irsend(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                       cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                       cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Irecv(const dumpi_irecv *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Irecv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->irecv(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->source), cb->get_mpitag(prm->tag),
                      cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Wait(const dumpi_wait *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Wait");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req = cb->get_request(prm->request);
  cb->getmpi()->wait(&req);
  cb->nullify_request(prm->request);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Test(const dumpi_test *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Test");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req = cb->get_request(prm->request);
  bool flag;
  if (cb->pessimistic() && prm->flag){
    // I have to explicitly wait on this request
    cb->getmpi()->wait(&req);
  } else {
    cb->getmpi()->test(&req, flag);
    if (flag)
      cb->nullify_request(prm->request);
  }
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Request_free(const dumpi_request_free *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Request_free");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "%s%s: null object pointer",
      PRFX.c_str(), me.c_str());
  }
  // Not implemented yet -- will require some fairly significant
  // reconsideration of how pending requests are managed.
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Waitany(const dumpi_waitany *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if (cb->pessimistic())
    return waitany_pessimistic(prm, thread, cpu, wall, perf, uarg);
  else
    return waitany_optimistic(prm, thread, cpu, wall, perf, uarg);
}

//
// Variant implementation of MPI_Waitany:  pessimistic wait.
//
int parsedumpi_callbacks::
waitany_optimistic(const dumpi_waitany *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  std::vector<mpi_request*> sst_reqs;
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  cb->get_requests(prm->count, prm->requests, sst_reqs);
  int index;
  cb->getmpi()->waitany(sst_reqs, index);
  if (index >= 0){
    cb->remap_request(prm->count, prm->index, index, prm->requests);
  }
  cb->nullify_requests(prm->requests, sst_reqs);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

//
// Variant implementation of MPI_Waitany:  pessimistic wait.
//
int parsedumpi_callbacks::
waitany_pessimistic(const dumpi_waitany *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Waitany{pessimistic}");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(prm->index >= 0 && prm->index < prm->count) {
    // The trace file matched a request -- we will match the same one.
    parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
    if(cb == NULL) {
      spkt_throw_printf(sprockit::null_error,
        "%s%s: null object pointer",
        PRFX.c_str(), me.c_str());
    }
    cb->start_mpi(cpu, wall, perf);
    dumpi_request rid = prm->requests[prm->index];
    mpi_request* req = cb->get_request(rid);
    cb->getmpi()->wait(&req);
    cb->nullify_request(rid);
    cb->end_mpi(cpu, wall, perf);
  }
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testany(const dumpi_testany *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if (cb->pessimistic())
    return testany_pessimistic(prm, thread, cpu, wall, perf, uarg);
  else
    return testany_optimistic(prm, thread, cpu, wall, perf, uarg);
}

//
// Optimistic remapping of an MPI_Testany call.
//
int parsedumpi_callbacks::
testany_optimistic(const dumpi_testany *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Testany{optimistic}");

  // We only bother with this if the parent matched a request
  // (otherwise, we have nothing meaningful to remap).
  if(prm->flag == 1) {
    parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
    if(cb == NULL) {
      spkt_throw_printf(sprockit::null_error,
        "%s%s: null object pointer",
        PRFX.c_str(), me.c_str());
    }
    cb->start_mpi(cpu, wall, perf);
    // Set up.
    std::vector<mpi_request*> sst_reqs;
    cb->get_requests(prm->count, prm->requests, sst_reqs);
    int index;
    bool flag;
    cb->getmpi()->testany(sst_reqs, index, flag);
    if(flag){
      cb->remap_request(prm->count, prm->index, index, prm->requests);
      cb->nullify_requests(prm->requests, sst_reqs);
    }
    cb->end_mpi(cpu, wall, perf);
  }
  return 1;
}

//
// Pessimistic remapping of testany calls.
//
int parsedumpi_callbacks::
testany_pessimistic(const dumpi_testany *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Testany{pessimistic}");
  if(prm->flag == 1 && prm->index >= 0 && prm->index < prm->count) {
    // The trace file matched a request -- we will match the same one.
    parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
    if(cb == NULL) {
      spkt_throw_printf(sprockit::null_error,
        "%s%s: null object pointer",
        PRFX.c_str(), me.c_str());
    }
    cb->start_mpi(cpu, wall, perf);
    dumpi_request rid = prm->requests[prm->index];
    mpi_request* req = cb->get_request(rid);
    cb->getmpi()->wait(&req);
    cb->nullify_request(rid);
    cb->end_mpi(cpu, wall, perf);
  }
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Waitall(const dumpi_waitall *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Waitall");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error, "%s%s: null object pointer", PRFX.c_str(), me.c_str());
  }
  cb->start_mpi(cpu, wall, perf);
  // Set up.
  std::vector<mpi_request*> sst_reqs;
  cb->get_requests(prm->count, prm->requests, sst_reqs);
  cb->getmpi()->waitall(sst_reqs);
  cb->complete_requests(prm->requests, prm->requests + prm->count);
  cb->nullify_requests(prm->requests, sst_reqs);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testall(const dumpi_testall *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Testall");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // Set up.
  std::vector<mpi_request*> sst_reqs;
  // Only go ahead if flag is true (otherwise may can have some odd findings).
  // This also means that we convert the 'testall' to a 'waitall'
  cb->get_requests(prm->count, prm->requests, sst_reqs);
  if(prm->flag && cb->pessimistic()) {
    cb->getmpi()->waitall(sst_reqs);
    cb->complete_requests(prm->requests, prm->requests + prm->count);
  } else {
    bool flag;
    cb->getmpi()->testall(sst_reqs, flag);
    cb->nullify_requests(prm->requests, sst_reqs);
    cb->end_mpi(cpu, wall, perf);
  }
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Waitsome(const dumpi_waitsome *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Waitsome");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // Set up.
  std::vector<mpi_request*> sst_reqs;
  std::vector<int> ind;
  if(cb->pessimistic()) {
    //wait directly on all the requests that finished in the dumpi run
    sst_reqs.resize(prm->outcount);
    for (int i=0; i < prm->outcount; ++i){
      dumpi_request rid = prm->requests[prm->indices[i]];
      sst_reqs[i] = cb->get_request(rid);
    }
    cb->getmpi()->waitall(sst_reqs);
    for (int i=0; i < prm->outcount; ++i){
      dumpi_request rid = prm->requests[prm->indices[i]];
      cb->nullify_request(rid);
    }
  }
  else {
    cb->get_requests(prm->count, prm->requests, sst_reqs);
    cb->getmpi()->waitsome(sst_reqs, ind);
    cb->nullify_requests(prm->requests, sst_reqs);
    cb->remap_requests(prm->count, prm->outcount, prm->indices, ind, prm->requests);
  }
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testsome(const dumpi_testsome *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Testsome");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // Set up.

  if (cb->pessimistic()) {
    //make sure all reqs completed in dumpi are completed in trace
    for (int i=0; i < prm->outcount; ++i){
      dumpi_request rid = prm->requests[prm->indices[i]];
      mpi_request* req = cb->get_request(rid);
      cb->getmpi()->wait(&req);
      cb->nullify_request(rid);
    }
  }
  else {
    std::vector<mpi_request*> sst_reqs;
    std::vector<int> ind;
    cb->get_requests(prm->count, prm->requests, sst_reqs);
    cb->getmpi()->testsome(sst_reqs, ind);
    cb->remap_requests(prm->count, prm->outcount, prm->indices, ind, prm->requests);
    cb->nullify_requests(prm->requests, sst_reqs);
  }
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Iprobe(const dumpi_iprobe *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Iprobe");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  /*
  // Set up.
  if(prm->flag) {
    bool flag;
    mpi_status* stat;
    cb->getmpi()->iprobe(cb->get_mpiid(prm->source),
                         cb->get_mpitag(prm->tag),
                         cb->get_mpicomm(prm->comm),
                         flag, stat);
  }
  */
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Probe(const dumpi_probe *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Probe");

  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  /*
  mpi_status* stat;
  bool flag;
  cb->getmpi()->iprobe(cb->get_mpiid(prm->source),
                       cb->get_mpitag(prm->tag),
                       cb->get_mpicomm(prm->comm),
                       flag, stat);
  */
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Cancel(const dumpi_cancel *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cancel");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req = cb->get_request(prm->request);
  cb->getmpi()->cancel(req);
  cb->complete_request(prm->request);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Test_cancelled(const dumpi_test_cancelled *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Test_cancelled");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // We are deliberately ignoring this one.
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Send_init(const dumpi_send_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Send_init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->send_init(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                          cb->get_mpiid(prm->dest),
                          cb->get_mpitag(prm->tag),
                          cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Bsend_init(const dumpi_bsend_init *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Bsend_init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->bsend_init(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                           cb->get_mpiid(prm->dest),
                           cb->get_mpitag(prm->tag),
                           cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Ssend_init(const dumpi_ssend_init *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Ssend_init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->ssend_init(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                           cb->get_mpiid(prm->dest),
                           cb->get_mpitag(prm->tag),
                           cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Rsend_init(const dumpi_rsend_init *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Rsend_init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->rsend_init(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                           cb->get_mpiid(prm->dest),
                           cb->get_mpitag(prm->tag),
                           cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Recv_init(const dumpi_recv_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Recv_init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_request* req;
  cb->getmpi()->recv_init(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                          cb->get_mpiid(prm->source),
                          cb->get_mpitag(prm->tag),
                          cb->get_mpicomm(prm->comm), req);
  cb->store_request(prm->request, req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Start(const dumpi_start *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Start");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->start(cb->get_request(prm->request));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Startall(const dumpi_startall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Startall");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  std::vector<mpi_request*> req;
  cb->start_mpi(cpu, wall, perf);
  cb->get_requests(prm->count, prm->requests, req);
  cb->getmpi()->startall(req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Sendrecv(const dumpi_sendrecv *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Sendrecv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_status stat;
  cb->getmpi()->sendrecv(prm->sendcount,
                         cb->get_mpitype(prm->sendcount, prm->sendtype),
                         cb->get_mpiid(prm->dest),
                         cb->get_mpitag(prm->sendtag),
                         prm->recvcount,
                         cb->get_mpitype(prm->recvcount, prm->recvtype),
                         cb->get_mpiid(prm->source),
                         cb->get_mpitag(prm->recvtag),
                         cb->get_mpicomm(prm->comm), &stat);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Sendrecv_replace(const dumpi_sendrecv_replace *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Sendrecv_replace");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_status stat;
  cb->getmpi()->sendrecv_replace(prm->count,
                                 cb->get_mpitype(prm->count, prm->datatype),
                                 cb->get_mpiid(prm->dest),
                                 cb->get_mpitag(prm->sendtag),
                                 cb->get_mpiid(prm->source),
                                 cb->get_mpitag(prm->recvtag),
                                 cb->get_mpicomm(prm->comm), &stat);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_contiguous(const dumpi_type_contiguous *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_contiguous");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error, PRFX, me, ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_type_id newtype;
  cb->getmpi()->type_contiguous(prm->count,
                                cb->get_mpitype(prm->count, prm->oldtype),
                                newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_vector(const dumpi_type_vector *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_vector");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error, PRFX, me, ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_type_id oldtype = cb->get_mpitype(1, prm->oldtype);
  mpi_type* old_type_obj = cb->getmpi()->type_from_id(oldtype);

  int size = prm->count * prm->blocklength * old_type_obj->packed_size();
  if(prm->newtype < cb->datatype_sizes_.count) {
    if(size != cb->datatype_sizes_.size[prm->newtype])
    {
      size = cb->datatype_sizes_.size[prm->newtype];
    }
  }
  mpi_type_id newtype;
  cb->getmpi()->type_vector(prm->count, prm->blocklength, 0, oldtype, newtype, false);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_hvector(const dumpi_type_hvector *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_hvector");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }

  spkt_throw(sprockit::unimplemented_error,
    "mpiapi::type_hvector");

  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_indexed(const dumpi_type_indexed *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_indexed");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_type_id newtype;
  std::vector<int> disps; disps.assign(prm->indices, prm->indices + prm->count);
  mpi_type_id oldtype = cb->get_mpitype(1, prm->oldtype);
  cb->getmpi()->type_indexed(prm->count, prm->lengths, disps,
    oldtype, newtype, true, MPI_COMBINER_INDEXED);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_hindexed(const dumpi_type_hindexed *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_hindexed");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }

  spkt_throw(sprockit::unimplemented_error,
    "mpiapi::type_hindexed");

  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);

  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_struct(const dumpi_type_struct *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_struct");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  std::vector<int> lengths(prm->lengths, prm->lengths + prm->count);
  std::vector<int> indices(prm->indices, prm->indices + prm->count);
  std::vector<mpi_type_id> oldtypes; oldtypes.resize(prm->count);
  for (int i=0; i < prm->count; ++i){
    oldtypes[i] = cb->get_mpitype(1, prm->oldtypes[i]);
  }
  mpi_type_id newtype;
  cb->getmpi()->type_struct(prm->count, lengths, indices, oldtypes, newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Address(const dumpi_address *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Address");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_extent(const dumpi_type_extent *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_extent");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_size(const dumpi_type_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_size");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->type_size(cb->get_mpitype(0, prm->datatype));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_lb(const dumpi_type_lb *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_lb");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_ub(const dumpi_type_ub *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_ub");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_commit(const dumpi_type_commit *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_commit");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->type_commit(cb->get_mpitype(1, prm->datatype));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_free(const dumpi_type_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_free");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->type_free(cb->get_mpitype(0, prm->datatype));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Get_elements(const dumpi_get_elements *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get_elements");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Pack(const dumpi_pack *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Pack");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Unpack(const dumpi_unpack *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Unpack");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Pack_size(const dumpi_pack_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Pack_size");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Barrier(const dumpi_barrier *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Barrier");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->barrier(cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Bcast(const dumpi_bcast *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Bcast");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->bcast(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                      cb->get_mpiid(prm->root), cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Gather(const dumpi_gather *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Gather");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->gather(prm->sendcount,
                       cb->get_mpitype(prm->sendcount, prm->sendtype),
                       prm->recvcount,
                       cb->get_mpitype(prm->recvcount, prm->recvtype),
                       cb->get_mpiid(prm->root), cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Gatherv(const dumpi_gatherv *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Gatherv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if (cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);

  std::vector<int> recvcounts;
  mpi_type_id sendtype = cb->get_mpitype(prm->sendcount, prm->sendtype);
  mpi_type_id recvtype;
  if (prm->commrank == prm->root){
    recvcounts.insert(recvcounts.end(), prm->recvcounts,
                      prm->recvcounts + prm->commsize);
    recvtype = cb->get_mpitype(1, prm->recvtype);
  } else {
    //ignore the recvtype, just use the sendtype
    //it won't be used anyway
    recvtype = sendtype;
  }

  cb->getmpi()->gatherv(prm->sendcount,
                        sendtype,
                        recvcounts,
                        recvtype,
                        cb->get_mpiid(prm->root), 
                        cb->get_mpicomm(prm->comm));

  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scatter(const dumpi_scatter *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Scatter");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->scatter(prm->sendcount,
                        cb->get_mpitype(prm->sendcount, prm->sendtype),
                        prm->recvcount,
                        cb->get_mpitype(prm->recvcount, prm->recvtype),
                        cb->get_mpiid(prm->root), cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scatterv(const dumpi_scatterv *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Scatterv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  std::vector<int> sendcounts;
  if(prm->commrank == prm->root)
    sendcounts.insert(sendcounts.end(), prm->sendcounts,
                      prm->sendcounts + prm->commsize);
  cb->getmpi()->scatterv(sendcounts, cb->get_mpitype(1, prm->sendtype),
                         prm->recvcount,
                         cb->get_mpitype(prm->recvcount, prm->recvtype),
                         cb->get_mpiid(prm->root),
                         cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Allgather(const dumpi_allgather *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Allgather");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->allgather(prm->sendcount,
                          cb->get_mpitype(prm->sendcount, prm->sendtype),
                          prm->recvcount,
                          cb->get_mpitype(prm->recvcount, prm->recvtype),
                          cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Allgatherv(const dumpi_allgatherv *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Allgatherv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  std::vector<int> recvcounts(prm->recvcounts, prm->recvcounts+prm->commsize);
  cb->getmpi()->allgatherv(prm->sendcount,
                           cb->get_mpitype(prm->sendcount, prm->sendtype),
                           recvcounts,
                           cb->get_mpitype(1, prm->recvtype),
                           cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Alltoall(const dumpi_alltoall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Alltoall");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->alltoall(prm->sendcount,
                         cb->get_mpitype(prm->sendcount, prm->sendtype),
                         prm->recvcount,
                         cb->get_mpitype(prm->recvcount, prm->recvtype),
                         cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Alltoallv(const dumpi_alltoallv *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Alltoallv");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  std::vector<int> sendcounts(prm->sendcounts, prm->sendcounts+prm->commsize);
  std::vector<int> recvcounts(prm->recvcounts, prm->recvcounts+prm->commsize);
  cb->getmpi()->alltoallv(sendcounts, cb->get_mpitype(1, prm->sendtype),
                          recvcounts, cb->get_mpitype(1, prm->recvtype),
                          cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Reduce(const dumpi_reduce *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Reduce");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->reduce(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                       cb->get_mpiop(prm->op), cb->get_mpiid(prm->root),
                       cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Op_create(const dumpi_op_create *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Op_create");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Op_free(const dumpi_op_free *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Op_free");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Allreduce(const dumpi_allreduce *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Allreduce");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->allreduce(prm->count,
                          cb->get_mpitype(prm->count, prm->datatype),
                          cb->get_mpiop(prm->op),
                          cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Reduce_scatter(const dumpi_reduce_scatter *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Reduce_scatter");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  std::vector<int> recvcounts(prm->recvcounts, prm->recvcounts+prm->commsize);
  cb->getmpi()->reduce_scatter(recvcounts,
                               cb->get_mpitype(1, prm->datatype),
                               cb->get_mpiop(prm->op),
                               cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scan(const dumpi_scan *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Scan");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->scan(prm->count, cb->get_mpitype(prm->count, prm->datatype),
                     cb->get_mpiop(prm->op), cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Group_size(const dumpi_group_size *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_size");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_rank(const dumpi_group_rank *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_rank");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_translate_ranks(const dumpi_group_translate_ranks *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  static const std::string me("MPI_Group_translate_ranks");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_compare(const dumpi_group_compare *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_compare");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_group(const dumpi_comm_group *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_group");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_union(const dumpi_group_union *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_union");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_intersection(const dumpi_group_intersection *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_intersection");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_difference(const dumpi_group_difference *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_difference");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_incl(const dumpi_group_incl *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_incl");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error, "%s%s: null object pointer", PRFX.c_str(), me.c_str());
  }

  //int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  mpi_group* newgrp;
  mpi_group* oldgrp = cb->get_mpigroup(prm->group);
  cb->getmpi()->group_incl(prm->ranks, prm->count, oldgrp, newgrp);
  cb->add_mpigroup(prm->newgroup, newgrp);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Group_excl(const dumpi_group_excl *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_excl");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_range_incl(const dumpi_group_range_incl *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_range_incl");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_range_excl(const dumpi_group_range_excl *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_range_excl");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Group_free(const dumpi_group_free *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Group_free");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_size(const dumpi_comm_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_size");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->comm_size(cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_rank(const dumpi_comm_rank *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_rank");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->comm_rank(cb->get_mpicomm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_compare(const dumpi_comm_compare *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_compare");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_dup(const dumpi_comm_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_dup");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_comm* newcomm;
  cb->getmpi()->comm_dup(cb->get_mpicomm(prm->oldcomm), newcomm);
  cb->add_mpicomm(prm->newcomm, newcomm);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_create(const dumpi_comm_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_create");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  // UNIMPLEMENTED.
  //throw sprockit::unimplemented_error(PRFX + me + ":  MPI_Comm_create does not work " +
  //                         "since groups are currently not present.");
  mpi_comm* newcomm;
  cb->getmpi()->comm_create(cb->get_mpicomm(prm->oldcomm),
                            cb->get_mpigroup(prm->group), newcomm);
  if(newcomm != mpi_comm::comm_null) 
    cb->add_mpicomm(prm->newcomm, newcomm);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_split(const dumpi_comm_split *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_split");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_comm* newcomm;
  cb->getmpi()->comm_split(cb->get_mpicomm(prm->oldcomm),
                           prm->color, prm->key, newcomm);
  cb->add_mpicomm(prm->newcomm, newcomm);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_free(const dumpi_comm_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_free");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->comm_free(cb->get_mpicomm(prm->comm));
  cb->erase_mpicomm(prm->comm);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_test_inter(const dumpi_comm_test_inter *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_test_inter");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_remote_size(const dumpi_comm_remote_size *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_remote_size");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_remote_group(const dumpi_comm_remote_group *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_remote_group");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Intercomm_create(const dumpi_intercomm_create *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Intercomm_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Intercomm_merge(const dumpi_intercomm_merge *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Intercomm_merge");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Keyval_create(const dumpi_keyval_create *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Keyval_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Keyval_free(const dumpi_keyval_free *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Keyval_free");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Attr_put(const dumpi_attr_put *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Attr_put");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Attr_get(const dumpi_attr_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Attr_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Attr_delete(const dumpi_attr_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Attr_delete");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Topo_test(const dumpi_topo_test *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Topo_test");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_create(const dumpi_cart_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Dims_create(const dumpi_dims_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Dims_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graph_create(const dumpi_graph_create *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Graph_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graphdims_get(const dumpi_graphdims_get *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Graphdims_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graph_get(const dumpi_graph_get *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Graph_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cartdim_get(const dumpi_cartdim_get *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cartdim_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_get(const dumpi_cart_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_rank(const dumpi_cart_rank *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_rank");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_coords(const dumpi_cart_coords *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_coords");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graph_neighbors_count(const dumpi_graph_neighbors_count *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf,void *uarg)
{
  static const std::string me("MPI_Graph_neighbors_count");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graph_neighbors(const dumpi_graph_neighbors *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Graph_neighbors");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_shift(const dumpi_cart_shift *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_shift");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_sub(const dumpi_cart_sub *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_sub");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Cart_map(const dumpi_cart_map *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Cart_map");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Graph_map(const dumpi_graph_map *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Graph_map");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Get_processor_name(const dumpi_get_processor_name *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get_processor_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Get_version(const dumpi_get_version *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get_version");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Errhandler_create(const dumpi_errhandler_create *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Errhandler_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Errhandler_set(const dumpi_errhandler_set *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Errhandler_set");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Errhandler_get(const dumpi_errhandler_get *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Errhandler_get");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Errhandler_free(const dumpi_errhandler_free *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Errhandler_free");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Error_string(const dumpi_error_string *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Error_string");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Error_class(const dumpi_error_class *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Error_class");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Wtime(const dumpi_wtime *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Wtime");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->wtime();
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Wtick(const dumpi_wtick *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Wtick");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Init(const dumpi_init *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Init");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  // cb->getmpi()->init();
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Finalize(const dumpi_finalize *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Finalize");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->finalize();
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Initialized(const dumpi_initialized *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Initialized");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->initialized();
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Abort(const dumpi_abort *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Abort");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Close_port(const dumpi_close_port *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Close_port");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_accept(const dumpi_comm_accept *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_accept");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_connect(const dumpi_comm_connect *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_connect");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_disconnect(const dumpi_comm_disconnect *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_disconnect");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_get_parent(const dumpi_comm_get_parent *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_get_parent");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_join(const dumpi_comm_join *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_join");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_spawn(const dumpi_comm_spawn *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_spawn");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_spawn_multiple(const dumpi_comm_spawn_multiple *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_spawn_multiple");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Lookup_name(const dumpi_lookup_name *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Lookup_name");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Open_port(const dumpi_open_port *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Open_port");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Publish_name(const dumpi_publish_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Publish_name");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Unpublish_name(const dumpi_unpublish_name *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Unpublish_name");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Accumulate(const dumpi_accumulate *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Accumulate");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Get(const dumpi_get *prm, uint16_t thread,
           const dumpi_time *cpu, const dumpi_time *wall,
           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Put(const dumpi_put *prm, uint16_t thread,
           const dumpi_time *cpu, const dumpi_time *wall,
           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Put");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_complete(const dumpi_win_complete *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_complete");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_create(const dumpi_win_create *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_fence(const dumpi_win_fence *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_fence");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_free(const dumpi_win_free *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_free");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_get_group(const dumpi_win_get_group *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_get_group");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_lock(const dumpi_win_lock *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_lock");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_post(const dumpi_win_post *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_post");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_start(const dumpi_win_start *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_start");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_test(const dumpi_win_test *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_test");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_unlock(const dumpi_win_unlock *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_unlock");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_wait(const dumpi_win_wait *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_wait");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Alltoallw(const dumpi_alltoallw *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Alltoallw");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Exscan(const dumpi_exscan *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Exscan");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Add_error_class(const dumpi_add_error_class *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Add_error_class");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Add_error_code(const dumpi_add_error_code *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Add_error_code");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Add_error_string(const dumpi_add_error_string *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Add_error_string");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_call_errhandler(const dumpi_comm_call_errhandler *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_call_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_create_keyval(const dumpi_comm_create_keyval *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_create_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_delete_attr(const dumpi_comm_delete_attr *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_delete_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_free_keyval(const dumpi_comm_free_keyval *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_free_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_get_attr(const dumpi_comm_get_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_get_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_get_name(const dumpi_comm_get_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_get_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_set_attr(const dumpi_comm_set_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_set_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_set_name(const dumpi_comm_set_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_set_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_call_errhandler(const dumpi_file_call_errhandler *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_call_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Grequest_complete(const dumpi_grequest_complete *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Grequest_complete");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Grequest_start(const dumpi_grequest_start *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Grequest_start");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Init_thread(const dumpi_init_thread *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Init_thread");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  // Only warn if the user is trying to do something more than MPI_THREAD_FUNNELED
  if(prm->required != DUMPI_THREAD_SINGLE and prm->required != DUMPI_THREAD_FUNNELED) {
    cerr0 << "WARNING: MPI_Init_thread called requesting thread support level ";
    switch(prm->required) {
    case DUMPI_THREAD_SERIALIZED:
      cerr0 << "MPI_THREAD_SERIALIZED";
      break;
    case DUMPI_THREAD_MULTIPLE:
      cerr0 << "MPI_THREAD_MULTIPLE";
      break;
    default:
      cerr0 << "other than MPI_THREAD_SINGLE or MPI_THREAD_FUNNELED";
      break;
    };
    cerr0 << ".\n Compute times reported will be meaningless if MPI calls are made from different threads." << std::endl;
  }
  //throw sprockit::unimplemented_error(PRFX + me + ":  MPI_Init_thread not implemented.");
  cb->start_mpi(cpu, wall, perf);
  // cb->getmpi()->init();
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Is_thread_main(const dumpi_is_thread_main *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Is_thread_main");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Query_thread(const dumpi_query_thread *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Query_thread");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Status_set_cancelled(const dumpi_status_set_cancelled *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Status_set_cancelled");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Status_set_elements(const dumpi_status_set_elements *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Status_set_elements");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_keyval(const dumpi_type_create_keyval *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_delete_attr(const dumpi_type_delete_attr *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_delete_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_dup(const dumpi_type_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_dup");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  cb->start_mpi(cpu, wall, perf);
  mpi_type_id newtype;
  cb->getmpi()->type_dup(cb->get_mpitype(0, prm->oldtype), newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_free_keyval(const dumpi_type_free_keyval *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_free_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_get_attr(const dumpi_type_get_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_get_contents(const dumpi_type_get_contents *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_contents");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_get_envelope(const dumpi_type_get_envelope *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_envelope");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_get_name(const dumpi_type_get_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_set_attr(const dumpi_type_set_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_set_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_set_name(const dumpi_type_set_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_set_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_match_size(const dumpi_type_match_size *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_match_size");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_call_errhandler(const dumpi_win_call_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_call_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_create_keyval(const dumpi_win_create_keyval *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_create_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_delete_attr(const dumpi_win_delete_attr *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_delete_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_free_keyval(const dumpi_win_free_keyval *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_free_keyval");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_get_attr(const dumpi_win_get_attr *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_get_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_get_name(const dumpi_win_get_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_get_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_set_attr(const dumpi_win_set_attr *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_set_attr");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_set_name(const dumpi_win_set_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_set_name");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Alloc_mem(const dumpi_alloc_mem *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Alloc_mem");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_create_errhandler
(const dumpi_comm_create_errhandler *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_create_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_get_errhandler(const dumpi_comm_get_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_get_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Comm_set_errhandler(const dumpi_comm_set_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Comm_set_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_create_errhandler
(const dumpi_file_create_errhandler *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_create_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_errhandler(const dumpi_file_get_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_set_errhandler(const dumpi_file_set_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_set_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Finalized(const dumpi_finalized *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Finalized");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->finalized();
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Free_mem(const dumpi_free_mem *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Free_mem");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Get_address(const dumpi_get_address *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Get_address");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_create(const dumpi_info_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_create");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_delete(const dumpi_info_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_delete");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_dup(const dumpi_info_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_dup");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_free(const dumpi_info_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_free");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_get(const dumpi_info_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_get");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_get_nkeys(const dumpi_info_get_nkeys *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_get_nkeys");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_get_nthkey(const dumpi_info_get_nthkey *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_get_nthkey");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_get_valuelen(const dumpi_info_get_valuelen *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_get_valuelen");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Info_set(const dumpi_info_set *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Info_set");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Pack_external(const dumpi_pack_external *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Pack_external");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Pack_external_size(const dumpi_pack_external_size *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Pack_external_size");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Request_get_status(const dumpi_request_get_status *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Request_get_status");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_darray(const dumpi_type_create_darray *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_darray");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_hindexed(const dumpi_type_create_hindexed *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_hindexed");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_hvector(const dumpi_type_create_hvector *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_hvector");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_indexed_block
(const dumpi_type_create_indexed_block *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_indexed_block");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_resized(const dumpi_type_create_resized *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_resized");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Type_create_struct(const dumpi_type_create_struct *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_struct");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }

  spkt_throw(sprockit::unimplemented_error,
    "mpiapi::type_create_struct");

  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_create_subarray(const dumpi_type_create_subarray *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_create_subarray");
  spkt_throw_printf(sprockit::unimplemented_error,
        "parsedumpi_callbacks::on_MPI_Type_create_subarray: not implemented");
}

int parsedumpi_callbacks::
on_MPI_Type_get_extent(const dumpi_type_get_extent *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_extent");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  //cb->start_mpi(cpu, wall, perf);
  //cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_get_true_extent(const dumpi_type_get_true_extent *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Type_get_true_extent");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  //cb->start_mpi(cpu, wall, perf);
  //cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Unpack_external(const dumpi_unpack_external *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Unpack_external");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  //cb->start_mpi(cpu, wall, perf);
  //cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_create_errhandler(const dumpi_win_create_errhandler *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  static const std::string me("MPI_Win_create_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_get_errhandler(const dumpi_win_get_errhandler *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_get_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Win_set_errhandler(const dumpi_win_set_errhandler *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Win_set_errhandler");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_open(const dumpi_file_open *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_open");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_close(const dumpi_file_close *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_close");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_delete(const dumpi_file_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_delete");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_set_size(const dumpi_file_set_size *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_set_size");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_preallocate(const dumpi_file_preallocate *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_preallocate");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_size(const dumpi_file_get_size *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_size");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_group(const dumpi_file_get_group *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_group");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_amode(const dumpi_file_get_amode *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_amode");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_set_info(const dumpi_file_set_info *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_set_info");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_info(const dumpi_file_get_info *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_info");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_set_view(const dumpi_file_set_view *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_set_view");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_view(const dumpi_file_get_view *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_view");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_at(const dumpi_file_read_at *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_at");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all(const dumpi_file_read_at_all *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_at_all");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_at(const dumpi_file_write_at *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_at");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all(const dumpi_file_write_at_all *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_at_all");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iread_at(const dumpi_file_iread_at *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iread_at");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iwrite_at(const dumpi_file_iwrite_at *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iwrite_at");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read(const dumpi_file_read *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_all(const dumpi_file_read_all *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_all");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write(const dumpi_file_write *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_all(const dumpi_file_write_all *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_all");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iread(const dumpi_file_iread *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iread");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iwrite(const dumpi_file_iwrite *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iwrite");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_seek(const dumpi_file_seek *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_seek");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_position(const dumpi_file_get_position *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_position");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_byte_offset(const dumpi_file_get_byte_offset *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_byte_offset");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_shared(const dumpi_file_read_shared *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_shared");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_shared(const dumpi_file_write_shared *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_shared");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iread_shared(const dumpi_file_iread_shared *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iread_shared");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_iwrite_shared(const dumpi_file_iwrite_shared *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_iwrite_shared");

  
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered(const dumpi_file_read_ordered *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_ordered");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered(const dumpi_file_write_ordered *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_ordered");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_seek_shared(const dumpi_file_seek_shared *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_seek_shared");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_position_shared
(const dumpi_file_get_position_shared *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_position_shared");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all_begin
(const dumpi_file_read_at_all_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_at_all_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all_end(const dumpi_file_read_at_all_end *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_at_all_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all_begin
(const dumpi_file_write_at_all_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_at_all_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all_end(const dumpi_file_write_at_all_end *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf,void *uarg)
{
  static const std::string me("MPI_File_write_at_all_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_all_begin(const dumpi_file_read_all_begin *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_all_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_all_end(const dumpi_file_read_all_end *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_all_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_all_begin(const dumpi_file_write_all_begin *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_all_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_all_end(const dumpi_file_write_all_end *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_all_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered_begin
(const dumpi_file_read_ordered_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_read_ordered_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered_end(const dumpi_file_read_ordered_end *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  static const std::string me("MPI_File_read_ordered_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered_begin
(const dumpi_file_write_ordered_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_ordered_begin");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered_end
(const dumpi_file_write_ordered_end *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_write_ordered_end");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_type_extent(const dumpi_file_get_type_extent *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_type_extent");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_Register_datarep(const dumpi_register_datarep *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_Register_datarep");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_set_atomicity(const dumpi_file_set_atomicity *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_set_atomicity");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_get_atomicity(const dumpi_file_get_atomicity *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_get_atomicity");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPI_File_sync(const dumpi_file_sync *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Test(const dumpio_test *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Wait(const dumpio_wait *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Testall(const dumpio_testall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Waitall(const dumpio_waitall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Testany(const dumpio_testany *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Waitany(const dumpio_waitany *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Waitsome(const dumpio_waitsome *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}

int parsedumpi_callbacks::
on_MPIO_Testsome(const dumpio_testsome *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  static const std::string me("MPI_File_sync");
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    throw sprockit::null_error(PRFX + me + ":  Null object pointer.");
  }
  int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return retval;
}


}
} // end of namespace sstmac


