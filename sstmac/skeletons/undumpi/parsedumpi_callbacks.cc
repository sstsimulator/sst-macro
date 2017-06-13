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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/skeletons/undumpi/parsedumpi_callbacks.h>
#include <sprockit/errors.h>
#include <sprockit/output.h>
#include <cstring>
#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_types.h>

#define ENABLE_MPI_ALLGATHER    1
#define ENABLE_MPI_ALLTOALL     1
#define ENABLE_MPI_ALLREDUCE    1
#define ENABLE_MPI_BARRIER      1
#define ENABLE_MPI_BCAST        1
#define ENABLE_MPI_GATHER       1
#define ENABLE_MPI_REDUCE       1
#define ENABLE_MPI_SCATTER      1
#define ENABLE_MPI_SEND         1
#define ENABLE_MPI_SENDRECV     1
#define ENABLE_MPI_REQUEST_FREE 1
#define ENABLE_MPI_START        1
#define ENABLE_MPI_STARTALL     1
#define ENABLE_MPI_ISEND        1
#define ENABLE_MPI_RECV         1
#define ENABLE_MPI_IRECV        1
#define ENABLE_MPI_ALLGATHERV   1
#define ENABLE_MPI_ALLTOALLV    1
#define ENABLE_MPI_GATHERV      1
#define ENABLE_MPI_SCATTERV     1
#define ENABLE_MPI_WAIT         1
#define ENABLE_MPI_WAITALL      1
#define ENABLE_MPI_WAITANY      1
#define ENABLE_MPI_WAITSOME     1
#define ENABLE_MPI_COMM_DUP     1
#define ENABLE_MPI_COMM_SIZE    1
#define ENABLE_MPI_COMM_CREATE  1
#define ENABLE_MPI_COMM_GROUP   1
#define ENABLE_MPI_COMM_SPLIT   1
#define ENABLE_MPI_COMM_FREE    1
#define ENABLE_MPI_GROUP_INCL   1
#define ENABLE_MPI_TEST         1
#define ENABLE_MPI_TESTANY      1
#define ENABLE_MPI_TESTALL      1
#define ENABLE_MPI_TESTSOME     1


namespace sstmac {
namespace sw {
extern void api_lock();
extern void api_unlock();
}}

namespace sumi {

/// The shared callback pointer array.
libundumpi_callbacks *parsedumpi_callbacks::cbacks_ = nullptr;


int pass(void* uarg,
  const dumpi_time *cpu,
  const dumpi_time *wall,
  const dumpi_perfinfo *perf,
  const char* fxn)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "%s: null callback pointer", fxn);
  }
  //function means nothing to a trace
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int not_implemented(const char* fxn)
{
  spkt_throw(sprockit::unimplemented_error, fxn);
  return 0;
}

/// Populate callbacks.
parsedumpi_callbacks::
parsedumpi_callbacks(parsedumpi *parent) :
#if SSTMAC_COMM_SYNC_STATS
  exact_mpi_times_(parent->exact_mpi_times()),
#endif
  parent_(parent),
  initialized_(false)
{
  sstmac::sw::api_lock();
  if(cbacks_ == NULL) {
    set_callbacks();
  }
  trace_compute_start_.sec = -1;
  init_maps();
  memset(&datatype_sizes_, 0, sizeof(dumpi_sizeof));
  sstmac::sw::api_unlock();
  parent->mpi()->set_generate_ids(false);
}

parsedumpi_callbacks::~parsedumpi_callbacks()
{
}

/// Start parsing.
void
parsedumpi_callbacks::parse_stream(
  const std::string &fname,
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
  int retval = undumpi_read_stream_full(
                  fname.c_str(), profile, cbacks_, this, print_progress, percent_terminate);
  if(retval != 1) {
    spkt_throw(sprockit::io_error, here + ":  Failed reading dumpi stream.\n");
  }
  undumpi_close(profile);
}

/// Initialize maps.
void parsedumpi_callbacks::init_maps()
{
  // Built-in mpitypes.
  mpitype_[DUMPI_DATATYPE_ERROR] = MPI_DATATYPE_NULL;
  mpitype_[DUMPI_DATATYPE_NULL] = MPI_DATATYPE_NULL;
  mpitype_[DUMPI_CHAR] = MPI_CHAR;
  mpitype_[DUMPI_SIGNED_CHAR] = MPI_SIGNED_CHAR;
  mpitype_[DUMPI_UNSIGNED_CHAR] = MPI_UNSIGNED_CHAR;
  mpitype_[DUMPI_BYTE] = MPI_BYTE;
  mpitype_[DUMPI_WCHAR] = MPI_WCHAR;
  mpitype_[DUMPI_SHORT] = MPI_SHORT;
  mpitype_[DUMPI_UNSIGNED_SHORT] = MPI_UNSIGNED_SHORT;
  mpitype_[DUMPI_INT] = MPI_INT;
  mpitype_[DUMPI_UNSIGNED] = MPI_UNSIGNED;
  mpitype_[DUMPI_LONG] = MPI_LONG;
  mpitype_[DUMPI_UNSIGNED_LONG] = MPI_UNSIGNED_LONG;
  mpitype_[DUMPI_FLOAT] = MPI_FLOAT;
  mpitype_[DUMPI_DOUBLE] = MPI_DOUBLE;
  mpitype_[DUMPI_LONG_DOUBLE] = MPI_LONG_DOUBLE;
  mpitype_[DUMPI_LONG_LONG_INT] = MPI_LONG_LONG_INT;
  mpitype_[DUMPI_UNSIGNED_LONG_LONG] = MPI_UNSIGNED_LONG_LONG;
  mpitype_[DUMPI_LONG_LONG] = MPI_LONG_LONG;
  mpitype_[DUMPI_PACKED] = MPI_PACKED;
  mpitype_[DUMPI_LB] = MPI_LB;
  mpitype_[DUMPI_UB] = MPI_UB;
  mpitype_[DUMPI_FLOAT_INT] = MPI_FLOAT_INT;
  mpitype_[DUMPI_DOUBLE_INT] = MPI_DOUBLE_INT;
  mpitype_[DUMPI_LONG_INT] = MPI_LONG_INT;
  mpitype_[DUMPI_SHORT_INT] = MPI_SHORT_INT;
  mpitype_[DUMPI_2INT] = MPI_2INT;
  mpitype_[DUMPI_LONG_DOUBLE_INT] = MPI_LONG_DOUBLE_INT;
}


// Convert a dumpi time difference into a timestamp.
inline sstmac::timestamp deltat(const dumpi_clock &left, const dumpi_clock &right)
{
  static const int64_t billion(1e9);
  return sstmac::timestamp::exact_nsec(billion*(left.sec-right.sec) +
                               (left.nsec - right.nsec));
}

/// Indicate that we are starting an MPI call.
void parsedumpi_callbacks::
start_mpi(const dumpi_time *cpu, const dumpi_time *wall,
          const dumpi_perfinfo *perf)
{
  if (!initialized_) return;

#if SSTMAC_COMM_SYNC_STATS
  if (exact_mpi_times_){
    auto deltaSec = wall->stop.sec - wall->start.sec;
    auto deltaNsec = wall->stop.nsec - wall->start.nsec;
    sstmac::timestamp traceMPItime(deltaSec, deltaNsec);
    parent_->mpi()->set_next_call_length(traceMPItime);
  }
#endif
  if(trace_compute_start_.sec >= 0) {
    // This is not the first MPI call -- simulate a compute
    if(perf != NULL && perf->count > 0) {
      spkt_throw(sprockit::unimplemented_error,
        "DUMPI perfctr compute: only compatible with time");

      // We get here if we are using the processor model.
      if(size_t(perf->count) != perfctr_compute_start_.size())
        spkt_throw(sprockit::illformed_error, "parsedumpi_callbacks::start_mpi:  "
                              "Number of active perfcounters changed between calls");
      for(int i = 0; i < perf->count; ++i) {
        int64_t evtval = perf->invalue[i] - perfctr_compute_start_[i];
        if(evtval < 0) {
          spkt_throw(sprockit::illformed_error, "parsedumpi_callbacks::start_mpi:  "
                                "Performance counter moved backward between calls");
        }
      }
    } else {
      // We get here if we are not using processor modeling.
      sstmac::timestamp thetime = (parent_->timescaling_ * deltat(wall->start, trace_compute_start_));
      if(thetime.ticks() < 0) {
        // It turns out that RSQ can actually screw up on times
        // (both wall time and cpu time are wrong for final MPI barrier
        // in 016-4x2x2_run01/dumpi0014.bin collected on 08/07/09
        //do nothing
      } else {
        parent_->compute(thetime);
      }
    }
  }
}

/// Indicate that we have completed an MPI call.
void parsedumpi_callbacks::
end_mpi(const dumpi_time *cpu, const dumpi_time *wall,
        const dumpi_perfinfo *perf)
{
  trace_compute_start_ = wall->stop;
  if(perf) {
    perfctr_compute_start_.resize(perf->count);
    for(int i = 0; i < perf->count; ++i) {
      perfctr_compute_start_[i] = perf->outvalue[i];
    }
  }
}

static MPI_Request
translate_request(dumpi_request req)
{
  if (req == DUMPI_REQUEST_NULL) return MPI_REQUEST_NULL;
  else return req;
}

static MPI_Group
translate_group(dumpi_group grp)
{
  if (grp == DUMPI_FIRST_USER_GROUP) return MPI_GROUP_WORLD;
  else return grp;
}

static MPI_Comm
translate_comm(dumpi_comm comm)
{
  if (comm == DUMPI_COMM_WORLD) return MPI_COMM_WORLD;
  else return comm;
}


/// Get an mpiid.
/// Special handling for MPI_ROOT and MPI_ANY_SOURCE.
int parsedumpi_callbacks::get_mpiid(dumpi_source id)
{
  if(id == DUMPI_ANY_SOURCE) {
    return MPI_ANY_SOURCE;
  }
  else if(id == DUMPI_ROOT) {
    return MPI_ROOT;
  }
  else {
    return id;
  }
}

/// Add a new mpi type.
void parsedumpi_callbacks::
add_mpitype(dumpi_datatype id, MPI_Datatype mpit)
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
MPI_Datatype
parsedumpi_callbacks::get_mpitype(dumpi_datatype id)
{
  mpitype_map_t::iterator it = mpitype_.find(id);
  if(it == mpitype_.end()) {
    if(id < datatype_sizes_.count) {
      int size = datatype_sizes_.size[id];
      mpitype_[id] = mpi_type::builtins[size].id;
      printf("Remapped dumpi type %d to builtin of size %d\n", id, size);
    }
    else {
      cerrn << sprockit::printf("Warning: no match for datatype id %d - assuming double\n", int(id));
      mpitype_[id] = MPI_DOUBLE;
    }
    return mpitype_[id];
  }
  return it->second;
}

//
// Get a group of request handles.
//
MPI_Datatype*
parsedumpi_callbacks::get_mpitypes(int count, const dumpi_datatype *dumpitypes)
{
  static_assert(sizeof(MPI_Datatype) <= sizeof(dumpi_datatype), "sizes");
  MPI_Datatype* mpitypes = (MPI_Datatype*) (const_cast<dumpi_datatype*>(dumpitypes));
  if(count < 0){
    spkt_throw(sprockit::value_error,
      "parsedumpi_callbacks::get_requests: negative request count");
  }
  for(int i = 0; i < count; ++i) {
    mpitypes[i] = get_mpitype(dumpitypes[i]);
  }
  return mpitypes;
}

/// Set all callbacks.
void parsedumpi_callbacks::set_callbacks()
{
  //libundumpi_clear_callbacks(cbacks_);
  if(cbacks_ == NULL) {
    cbacks_ = new libundumpi_callbacks;
    libundumpi_clear_callbacks(cbacks_);
  }
  cbacks_->on_send                      = on_MPI_Send                     ;
  cbacks_->on_bsend                     = on_MPI_Bsend                    ;
  cbacks_->on_ssend                     = on_MPI_Ssend                    ;
  cbacks_->on_rsend                     = on_MPI_Rsend                    ;
  cbacks_->on_recv                      = on_MPI_Recv                     ;
  cbacks_->on_get_count                 = on_MPI_Get_count                ;
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
}

int parsedumpi_callbacks::
on_MPI_Bsend(const dumpi_bsend *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send* newprm = (const dumpi_send*) prm;
  return on_MPI_Send(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Ssend(const dumpi_ssend *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send* newprm = (const dumpi_send*) prm;
  return on_MPI_Send(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Rsend(const dumpi_rsend *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send* newprm = (const dumpi_send*) prm;
  return on_MPI_Send(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Send(const dumpi_send *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_SEND
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "MPI_Send: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->send(NULL, prm->count, cb->get_mpitype(prm->datatype),
                     cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                     translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Recv(const dumpi_recv *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_RECV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
      "MPI_Recv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->recv(NULL, prm->count, cb->get_mpitype(prm->datatype),
                     cb->get_mpiid(prm->source), cb->get_mpitag(prm->tag),
                     translate_comm(prm->comm), MPI_STATUS_IGNORE);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Get_count(const dumpi_get_count *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Get_count");
}

int parsedumpi_callbacks::
on_MPI_Buffer_attach(const dumpi_buffer_attach *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Buffer_attach");
}

int parsedumpi_callbacks::
on_MPI_Buffer_detach(const dumpi_buffer_detach *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Buffer_detach");
}

int parsedumpi_callbacks::
on_MPI_Issend(const dumpi_issend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_isend* newprm = (const dumpi_isend*) prm;
  return on_MPI_Isend(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Irsend(const dumpi_irsend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_isend* newprm = (const dumpi_isend*) prm;
  return on_MPI_Isend(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Ibsend(const dumpi_ibsend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_isend* newprm = (const dumpi_isend*) prm;
  return on_MPI_Isend(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Isend(const dumpi_isend *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ISEND
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Isend: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = prm->request;
  cb->getmpi()->isend(NULL, prm->count, cb->get_mpitype(prm->datatype),
                      cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                      translate_comm(prm->comm), &req);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Irecv(const dumpi_irecv *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_IRECV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Irecv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = prm->request;
  cb->getmpi()->irecv(NULL, prm->count, cb->get_mpitype(prm->datatype),
                      cb->get_mpiid(prm->source), cb->get_mpitag(prm->tag),
                      translate_comm(prm->comm), &req);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Wait(const dumpi_wait *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_WAIT
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Irecv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = translate_request(prm->request);
  cb->getmpi()->wait(&req, MPI_STATUS_IGNORE);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Test(const dumpi_test *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_TEST
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Test: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  if (prm->flag){
    //this needs to complete - to keep trace 'valid'
    //we have to make sure this request is complete
    MPI_Request req = translate_request(prm->request);
    cb->getmpi()->wait(&req, MPI_STATUS_IGNORE);
  } else;  //otherwise - don't do anything - this isn't finished
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Request_free(const dumpi_request_free *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Request_free");
}

int parsedumpi_callbacks::
on_MPI_Waitany(const dumpi_waitany *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  return waitany_pessimistic(prm, thread, cpu, wall, perf, uarg);
}

//
// Variant implementation of MPI_Waitany:  pessimistic wait.
//
int parsedumpi_callbacks::
waitany_pessimistic(const dumpi_waitany *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_WAITANY
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Waitany: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  if(prm->index >= 0 && prm->index < prm->count) {
    dumpi_request rid = prm->requests[prm->index];
    MPI_Request req = translate_request(rid);
    cb->getmpi()->wait(&req, MPI_STATUS_IGNORE);
  }
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testany(const dumpi_testany *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  return testany_pessimistic(prm, thread, cpu, wall, perf, uarg);
}

//
// Pessimistic remapping of testany calls.
//
int parsedumpi_callbacks::
testany_pessimistic(const dumpi_testany *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_TESTANY
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Testany: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  if(prm->flag == 1 && prm->index >= 0 && prm->index < prm->count) {
    // The trace file matched a request -- we will match the same one.
    dumpi_request rid = prm->requests[prm->index];
    MPI_Request req = translate_request(rid);
    cb->getmpi()->wait(&req, MPI_STATUS_IGNORE);
  }
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Waitall(const dumpi_waitall *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_WAITALL
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Waitall: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  for (int i=0; i < prm->count; ++i){
    prm->requests[i] = translate_request(prm->requests[i]);
  }
  cb->getmpi()->waitall(prm->count, prm->requests, MPI_STATUSES_IGNORE);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testall(const dumpi_testall *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_TESTALL
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Testall: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  // Only go ahead if flag is true (otherwise may have some odd findings).
  // This also means that we convert the 'testall' to a 'waitall'
  if(prm->flag) {
    for (int i=0; i < prm->count; ++i)
      prm->requests[i] = translate_request(prm->requests[i]);
    cb->getmpi()->waitall(prm->count, prm->requests, MPI_STATUSES_IGNORE);
  }
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Waitsome(const dumpi_waitsome *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_WAITSOME
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Waitsome: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  for (int i=0; i < prm->outcount; ++i){
    dumpi_request rid = prm->requests[prm->indices[i]];
    MPI_Request req = translate_request(rid);
    cb->getmpi()->wait(&req, MPI_STATUSES_IGNORE);
  }
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Testsome(const dumpi_testsome *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_TESTSOME
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Testsome: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  for (int i=0; i < prm->outcount; ++i){
    dumpi_request rid = prm->requests[prm->indices[i]];
    MPI_Request req = translate_request(rid);
    cb->getmpi()->wait(&req, MPI_STATUS_IGNORE);
  }
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Iprobe(const dumpi_iprobe *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Iprobe: null callback pointer");
  }
  //this accomplishes nothing - a trace cannot care about the result of Iprobe
  cb->start_mpi(cpu, wall, perf);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Probe(const dumpi_probe *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "MPI_Probe: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  //I should stay here and spin until I get a matching probe
  cb->getmpi()->probe(cb->get_mpiid(prm->source),
    cb->get_mpitag(prm->tag),
    translate_comm(prm->comm),
    MPI_STATUS_IGNORE);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Cancel(const dumpi_cancel *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Cancel");
}

int parsedumpi_callbacks::
on_MPI_Test_cancelled(const dumpi_test_cancelled *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Test_cancelled");
}

int parsedumpi_callbacks::
on_MPI_Send_init(const dumpi_send_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
    "MPI_Send_init: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = prm->request;
  cb->getmpi()->send_init(NULL, prm->count, cb->get_mpitype(prm->datatype),
                    cb->get_mpiid(prm->dest), cb->get_mpitag(prm->tag),
                    translate_comm(prm->comm), &req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Bsend_init(const dumpi_bsend_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send_init* newprm = (const dumpi_send_init*) prm;
  return on_MPI_Send_init(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Rsend_init(const dumpi_rsend_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send_init* newprm = (const dumpi_send_init*) prm;
  return on_MPI_Send_init(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Ssend_init(const dumpi_ssend_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  const dumpi_send_init* newprm = (const dumpi_send_init*) prm;
  return on_MPI_Send_init(newprm, thread, cpu, wall, perf, uarg);
}

int parsedumpi_callbacks::
on_MPI_Recv_init(const dumpi_recv_init *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
  spkt_throw_printf(sprockit::null_error,
  "MPI_Recv_init: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = prm->request;
  cb->getmpi()->recv_init(NULL, prm->count, cb->get_mpitype(prm->datatype),
                          cb->get_mpiid(prm->source), cb->get_mpitag(prm->tag),
                          translate_comm(prm->comm), &req);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Start(const dumpi_start *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_START
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
    "MPI_Start: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Request req = translate_request(prm->request);
  cb->getmpi()->start(&req);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Startall(const dumpi_startall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_STARTALL
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
    "MPI_Startall: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->startall(prm->count, prm->requests);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Sendrecv(const dumpi_sendrecv *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_SENDRECV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
    "MPI_Sendrecv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->sendrecv(NULL, prm->sendcount, cb->get_mpitype(prm->sendtype),
                        cb->get_mpiid(prm->dest), cb->get_mpitag(prm->sendtag),
                        NULL, prm->recvcount, cb->get_mpitype(prm->recvtype),
                        cb->get_mpiid(prm->source), cb->get_mpitag(prm->recvtag),
                        translate_comm(prm->comm), MPI_STATUS_IGNORE);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Sendrecv_replace(const dumpi_sendrecv_replace *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_SENDRECV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw_printf(sprockit::null_error,
    "MPI_Sendrecv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->sendrecv(NULL, prm->count, cb->get_mpitype(prm->datatype),
                        cb->get_mpiid(prm->dest), cb->get_mpitag(prm->sendtag),
                        NULL, prm->count, cb->get_mpitype(prm->datatype),
                        cb->get_mpiid(prm->source), cb->get_mpitag(prm->recvtag),
                        translate_comm(prm->comm), MPI_STATUS_IGNORE);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_contiguous(const dumpi_type_contiguous *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_contiguous: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype newtype;
  cb->getmpi()->type_contiguous(prm->count,
                                cb->get_mpitype(prm->oldtype),
                                &newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_vector(const dumpi_type_vector *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_vector: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype oldtype = cb->get_mpitype(prm->oldtype);
  mpi_type* old_type_obj = cb->getmpi()->type_from_id(oldtype);

  //this is to account for some crazy bug that I don't remember
  int size = prm->count * prm->blocklength * old_type_obj->packed_size();
  if(prm->newtype < cb->datatype_sizes_.count) {
    if(size != cb->datatype_sizes_.size[prm->newtype])
    {
      size = cb->datatype_sizes_.size[prm->newtype];
    }
  }
  MPI_Datatype newtype;
  cb->getmpi()->type_vector(prm->count, prm->blocklength, 0, oldtype, &newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_hvector(const dumpi_type_hvector *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_hvector");
}

int parsedumpi_callbacks::
on_MPI_Type_indexed(const dumpi_type_indexed *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_indexed: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype newtype;
  std::vector<int> disps; disps.assign(prm->indices, prm->indices + prm->count);
  MPI_Datatype oldtype = cb->get_mpitype(prm->oldtype);
  cb->getmpi()->type_indexed(prm->count, prm->lengths, prm->indices,
    oldtype, &newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_hindexed(const dumpi_type_hindexed *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_hindexed");
}

int parsedumpi_callbacks::
on_MPI_Type_struct(const dumpi_type_struct *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_struct: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype* oldtypes = cb->get_mpitypes(prm->count, prm->oldtypes);
  MPI_Datatype newtype;
  cb->getmpi()->type_create_struct(prm->count, prm->lengths, prm->indices, oldtypes, &newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Address(const dumpi_address *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Address");
}

int parsedumpi_callbacks::
on_MPI_Type_extent(const dumpi_type_extent *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Type_extent");
}

int parsedumpi_callbacks::
on_MPI_Type_size(const dumpi_type_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Type_size");
}

int parsedumpi_callbacks::
on_MPI_Type_lb(const dumpi_type_lb *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Type_lb");
}

int parsedumpi_callbacks::
on_MPI_Type_ub(const dumpi_type_ub *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Type_ub");
}

int parsedumpi_callbacks::
on_MPI_Type_commit(const dumpi_type_commit *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_commit: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype dtype = cb->get_mpitype(prm->datatype);
  cb->getmpi()->type_commit(&dtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Type_free(const dumpi_type_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_free: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype dtype = cb->get_mpitype(prm->datatype);
  cb->getmpi()->type_free(&dtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Get_elements(const dumpi_get_elements *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Get_elements");
}

int parsedumpi_callbacks::
on_MPI_Pack(const dumpi_pack *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Pack");
}

int parsedumpi_callbacks::
on_MPI_Unpack(const dumpi_unpack *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Unpack");
}

int parsedumpi_callbacks::
on_MPI_Pack_size(const dumpi_pack_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Pack_size");
}

int parsedumpi_callbacks::
on_MPI_Barrier(const dumpi_barrier *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_BARRIER
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Barrier: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->barrier(translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Bcast(const dumpi_bcast *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_BCAST
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Bcast: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->bcast(prm->count, cb->get_mpitype(prm->datatype),
                      cb->get_mpiid(prm->root), translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Gather(const dumpi_gather *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_GATHER
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Gather: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->gather(prm->sendcount,
                       cb->get_mpitype(prm->sendtype),
                       prm->recvcount,
                       cb->get_mpitype(prm->recvtype),
                       cb->get_mpiid(prm->root),
                       translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Gatherv(const dumpi_gatherv *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_GATHERV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Gatherv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);

  MPI_Datatype sendtype = cb->get_mpitype(prm->sendtype);
  MPI_Datatype recvtype;
  if (prm->commrank == prm->root){
    recvtype = cb->get_mpitype(prm->recvtype);
  } else {
    //ignore the recvtype, just use the sendtype
    //it won't be used anyway
    recvtype = sendtype;
  }

  cb->getmpi()->gatherv(prm->sendcount,
                        sendtype,
                        prm->recvcounts,
                        recvtype,
                        cb->get_mpiid(prm->root), 
                        translate_comm(prm->comm));

  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scatter(const dumpi_scatter *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_SCATTER
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Scatter: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->scatter(prm->sendcount,
                        cb->get_mpitype(prm->sendtype),
                        prm->recvcount,
                        cb->get_mpitype(prm->recvtype),
                        cb->get_mpiid(prm->root),
                        translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scatterv(const dumpi_scatterv *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_SCATTERV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Scatterv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->scatterv(prm->sendcounts,
                         cb->get_mpitype(prm->sendtype),
                         prm->recvcount,
                         cb->get_mpitype(prm->recvtype),
                         cb->get_mpiid(prm->root),
                         translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Allgather(const dumpi_allgather *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ALLGATHER
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Allgather: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->allgather(prm->sendcount,
                          cb->get_mpitype(prm->sendtype),
                          prm->recvcount,
                          cb->get_mpitype(prm->recvtype),
                          translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Allgatherv(const dumpi_allgatherv *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ALLGATHERV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Allgatherv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->allgatherv(prm->sendcount,
                           cb->get_mpitype(prm->sendtype),
                           prm->recvcounts,
                           cb->get_mpitype(prm->recvtype),
                           translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Alltoall(const dumpi_alltoall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ALLTOALL
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Alltoall: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->alltoall(prm->sendcount,
                         cb->get_mpitype(prm->sendtype),
                         prm->recvcount,
                         cb->get_mpitype(prm->recvtype),
                         translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Alltoallv(const dumpi_alltoallv *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ALLTOALLV
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Alltoallv: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->alltoallv(prm->sendcounts, cb->get_mpitype(prm->sendtype),
                          prm->recvcounts, cb->get_mpitype(prm->recvtype),
                          translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Reduce(const dumpi_reduce *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_REDUCE
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Reduce: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->reduce(prm->count, cb->get_mpitype(prm->datatype),
                       DUMPI_OP, //this doesn't matter
                       cb->get_mpiid(prm->root),
                       translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Allreduce(const dumpi_allreduce *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_ALLREDUCE
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Allreduce: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->allreduce(prm->count,
                          cb->get_mpitype(prm->datatype),
                          DUMPI_OP, //this doesn't matter
                          translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Reduce_scatter(const dumpi_reduce_scatter *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Reduce_scatter: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->reduce_scatter(prm->recvcounts,
                               cb->get_mpitype(prm->datatype),
                               DUMPI_OP,
                               translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Scan(const dumpi_scan *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Scan: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->scan(prm->count, cb->get_mpitype(prm->datatype),
                     DUMPI_OP, translate_comm(prm->comm));
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Op_create(const dumpi_op_create *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Op_create");
}

int parsedumpi_callbacks::
on_MPI_Op_free(const dumpi_op_free *prm, uint16_t thread,
               const dumpi_time *cpu, const dumpi_time *wall,
               const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Op_free");
}

int parsedumpi_callbacks::
on_MPI_Group_size(const dumpi_group_size *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_size");
}

int parsedumpi_callbacks::
on_MPI_Group_rank(const dumpi_group_rank *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_rank");
}

int parsedumpi_callbacks::
on_MPI_Group_translate_ranks(const dumpi_group_translate_ranks *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_translate_ranks");
}

int parsedumpi_callbacks::
on_MPI_Group_compare(const dumpi_group_compare *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_compare");
}

int parsedumpi_callbacks::
on_MPI_Comm_group(const dumpi_comm_group *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_group");
}

int parsedumpi_callbacks::
on_MPI_Group_union(const dumpi_group_union *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_union");
}

int parsedumpi_callbacks::
on_MPI_Group_intersection(const dumpi_group_intersection *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_intersection");
}

int parsedumpi_callbacks::
on_MPI_Group_difference(const dumpi_group_difference *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_difference");
}

int parsedumpi_callbacks::
on_MPI_Group_incl(const dumpi_group_incl *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_GROUP_INCL
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Group_incl: null callback pointer");
  }

  //int retval = cb->handle_unimplemented(me);
  cb->start_mpi(cpu, wall, perf);
  MPI_Group ingrp = translate_group(prm->group);
  MPI_Group outgrp = prm->newgroup;
  cb->getmpi()->group_incl(ingrp, prm->count, prm->ranks, &outgrp);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Group_excl(const dumpi_group_excl *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_excl");
}

int parsedumpi_callbacks::
on_MPI_Group_range_incl(const dumpi_group_range_incl *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_range_incl");
}

int parsedumpi_callbacks::
on_MPI_Group_range_excl(const dumpi_group_range_excl *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_range_excl");
}

int parsedumpi_callbacks::
on_MPI_Group_free(const dumpi_group_free *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Group_free");
}

int parsedumpi_callbacks::
on_MPI_Comm_size(const dumpi_comm_size *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_size");
}

int parsedumpi_callbacks::
on_MPI_Comm_rank(const dumpi_comm_rank *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_rank");
}

int parsedumpi_callbacks::
on_MPI_Comm_compare(const dumpi_comm_compare *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_compare");
}

int parsedumpi_callbacks::
on_MPI_Comm_dup(const dumpi_comm_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_COMM_DUP
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Group_incl: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Comm newcomm = prm->newcomm;
  cb->getmpi()->comm_dup(translate_comm(prm->oldcomm), &newcomm);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_create(const dumpi_comm_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_COMM_CREATE
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Comm_create: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Comm newcomm = prm->newcomm;
  cb->getmpi()->comm_create(translate_comm(prm->oldcomm),
                            prm->group, &newcomm);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_split(const dumpi_comm_split *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_COMM_SPLIT
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Comm_split: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Comm newcomm = prm->newcomm;
  cb->getmpi()->comm_split(translate_comm(prm->oldcomm),
                           prm->color, prm->key, &newcomm);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_free(const dumpi_comm_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
#if ENABLE_MPI_COMM_FREE
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Comm_free: null callback pointer");
  }

  cb->start_mpi(cpu, wall, perf);
  MPI_Comm comm = prm->comm;
  cb->getmpi()->comm_free(&comm);
  cb->end_mpi(cpu, wall, perf);
#endif
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Comm_test_inter(const dumpi_comm_test_inter *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_test_inter");
}

int parsedumpi_callbacks::
on_MPI_Comm_remote_size(const dumpi_comm_remote_size *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_remote_size");
}

int parsedumpi_callbacks::
on_MPI_Comm_remote_group(const dumpi_comm_remote_group *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_remote_group");
}

int parsedumpi_callbacks::
on_MPI_Intercomm_create(const dumpi_intercomm_create *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Intercomm_create");
}

int parsedumpi_callbacks::
on_MPI_Intercomm_merge(const dumpi_intercomm_merge *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Intercomm_merge");
}

int parsedumpi_callbacks::
on_MPI_Keyval_create(const dumpi_keyval_create *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Keyval_create");
}

int parsedumpi_callbacks::
on_MPI_Keyval_free(const dumpi_keyval_free *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Keyval_free");
}

int parsedumpi_callbacks::
on_MPI_Attr_put(const dumpi_attr_put *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Attr_put");
}

int parsedumpi_callbacks::
on_MPI_Attr_get(const dumpi_attr_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Attr_get");
}

int parsedumpi_callbacks::
on_MPI_Attr_delete(const dumpi_attr_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Attr_delete");
}

int parsedumpi_callbacks::
on_MPI_Topo_test(const dumpi_topo_test *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Topo_test");
}

int parsedumpi_callbacks::
on_MPI_Cart_create(const dumpi_cart_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("MPI_Cart_create");
}

int parsedumpi_callbacks::
on_MPI_Dims_create(const dumpi_dims_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Dims_create");
}

int parsedumpi_callbacks::
on_MPI_Graph_create(const dumpi_graph_create *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graph_create");
}

int parsedumpi_callbacks::
on_MPI_Graphdims_get(const dumpi_graphdims_get *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graphdims_get");
}

int parsedumpi_callbacks::
on_MPI_Graph_get(const dumpi_graph_get *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graph_get");
}

int parsedumpi_callbacks::
on_MPI_Cartdim_get(const dumpi_cartdim_get *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cartdim_get");
}

int parsedumpi_callbacks::
on_MPI_Cart_get(const dumpi_cart_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_get");
}

int parsedumpi_callbacks::
on_MPI_Cart_rank(const dumpi_cart_rank *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_rank");
}

int parsedumpi_callbacks::
on_MPI_Cart_coords(const dumpi_cart_coords *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_coords");
}

int parsedumpi_callbacks::
on_MPI_Graph_neighbors_count(const dumpi_graph_neighbors_count *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf,void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graph_neighbors_count");
}

int parsedumpi_callbacks::
on_MPI_Graph_neighbors(const dumpi_graph_neighbors *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graph_neighbors");
}

int parsedumpi_callbacks::
on_MPI_Cart_shift(const dumpi_cart_shift *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_shift");
}

int parsedumpi_callbacks::
on_MPI_Cart_sub(const dumpi_cart_sub *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_sub");
}

int parsedumpi_callbacks::
on_MPI_Cart_map(const dumpi_cart_map *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Cart_map");
}

int parsedumpi_callbacks::
on_MPI_Graph_map(const dumpi_graph_map *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Graph_map");
}

int parsedumpi_callbacks::
on_MPI_Get_processor_name(const dumpi_get_processor_name *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Get_processor_name");
}

int parsedumpi_callbacks::
on_MPI_Get_version(const dumpi_get_version *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Get_version");
}

int parsedumpi_callbacks::
on_MPI_Errhandler_create(const dumpi_errhandler_create *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Errhandler_create");
}

int parsedumpi_callbacks::
on_MPI_Errhandler_set(const dumpi_errhandler_set *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Errhandler_set");
}

int parsedumpi_callbacks::
on_MPI_Errhandler_get(const dumpi_errhandler_get *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Errhandler_get");
}

int parsedumpi_callbacks::
on_MPI_Errhandler_free(const dumpi_errhandler_free *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Errhandler_free");
}

int parsedumpi_callbacks::
on_MPI_Error_string(const dumpi_error_string *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Error_string");
}

int parsedumpi_callbacks::
on_MPI_Error_class(const dumpi_error_class *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Error_class");
}

int parsedumpi_callbacks::
on_MPI_Wtime(const dumpi_wtime *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Wtime");
}

int parsedumpi_callbacks::
on_MPI_Wtick(const dumpi_wtick *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Wtick");
}

int parsedumpi_callbacks::
on_MPI_Type_dup(const dumpi_type_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Type_dup: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  MPI_Datatype newtype;
  cb->getmpi()->type_dup(cb->get_mpitype(prm->oldtype), &newtype);
  cb->add_mpitype(prm->newtype, newtype);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Init(const dumpi_init *prm, uint16_t thread,
            const dumpi_time *cpu, const dumpi_time *wall,
            const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Init: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->init(const_cast<int*>(&prm->argc), const_cast<char***>(&prm->argv));
  cb->end_mpi(cpu, wall, perf);
  cb->set_initialized(true);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Init_thread(const dumpi_init_thread *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Init_thread: null callback pointer");
  }
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
  cb->start_mpi(cpu, wall, perf);
  int provided;
  int argc = prm->argc;
  char** argv = const_cast<char**>(prm->argv);
  cb->getmpi()->init_thread(&argc, &argv, 
                        prm->required, &provided);
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Finalize(const dumpi_finalize *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  parsedumpi_callbacks *cb = reinterpret_cast<parsedumpi_callbacks*>(uarg);
  if(cb == NULL) {
    spkt_throw(sprockit::null_error,
      "on_MPI_Finalize: null callback pointer");
  }
  cb->start_mpi(cpu, wall, perf);
  cb->getmpi()->finalize();
  cb->end_mpi(cpu, wall, perf);
  return 1;
}

int parsedumpi_callbacks::
on_MPI_Initialized(const dumpi_initialized *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Abort(const dumpi_abort *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Abort");
}

int parsedumpi_callbacks::
on_MPI_Close_port(const dumpi_close_port *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Close_port");
}

int parsedumpi_callbacks::
on_MPI_Comm_accept(const dumpi_comm_accept *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_accept");
}

int parsedumpi_callbacks::
on_MPI_Comm_connect(const dumpi_comm_connect *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Comm_connect");
}

int parsedumpi_callbacks::
on_MPI_Comm_disconnect(const dumpi_comm_disconnect *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Comm_disconnect");
}

int parsedumpi_callbacks::
on_MPI_Comm_get_parent(const dumpi_comm_get_parent *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Comm_get_parent");
}

int parsedumpi_callbacks::
on_MPI_Comm_join(const dumpi_comm_join *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Comm_join");
}

int parsedumpi_callbacks::
on_MPI_Comm_spawn(const dumpi_comm_spawn *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Comm_spawn");
}

int parsedumpi_callbacks::
on_MPI_Comm_spawn_multiple(const dumpi_comm_spawn_multiple *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Comm_spawn_multiple");
}

int parsedumpi_callbacks::
on_MPI_Lookup_name(const dumpi_lookup_name *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Lookup_name");
}

int parsedumpi_callbacks::
on_MPI_Open_port(const dumpi_open_port *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Open_port");
}

int parsedumpi_callbacks::
on_MPI_Publish_name(const dumpi_publish_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Publish_name");
}

int parsedumpi_callbacks::
on_MPI_Unpublish_name(const dumpi_unpublish_name *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Unpublish_name");
}

int parsedumpi_callbacks::
on_MPI_Accumulate(const dumpi_accumulate *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Accumulate");
}

int parsedumpi_callbacks::
on_MPI_Get(const dumpi_get *prm, uint16_t thread,
           const dumpi_time *cpu, const dumpi_time *wall,
           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Get");
}

int parsedumpi_callbacks::
on_MPI_Put(const dumpi_put *prm, uint16_t thread,
           const dumpi_time *cpu, const dumpi_time *wall,
           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Put");
}

int parsedumpi_callbacks::
on_MPI_Win_complete(const dumpi_win_complete *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_complete");
}

int parsedumpi_callbacks::
on_MPI_Win_create(const dumpi_win_create *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_create");
}

int parsedumpi_callbacks::
on_MPI_Win_fence(const dumpi_win_fence *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_fence");
}

int parsedumpi_callbacks::
on_MPI_Win_free(const dumpi_win_free *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_free");
}

int parsedumpi_callbacks::
on_MPI_Win_get_group(const dumpi_win_get_group *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_get_group");
}

int parsedumpi_callbacks::
on_MPI_Win_lock(const dumpi_win_lock *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_lock");
}

int parsedumpi_callbacks::
on_MPI_Win_post(const dumpi_win_post *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_post");
}

int parsedumpi_callbacks::
on_MPI_Win_start(const dumpi_win_start *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_start");
}

int parsedumpi_callbacks::
on_MPI_Win_test(const dumpi_win_test *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_test");
}

int parsedumpi_callbacks::
on_MPI_Win_unlock(const dumpi_win_unlock *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_unlock");
}

int parsedumpi_callbacks::
on_MPI_Win_wait(const dumpi_win_wait *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Win_wait");
}

int parsedumpi_callbacks::
on_MPI_Alltoallw(const dumpi_alltoallw *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Alltoallw");
}

int parsedumpi_callbacks::
on_MPI_Exscan(const dumpi_exscan *prm, uint16_t thread,
              const dumpi_time *cpu, const dumpi_time *wall,
              const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Exscan");
}

int parsedumpi_callbacks::
on_MPI_Add_error_class(const dumpi_add_error_class *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Add_error_code(const dumpi_add_error_code *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Add_error_string(const dumpi_add_error_string *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_call_errhandler(const dumpi_comm_call_errhandler *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_create_keyval(const dumpi_comm_create_keyval *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_delete_attr(const dumpi_comm_delete_attr *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_free_keyval(const dumpi_comm_free_keyval *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_get_attr(const dumpi_comm_get_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_get_name(const dumpi_comm_get_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_set_attr(const dumpi_comm_set_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Comm_set_name(const dumpi_comm_set_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_File_call_errhandler(const dumpi_file_call_errhandler *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Grequest_complete(const dumpi_grequest_complete *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Grequest_start(const dumpi_grequest_start *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Add_error_class");
}

int parsedumpi_callbacks::
on_MPI_Is_thread_main(const dumpi_is_thread_main *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Query_thread(const dumpi_query_thread *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Status_set_cancelled(const dumpi_status_set_cancelled *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Status_set_elements(const dumpi_status_set_elements *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_create_keyval(const dumpi_type_create_keyval *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_delete_attr(const dumpi_type_delete_attr *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_free_keyval(const dumpi_type_free_keyval *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_get_attr(const dumpi_type_get_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_get_contents(const dumpi_type_get_contents *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_get_envelope(const dumpi_type_get_envelope *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_get_name(const dumpi_type_get_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_set_attr(const dumpi_type_set_attr *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_set_name(const dumpi_type_set_name *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_match_size(const dumpi_type_match_size *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_call_errhandler(const dumpi_win_call_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_create_keyval(const dumpi_win_create_keyval *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_delete_attr(const dumpi_win_delete_attr *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_free_keyval(const dumpi_win_free_keyval *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_get_attr(const dumpi_win_get_attr *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_get_name(const dumpi_win_get_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_set_attr(const dumpi_win_set_attr *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_set_name(const dumpi_win_set_name *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Alloc_mem(const dumpi_alloc_mem *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Comm_create_errhandler
(const dumpi_comm_create_errhandler *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Comm_get_errhandler(const dumpi_comm_get_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Comm_set_errhandler(const dumpi_comm_set_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_create_errhandler
(const dumpi_file_create_errhandler *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_errhandler(const dumpi_file_get_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_set_errhandler(const dumpi_file_set_errhandler *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Finalized(const dumpi_finalized *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Free_mem(const dumpi_free_mem *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Get_address(const dumpi_get_address *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_create(const dumpi_info_create *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_delete(const dumpi_info_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_dup(const dumpi_info_dup *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_free(const dumpi_info_free *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_get(const dumpi_info_get *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_get_nkeys(const dumpi_info_get_nkeys *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_get_nthkey(const dumpi_info_get_nthkey *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_get_valuelen(const dumpi_info_get_valuelen *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Info_set(const dumpi_info_set *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Pack_external(const dumpi_pack_external *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Pack_external_size(const dumpi_pack_external_size *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Request_get_status(const dumpi_request_get_status *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_create_darray(const dumpi_type_create_darray *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_darray");
}

int parsedumpi_callbacks::
on_MPI_Type_create_hindexed(const dumpi_type_create_hindexed *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_hindexed");
}

int parsedumpi_callbacks::
on_MPI_Type_create_hvector(const dumpi_type_create_hvector *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_hvector");
}

int parsedumpi_callbacks::
on_MPI_Type_create_indexed_block
(const dumpi_type_create_indexed_block *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_indexed_block");
}

int parsedumpi_callbacks::
on_MPI_Type_create_resized(const dumpi_type_create_resized *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_resized");
}

int parsedumpi_callbacks::
on_MPI_Type_create_struct(const dumpi_type_create_struct *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_struct");
}

int parsedumpi_callbacks::
on_MPI_Type_create_subarray(const dumpi_type_create_subarray *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_Type_create_subarray");
}

int parsedumpi_callbacks::
on_MPI_Type_get_extent(const dumpi_type_get_extent *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Type_get_true_extent(const dumpi_type_get_true_extent *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Unpack_external(const dumpi_unpack_external *prm, uint16_t thread,
                       const dumpi_time *cpu, const dumpi_time *wall,
                       const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_create_errhandler(const dumpi_win_create_errhandler *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_get_errhandler(const dumpi_win_get_errhandler *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Win_set_errhandler(const dumpi_win_set_errhandler *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_open(const dumpi_file_open *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_open");
}

int parsedumpi_callbacks::
on_MPI_File_close(const dumpi_file_close *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_close");
}

int parsedumpi_callbacks::
on_MPI_File_delete(const dumpi_file_delete *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_delete");
}

int parsedumpi_callbacks::
on_MPI_File_set_size(const dumpi_file_set_size *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_preallocate(const dumpi_file_preallocate *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_preallocate");
}

int parsedumpi_callbacks::
on_MPI_File_get_size(const dumpi_file_get_size *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_group(const dumpi_file_get_group *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_amode(const dumpi_file_get_amode *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_set_info(const dumpi_file_set_info *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_info(const dumpi_file_get_info *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_set_view(const dumpi_file_set_view *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_view(const dumpi_file_get_view *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_read_at(const dumpi_file_read_at *prm, uint16_t thread,
                    const dumpi_time *cpu, const dumpi_time *wall,
                    const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_at");
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all(const dumpi_file_read_at_all *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_at_all");
}

int parsedumpi_callbacks::
on_MPI_File_write_at(const dumpi_file_write_at *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_at");
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all(const dumpi_file_write_at_all *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_at_all");
}

int parsedumpi_callbacks::
on_MPI_File_iread_at(const dumpi_file_iread_at *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iread_at");
}

int parsedumpi_callbacks::
on_MPI_File_iwrite_at(const dumpi_file_iwrite_at *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iwrite_at");
}

int parsedumpi_callbacks::
on_MPI_File_read(const dumpi_file_read *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read");
}

int parsedumpi_callbacks::
on_MPI_File_read_all(const dumpi_file_read_all *prm, uint16_t thread,
                     const dumpi_time *cpu, const dumpi_time *wall,
                     const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_all");
}

int parsedumpi_callbacks::
on_MPI_File_write(const dumpi_file_write *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write");
}

int parsedumpi_callbacks::
on_MPI_File_write_all(const dumpi_file_write_all *prm, uint16_t thread,
                      const dumpi_time *cpu, const dumpi_time *wall,
                      const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_all");
}

int parsedumpi_callbacks::
on_MPI_File_iread(const dumpi_file_iread *prm, uint16_t thread,
                  const dumpi_time *cpu, const dumpi_time *wall,
                  const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iread");
}

int parsedumpi_callbacks::
on_MPI_File_iwrite(const dumpi_file_iwrite *prm, uint16_t thread,
                   const dumpi_time *cpu, const dumpi_time *wall,
                   const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iwrite");
}

int parsedumpi_callbacks::
on_MPI_File_seek(const dumpi_file_seek *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_seek");
}

int parsedumpi_callbacks::
on_MPI_File_get_position(const dumpi_file_get_position *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_byte_offset(const dumpi_file_get_byte_offset *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_read_shared(const dumpi_file_read_shared *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_shared");
}

int parsedumpi_callbacks::
on_MPI_File_write_shared(const dumpi_file_write_shared *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_shared");
}

int parsedumpi_callbacks::
on_MPI_File_iread_shared(const dumpi_file_iread_shared *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iread_shared");
}

int parsedumpi_callbacks::
on_MPI_File_iwrite_shared(const dumpi_file_iwrite_shared *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_iwrite_shared");
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered(const dumpi_file_read_ordered *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_ordered");
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered(const dumpi_file_write_ordered *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_ordered");
}

int parsedumpi_callbacks::
on_MPI_File_seek_shared(const dumpi_file_seek_shared *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_seek_shared");
}

int parsedumpi_callbacks::
on_MPI_File_get_position_shared
(const dumpi_file_get_position_shared *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all_begin
(const dumpi_file_read_at_all_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_at_all_begin");
}

int parsedumpi_callbacks::
on_MPI_File_read_at_all_end(const dumpi_file_read_at_all_end *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_at_all_end");
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all_begin
(const dumpi_file_write_at_all_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_at_all_begin");
}

int parsedumpi_callbacks::
on_MPI_File_write_at_all_end(const dumpi_file_write_at_all_end *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf,void *uarg)
{
  return not_implemented("on_MPI_File_write_at_all_end");
}

int parsedumpi_callbacks::
on_MPI_File_read_all_begin(const dumpi_file_read_all_begin *prm,
                           uint16_t thread,
                           const dumpi_time *cpu, const dumpi_time *wall,
                           const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_all_begin");
}

int parsedumpi_callbacks::
on_MPI_File_read_all_end(const dumpi_file_read_all_end *prm, uint16_t thread,
                         const dumpi_time *cpu, const dumpi_time *wall,
                         const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_all_end");
}

int parsedumpi_callbacks::
on_MPI_File_write_all_begin(const dumpi_file_write_all_begin *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_all_begin");
}

int parsedumpi_callbacks::
on_MPI_File_write_all_end(const dumpi_file_write_all_end *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_all_end");
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered_begin
(const dumpi_file_read_ordered_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_read_ordered_begin");
}

int parsedumpi_callbacks::
on_MPI_File_read_ordered_end(const dumpi_file_read_ordered_end *prm,
                             uint16_t thread,
                             const dumpi_time *cpu, const dumpi_time *wall,
                             const dumpi_perfinfo *perf, void*uarg)
{
  return not_implemented("on_MPI_File_read_ordered_end");
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered_begin
(const dumpi_file_write_ordered_begin *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_ordered_begin");
}

int parsedumpi_callbacks::
on_MPI_File_write_ordered_end
(const dumpi_file_write_ordered_end *prm, uint16_t thread,
 const dumpi_time *cpu, const dumpi_time *wall,
 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_write_ordered_end");
}

int parsedumpi_callbacks::
on_MPI_File_get_type_extent(const dumpi_file_get_type_extent *prm,
                            uint16_t thread,
                            const dumpi_time *cpu, const dumpi_time *wall,
                            const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_Register_datarep(const dumpi_register_datarep *prm, uint16_t thread,
                        const dumpi_time *cpu, const dumpi_time *wall,
                        const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_set_atomicity(const dumpi_file_set_atomicity *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_get_atomicity(const dumpi_file_get_atomicity *prm, uint16_t thread,
                          const dumpi_time *cpu, const dumpi_time *wall,
                          const dumpi_perfinfo *perf, void *uarg)
{
  return pass(uarg, cpu, wall, perf, "on_MPI_Initialized");
}

int parsedumpi_callbacks::
on_MPI_File_sync(const dumpi_file_sync *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPI_File_sync");
}

int parsedumpi_callbacks::
on_MPIO_Test(const dumpio_test *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Test");
}

int parsedumpi_callbacks::
on_MPIO_Wait(const dumpio_wait *prm, uint16_t thread,
             const dumpi_time *cpu, const dumpi_time *wall,
             const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Wait");
}

int parsedumpi_callbacks::
on_MPIO_Testall(const dumpio_testall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Testall");
}

int parsedumpi_callbacks::
on_MPIO_Waitall(const dumpio_waitall *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Waitall");
}

int parsedumpi_callbacks::
on_MPIO_Testany(const dumpio_testany *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Testany");
}

int parsedumpi_callbacks::
on_MPIO_Waitany(const dumpio_waitany *prm, uint16_t thread,
                const dumpi_time *cpu, const dumpi_time *wall,
                const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Waitany");
}

int parsedumpi_callbacks::
on_MPIO_Waitsome(const dumpio_waitsome *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Waitsome");
}

int parsedumpi_callbacks::
on_MPIO_Testsome(const dumpio_testsome *prm, uint16_t thread,
                 const dumpi_time *cpu, const dumpi_time *wall,
                 const dumpi_perfinfo *perf, void *uarg)
{
  return not_implemented("on_MPIO_Testsome");
}


}