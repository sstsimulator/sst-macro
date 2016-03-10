/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/libraries/mpi/sstmac_mpi.h>

#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_comm/keyval.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_op.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_id.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_group.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_info.h>
#include <sstmac/libraries/mpi/mpi_payload.h>
#include <sstmac/libraries/mpi/rma/mpi_window.h>
#include <sstmac/libraries/mpi/mpi_api_persistent.h>

#include <sstmac/software/process/thread.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/common/thread_info.h>
#include <iostream>

#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/malloc.h>

#include <sprockit/unordered.h>


#define MAX_TYPE_COUNT 10

namespace sstmac {
namespace sw {

class EnsureMPIFinalize
{
 private:
  bool initialized_;
  bool finalized_;

 public:
  EnsureMPIFinalize() :
    initialized_(false), finalized_(false)
  {
  }

  void
  set_initialized()
  {
    initialized_ = true;
  }

  void
  set_finalized()
  {
    finalized_ = true;
  }

  ~EnsureMPIFinalize()
  {
    if (initialized_ && !finalized_) {
      cerrn << "Program terminated without calling MPI_Finalize"
                << std::endl;
    }
  }
};
static EnsureMPIFinalize ensure_mpi_finalize;

bool has_been_initialized = false;

static bool payloads_ = true;

}
}



using namespace sstmac::sw;
using namespace sstmac;

extern "C" void MPI_Disable_Payloads()
{
  payloads_ = false;
}
extern "C" void MPI_Enable_Payloads()
{
  payloads_ = true;
}

/*
 * ---------------------------------------------------------------------------
 * ------------------ Validity checkers -----------------------
 * ---------------------------------------------------------------------------
 */


void
fill_status(MPI_Status* status, mpi_status* stat, mpi_api* mpi)
{
  if (!status)
    return;

  status->MPI_SOURCE = stat->source();
  status->MPI_TAG = stat->tag();
  status->MPI_ERROR = 0;
  mpi_type* recvtype = mpi->type_from_id(stat->recv_type());
  int pack_size = recvtype->packed_size();
  if (pack_size == 0){
    status->COUNT = 0;
    status->BYTES_RECV = 0;
  }
  else {
    int count = stat->bytes_received() / pack_size;
    int remainder = stat->bytes_received() % recvtype->packed_size();
    if (remainder){
      status->COUNT = MPI_UNDEFINED;
    }
    else {
      status->COUNT = count;
    }
    status->BYTES_RECV = stat->bytes_received();
  }
  status->CANCELLED = false;
}



/*
 * ---------------------------------------------------------------------------
 * --------------- Utility functions to convert to sstmac --------------------
 * ---------------------------------------------------------------------------
 */

static mpi_id
get_mpi_source(int src)
{
  switch (src) {
    case MPI_PROC_NULL:
      return mpi::proc_null;
    case MPI_ANY_SOURCE:
      return mpi::any_source;
    default:
      return mpi_id(src);
  }
}

static mpi_tag
get_mpi_tag(int tag)
{
  switch (tag) {
    case MPI_ANY_TAG:
      return mpi::any_tag;
    default:
      return mpi_tag(tag);
  }
}

mpi_api*
current_mpi()
{
  thread* t = operating_system::current_thread();
  return t->get_api<mpi_api> ();
}


mpi_type*
get_mpi_type(MPI_Datatype t)
{
  return current_mpi()->type_from_id(mpi_type_id(t));
}

MPI_Datatype
get_mpi_datatype(mpi_type* t)
{
  if (t == mpi_type::mpi_char) {
    return MPI_CHAR;
  }
  else if (t == mpi_type::mpi_unsigned_char) {
    return MPI_UNSIGNED_CHAR;
  }
  else if (t == mpi_type::mpi_signed_char) {
    return MPI_SIGNED_CHAR;
  }
  else if (t == mpi_type::mpi_byte) {
    return MPI_BYTE;
  }
  else if (t == mpi_type::mpi_int) {
    return MPI_INT;
  }
  else if (t == mpi_type::mpi_unsigned) {
    return MPI_UNSIGNED;
  }
  else if (t == mpi_type::mpi_short) {
    return MPI_SHORT;
  }
  else if (t == mpi_type::mpi_unsigned_short) {
    return MPI_UNSIGNED_SHORT;
  }
  else if (t == mpi_type::mpi_double) {
    return MPI_DOUBLE;
  }
  else if (t == mpi_type::mpi_long) {
    return MPI_LONG;
  }
  else if (t == mpi_type::mpi_unsigned_long) {
    return MPI_UNSIGNED_LONG;
  }
  else if (t == mpi_type::mpi_float) {
    return MPI_FLOAT;
  }

  else if (t == mpi_type::mpi_double_int) {
    return MPI_DOUBLE_INT;
  }

  else if (t == mpi_type::mpi_2int) {
    return MPI_2INT;
  }

  else if (t == mpi_type::mpi_float_int) {
    return MPI_FLOAT_INT;
  }

  else if (t == mpi_type::mpi_long_int) {
    return MPI_LONG_INT;
  }

  else if (t == mpi_type::mpi_short_int) {
    return MPI_SHORT_INT;
  }

  else if (t == mpi_type::mpi_long_double_int) {
    return MPI_LONG_DOUBLE_INT;
  }
  else if (t == mpi_type::mpi_long_double) {
    return MPI_LONG_DOUBLE;
  }

  else if (t == mpi_type::mpi_char) {
    return MPI_INT8_T;
  }

  else if (t == mpi_type::mpi_short) {
    return MPI_INT16_T;
  }

  else if (t == mpi_type::mpi_int) {
    return MPI_INT32_T;
  }

  else if (t == mpi_type::mpi_long) {
    return MPI_INT64_T;
  }

  else if (t == mpi_type::mpi_unsigned_char) {
    return MPI_UINT8_T;
  }

  else if (t == mpi_type::mpi_unsigned_short) {
    return MPI_UINT16_T;
  }

  else if (t == mpi_type::mpi_unsigned) {
    return MPI_UINT32_T;
  }

  else if (t == mpi_type::mpi_unsigned_long) {
    return MPI_UINT64_T;
  }
  else {
    spkt_throw(sprockit::value_error,
        "sstmac_mpi::get_mpi_datatype: unknown type");
  }
}

mpi_op*
get_mpi_op(int o)
{
  switch (o) {
    case MPI_MAX:
      return mpi_op::max;
    case MPI_MIN:
      return mpi_op::min;
    case MPI_SUM:
      return mpi_op::sum;
    case MPI_PROD:
      return mpi_op::prod;
    case MPI_LAND:
      return mpi_op::land;
    case MPI_BAND:
      return mpi_op::band;
    case MPI_LOR:
      return mpi_op::lor;
    case MPI_BOR:
      return mpi_op::bor;
    case MPI_LXOR:
      return mpi_op::lxor;
    case MPI_BXOR:
      return mpi_op::bxor;
    case MPI_MAXLOC:
      return mpi_op::maxloc;
    case MPI_MINLOC:
      return mpi_op::minloc;
    case MPI_REPLACE:
      return mpi_op::replace;
    default:
      mpi_api* app = current_mpi();
      try {
        return app->created_op(o);
      }
      catch (...) {
        spkt_throw_printf(sprockit::spkt_error,
                         "sstmac_mpi::get_mpi_op() - unknown operation %d", o);
      }
  }
}


// ----- These functions were added to support SST/macro payloads,
// ----- should be called before and after mpi calls that use real payloads

static inline bool
is_fake_data(const void* buf)
{
  return buf == NULL || buf == SPROCKIT_FAKE_PTR;
}

static inline bool
is_real_data(const void* buf)
{
  return !is_fake_data(buf);
}



payload::const_ptr
get_mpi_payload_func(const void* buf, int offset, int count, MPI_Datatype type,
                     bool usewrapper)
{
  if (is_fake_data(buf) || !payloads_ || count == 0) {
    return payload::null();
  }
  else {
    mpi_type* ty = get_mpi_type(type);
    char* casted = (char*) buf;
    casted += (offset * ty->extent());

    return new mpi_payload(casted, ty, count, usewrapper);
  }
}

payload::const_ptr
get_mpi_payload(const void* buf, int offset, int count, MPI_Datatype type)
{
  return get_mpi_payload_func(buf, offset, count, type, false);
}

bool
get_mpi_vector_payload_func(const void* buf, int nrank, int count,
                            MPI_Datatype data_type, std::vector<payload::const_ptr>& ret,
                            bool usewrapper)
{
  if (is_fake_data(buf) || !payloads_) {
    //we need to make sure the ret is at least the correct size
    ret.resize(nrank);
    return false;
  }
  else {
    mpi_type* ty = get_mpi_type(data_type);
    ret.resize(nrank);
    char* casted = (char*) buf;
    for (int i = 0; i < nrank; i++) {
      ret[i] = new mpi_payload(casted + i * count * ty->extent(), ty, count, usewrapper);
    }
    return true;
  }
}

bool
get_mpi_vector_payload(const void* buf, int nrank, int count,
                       MPI_Datatype data_type, std::vector<payload::const_ptr>& ret)
{
  return get_mpi_vector_payload_func(buf, nrank, count, data_type, ret, false);
}

void
set_mpi_payload_fxn(void* buf, int bytes, mpi_type* type, payload::const_ptr load)
{
  if (is_fake_data(buf) || load == payload::null() || !payloads_) {
    return;
  }
  else {
    mpi_payload::const_ptr mpiload = ptr_safe_cast(const mpi_payload, load,
                                        "sstmac_mpi::set_mpi_payload: didn't cast mpi_payload");
    mpiload->extract(buf, bytes, type);
  }
}

void
set_mpi_payload_pt2pt(void* buf, int bytes, MPI_Datatype type, payload::const_ptr load)
{
  set_mpi_payload_fxn(buf, bytes, get_mpi_type(type), load);
}

void
set_mpi_payload_rma(void* buf, int count, MPI_Datatype type, payload::const_ptr load)
{
  mpi_type* ty = get_mpi_type(type);
  int bytes = count * ty->packed_size();
  set_mpi_payload_fxn(buf, bytes, ty, load);
}

void
set_mpi_payload_collective(void* buf, int offset, int count, MPI_Datatype type,
                payload::const_ptr load)
{
  mpi_type* ty = get_mpi_type(type);
  int byte_offset = offset * ty->extent();
  void* offset_buffer = ((char*)buf) + byte_offset;
  int bytes = count * ty->packed_size();
  set_mpi_payload_fxn(offset_buffer, bytes, ty, load);
}

void
set_mpi_vector_payload(mpi_api* mpi, void* buf, int count, MPI_Datatype data_type,
                       const std::vector<payload::const_ptr>& load)
{
  if (is_fake_data(buf) || !payloads_) {
    return;
  }
  else {
    mpi_type* recvtype = get_mpi_type(data_type);
    char* casted = (char*) buf;
    for (int i = 0; i < load.size(); i++) {
      mpi_payload::ptr mpiload = safe_cast(mpi_payload, const_cast<payload*>(load[i].get()));
      mpiload->recover(mpi);
      int bytes = mpiload->count() * mpiload->type()->packed_size();
      //int recvcount = bytes / recvtype->packed_size(); //in case we receive more than we send
      //if (bytes % recvtype->packed_size()){
      //  ++recvcount;
      //}

      mpiload->extract(casted, bytes, recvtype);

      casted += bytes;
    }
  }
}

void
get_displaced_payload(void * buf, int nranks, int *sendcnts, int *displs,
                      MPI_Datatype datatype, std::vector<payload::const_ptr> &load)
{
  for (int i = 0; i < nranks; ++i) {
    load.push_back(get_mpi_payload(buf, displs[i], sendcnts[i], datatype));
  }
}

void
set_displaced_payload_collective(void * buf, int nranks, int *recvcnts, int *displs,
                      MPI_Datatype datatype, std::vector<payload::const_ptr> load)
{
  for (int i = 0; i < nranks; ++i) {
    set_mpi_payload_collective(buf, displs[i], recvcnts[i], datatype, load[i]);
  }
}

void
recover_payload(mpi_api* mpi, void* buf, mpi_status* stat)
{
  set_mpi_payload_pt2pt(buf, stat->bytes_received(), stat->recv_type(), stat->content());  
}

extern "C" void MPI_Print(const char* msg)
{
  std::cout << msg << "\n";
}

/*
 * ---------------------------------------------------------------------------
 * ------------------ The real MPI functions ---------------------------------
 * ---------------------------------------------------------------------------
 */

int* const MPI_UNWEIGHTED = (int* const ) 0x1;

/// this guy is useful for libraries
extern "C" long MPI_NodeAddress(int rank, MPI_Comm c)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* com = mpi->get_comm(c);
  mpi_id r(rank);
  return com->node_at(r);
}

extern "C" long MPI_Taskid(int rank, MPI_Comm c)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* com = mpi->get_comm(c);
  mpi_id r(rank);
  return com->peer_task(r);
}

extern "C" void MPI_Abort(MPI_Comm c, int code)
{
  spkt_throw_printf(sprockit::spkt_error,
                   "sstmac_mpi::MPI_Abort with code %d -- this function is not actually implemented properly",
                   code);
}

extern "C" int MPI_Init(int *argc, char ***argv)
{
  mpi_api* mpi = current_mpi();
  mpi->init();

  mpi_comm* world = mpi->comm_world();
  world->set_name("MPI_COMM_WORLD");

  mpi_comm* self = mpi->comm_self();
  self->set_name("MPI_COMM_SELF");

  //empty group
  std::vector<task_id> vec;
  mpi_group* newgrp = new mpi_group(vec);

  MPI_Comm worldcomm = mpi->add_comm_ptr(world);
  MPI_Group worldgrp = mpi->add_group_ptr(world->group());
  mpi->add_comm_grp(worldcomm, worldgrp);

  MPI_Comm selfcomm = mpi->add_comm_ptr(self);
  MPI_Group selfgrp = mpi->add_group_ptr(self->group());
  mpi->add_comm_grp(selfcomm, selfgrp);

  mpi->add_group_ptr(MPI_GROUP_EMPTY, newgrp);

  mpi->add_err_handler(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL);
  mpi->add_err_handler(MPI_COMM_SELF, MPI_ERRORS_ARE_FATAL);

  ensure_mpi_finalize.set_initialized();

  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Initialized(int *flag)
{
  mpi_api* app = current_mpi();
  *flag = (int) app->initialized();
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Finalize()
{
  mpi_api* mpi = current_mpi();
  mpi->finalize();

  mpi_group* empty = mpi->get_group(MPI_GROUP_EMPTY);
  delete empty;

  //these all need to get cleared
  //otherwise cxa_finalize can freak out
  //with memory errors
  ensure_mpi_finalize.set_finalized();
  return MPI_SUCCESS; //yes, error handling would be nice
}

static void
complete_wait(
  mpi_api* mpi,
  mpi_request* sst_req_ptr,
  MPI_Request* mpi_req_ptr,
  mpi_status* sst_stat_ptr,
  MPI_Status* mpi_stat_ptr)
{
  MPI_Request mpi_req = *mpi_req_ptr;
  if (mpi_stat_ptr != MPI_STATUS_IGNORE)
    fill_status(mpi_stat_ptr, sst_stat_ptr, mpi);

  if (sst_req_ptr){
    if (sst_req_ptr->is_cancelled()){
      mpi_stat_ptr->CANCELLED = true;
    }
    else if (sst_req_ptr->is_persistent()){
      mpi_api::persistent* pers = safe_cast(mpi_api::persistent, sst_req_ptr);
      pers->started_ = false;
    }
  }
  else if (mpi_req != MPI_REQUEST_NULL){
    void* buffer = mpi->find_buffer(mpi_req);
    if (is_real_data(buffer)){
      recover_payload(mpi, buffer, sst_stat_ptr);
    }
    //these can be null
    mpi->erase_request_ptr(mpi_req);
    *mpi_req_ptr = MPI_REQUEST_NULL;    
  }
}

extern "C" int
MPI_Wait(MPI_Request *request, MPI_Status *status)
{
  if (request == 0) {
    spkt_throw_printf(sprockit::spkt_error,
                     "sstmac_mpi::MPI_Wait: how dare you pass a null pointer to a request \n");
  }
  else if (*request >= 0) {
    mpi_api* mpi = current_mpi();
    mpi_request* req = mpi->get_request(*request);
    if (req && !req->is_cancelled()){
      mpi_status sst_stat;
      mpi->wait(&req, &sst_stat);
      complete_wait(mpi, req, request, &sst_stat, status);
    }
  }
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Waitall(int count, MPI_Request array_of_requests[],
            MPI_Status array_of_statuses[])
{
  mpi_api* mpi = current_mpi();
  std::vector<mpi_request*> sst_reqs;
  sst_reqs.resize(count);
  for (int i = 0; i < count; i++) {
    if (array_of_requests[i] != MPI_REQUEST_NULL) {
      sst_reqs[i] = mpi->get_request(array_of_requests[i]);
    }
  }

  std::vector<mpi_status> sst_stats;
  mpi->waitall(sst_reqs, sst_stats);
  for (int i=0; i < count; ++i){
    MPI_Status* mpi_stat_ptr = array_of_statuses == MPI_STATUSES_IGNORE ? 
          MPI_STATUS_IGNORE : &array_of_statuses[i];
    complete_wait(mpi, sst_reqs[i], &array_of_requests[i],
        &sst_stats[i], mpi_stat_ptr);
  }

  return MPI_SUCCESS; //yes, error handling would be nice
}

int
do_waitany(int count, MPI_Request array_of_requests[],
           int *index, MPI_Status *status, double timeout)
{
  mpi_api* mpi = current_mpi();
  std::vector<mpi_request*> sst_reqs;
  sst_reqs.resize(count);
  for (int i = 0; i < count; i++) {
    sst_reqs[i] = mpi->get_request(array_of_requests[i]);
  }

  int ind;
  mpi_status sst_stat;
  mpi->waitany(sst_reqs, ind, &sst_stat, timestamp(timeout));
  if (ind >= 0){
    complete_wait(mpi, sst_reqs[ind], &array_of_requests[ind],
      &sst_stat, status);
  }

  if (ind >= 0){
    *index = ind;
  } else {
    *index = MPI_UNDEFINED;
  }

  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Waitany_timeout(int count, MPI_Request array_of_requests[],
                    int *index, MPI_Status *status, double timeout)
{
  return do_waitany(count, array_of_requests, index, status, timeout);

}

extern "C" int
MPI_Waitany(int count, MPI_Request array_of_requests[],
            int *index, MPI_Status *status)
{
  //timeout of zero means standard waitany
  return do_waitany(count, array_of_requests, index, status, 0);
}

extern "C" int
MPI_Waitsome(int incount, MPI_Request array_of_requests[],
            int *outcount, int array_of_indices[],
            MPI_Status array_of_statuses[])
{
  mpi_api* mpi = current_mpi();
  std::vector<mpi_request*> sst_reqs;
  sst_reqs.resize(incount);
  for (int i = 0; i < incount; i++) {
    sst_reqs[i] = mpi->get_request(array_of_requests[i]);
  }

  std::vector<int> indices;
  std::vector<mpi_status> sst_stats;
  mpi->waitsome(sst_reqs, indices, sst_stats);
  for (int i=0; i < indices.size(); ++i){
    int idx = indices[i];
    array_of_indices[i] = idx;
    MPI_Status* mpi_stat_ptr = array_of_statuses == MPI_STATUSES_IGNORE ? 
          MPI_STATUS_IGNORE : &array_of_statuses[idx];
    complete_wait(mpi, sst_reqs[idx], &array_of_requests[idx],
      &sst_stats[i], mpi_stat_ptr);
  }
  *outcount = indices.size();


  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Get_count(MPI_Status *status, MPI_Datatype datatype, int *count)
{
  //this now gets filled in with the status
  *count = status->COUNT;
  return MPI_SUCCESS;
}

extern "C" int
MPI_Test(MPI_Request *request, int *flag, MPI_Status *status)
{
  if (*request != MPI_REQUEST_NULL) {
    mpi_api* mpi = current_mpi();
    bool f;
    mpi_request* req = mpi->get_request(*request);
    mpi_status sst_stat;
    mpi->test(&req, f, &sst_stat);
    if (f) {
      complete_wait(mpi, req, request, &sst_stat, status);
    }
    *flag = f;
  }
  else {
    *flag = true;
    status->COUNT = 0;
    status->MPI_SOURCE = MPI_ANY_SOURCE;
    status->MPI_TAG = MPI_ANY_TAG;
  }
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Probe(int source, int tag, MPI_Comm comm, MPI_Status *status)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_status stat;
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->probe(get_mpi_source(source), get_mpi_tag(tag), commptr, &stat);
  fill_status(status, &stat, mpi);
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Cancel(MPI_Request *request)
{
  if (request >= 0) {
    mpi_api* mpi = current_mpi();
    int rank = mpi->comm_world()->rank();
    mpi_request* req = mpi->get_request(*request);
    mpi->cancel(req);
    //erase_request*(rank, *request);
  }
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Request_free(MPI_Request *request)
{
  if (*request != MPI_REQUEST_NULL) {
    mpi_api* mpi = current_mpi();
    mpi->erase_request_ptr(*request);
    *request = MPI_REQUEST_NULL;
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_dup(MPI_Comm comm, MPI_Comm *newcomm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_comm* ret;
  mpi->comm_dup(commptr, ret);
  *newcomm = ret->id();
  MPI_Group g = mpi->add_group_ptr(ret->group());
  mpi->add_comm_grp(*newcomm, g);
  mpi->copy_err_handler(*newcomm, comm);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_rank(MPI_Comm comm, int *rankptr)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  *rankptr = commptr->rank();
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_split(MPI_Comm comm, int color, int key,
               MPI_Comm *newcomm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  //SSTMAC_DEBUG << "[rank=" << rank << "] " << "(" << comm << ", "
  //             << color << ", " << key << ") \n";

  mpi_comm* ret;
  mpi->comm_split(commptr, color, key, ret);
  if (ret != mpi_comm::comm_null) {
    *newcomm = ret->id();
    MPI_Group g = mpi->add_group_ptr(ret->group());
    mpi->add_comm_grp(*newcomm, g);
  }
  else {
    *newcomm = MPI_COMM_NULL;
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_size(MPI_Comm comm, int *size)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  *size = commptr->size();
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_free(MPI_Comm *comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(*comm);
  mpi->comm_free(commptr);
  mpi->erase_err_handler(*comm);
  *comm = MPI_COMM_NULL;
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_create(MPI_Comm comm, MPI_Group group,
                MPI_Comm *newcomm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_group* grp_ptr = mpi->get_group(group);
  mpi_comm* ret;
  mpi->comm_create(commptr, grp_ptr, ret);
  if(ret == mpi_comm::comm_null) {
    *newcomm = MPI_COMM_NULL;
  }
  else {
    *newcomm = mpi->add_comm_ptr(ret);
    mpi->copy_err_handler(*newcomm, comm);
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Comm_group(MPI_Comm comm, MPI_Group *group)
{
  mpi_api* mpi = current_mpi();
  *group = mpi->get_comm_grp(comm);
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_group* grp_ptr = commptr->group();
  return MPI_SUCCESS;
}

extern "C" int
MPI_Group_incl(MPI_Group group, int n, int *ranks,
               MPI_Group *newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* oldgrp = mpi->get_group(group);

  if (n == 0) {
    *newgroup = MPI_GROUP_EMPTY;
  }
  else {
    mpi_group* newgrp;
    mpi->group_incl(ranks, n, oldgrp, newgrp);
    *newgroup = mpi->add_group_ptr(newgrp);
  }

  return MPI_SUCCESS;
}

extern "C" int
MPI_Group_free(MPI_Group *group)
{
  mpi_api* mpi = current_mpi();
  mpi->erase_group_ptr(*group);
  *group = MPI_GROUP_NULL;
  return MPI_SUCCESS;
}

typedef timestamp
(mpi_api::*mpiapi_send_fxn)(int count, mpi_type_id type,
                            mpi_id target,
                            mpi_tag tag, mpi_comm* comm,
                            const payload::const_ptr& content);

static int
mpi_blocking_send(void *buf, int count, MPI_Datatype datatype, int dest,
                  int tag, MPI_Comm comm, mpiapi_send_fxn fxn)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  if (dest == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  sstmac::payload::const_ptr load = get_mpi_payload(buf, 0, count, datatype);

  (mpi->*fxn)(count, mpi_type_id(datatype), mpi_id(dest), mpi_tag(tag),
              commptr, load);

  return MPI_SUCCESS;
}

extern "C" int
MPI_Ssend(void *buf, int count, MPI_Datatype datatype, int dest,
          int tag, MPI_Comm comm)
{
  return mpi_blocking_send(buf, count, datatype, dest, tag, comm,
                           &mpi_api::ssend);
}

extern "C" int
MPI_Rsend(void *buf, int count, MPI_Datatype datatype, int dest,
          int tag, MPI_Comm comm)
{
  return mpi_blocking_send(buf, count, datatype, dest, tag, comm,
                           &mpi_api::rsend);
}

extern "C" int
MPI_Bsend(void *buf, int count, MPI_Datatype datatype, int dest,
          int tag, MPI_Comm comm)
{
  return mpi_blocking_send(buf, count, datatype, dest, tag, comm,
                           &mpi_api::bsend);
}

extern "C" int
MPI_Send(void *buf, int count, MPI_Datatype datatype, int dest,
         int tag, MPI_Comm comm)
{
  return mpi_blocking_send(buf, count, datatype, dest, tag, comm, &mpi_api::send);
}


extern "C" int
MPI_Sendrecv(void *sendbuf, int sendcount,
             MPI_Datatype sendtype, int dest, int sendtag, void *recvbuf, int recvcount,
             MPI_Datatype recvtype, int source, int recvtag, MPI_Comm comm,
             MPI_Status* status)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_status stat;

  if (dest == MPI_PROC_NULL || source == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  payload::const_ptr load = get_mpi_payload(sendbuf, 0, sendcount, sendtype);

  mpi->sendrecv(sendcount, mpi_type_id(sendtype), mpi_id(dest),
                mpi_tag(sendtag), recvcount,
                mpi_type_id(recvtype), get_mpi_source(source),
                mpi_tag(recvtag), commptr, &stat, load);

  set_mpi_payload_pt2pt(recvbuf, stat.bytes_received(), recvtype, stat.content());

  if (status != MPI_STATUS_IGNORE)
    fill_status(status, &stat, mpi);

  return MPI_SUCCESS;
}

typedef timestamp
(mpi_api::*mpiapi_isend_fxn)(int count, mpi_type_id type,
                             mpi_id target, mpi_tag tag, mpi_comm* comm,
                             mpi_request* &req, const payload::const_ptr& content);

static MPI_Request
mpi_immediate_send(const void *buf, int count, MPI_Datatype datatype, int dest,
                   int tag, MPI_Comm comm, MPI_Request *request, mpiapi_isend_fxn fxn)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_request* req;

  if (dest == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  sstmac::payload::const_ptr load = get_mpi_payload(buf, 0, count, datatype);

  (mpi->*fxn)(count, mpi_type_id(datatype), mpi_id(dest), mpi_tag(tag),
                    commptr, req, load);
  *request = mpi->add_request_ptr(req);

  return MPI_SUCCESS;
}

extern "C" int
MPI_Isend(void *buf, int count, MPI_Datatype datatype, int dest,
          int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_immediate_send(buf, count, datatype, dest, tag, comm, request,
                            &mpi_api::isend);
}

extern "C" int
MPI_Ibsend(void *buf, int count, MPI_Datatype datatype,
           int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_immediate_send(buf, count, datatype, dest, tag, comm, request,
                            &mpi_api::ibsend);
}

extern "C" int
MPI_Issend(void *buf, int count, MPI_Datatype datatype,
           int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_immediate_send(buf, count, datatype, dest, tag, comm, request,
                            &mpi_api::issend);
}

extern "C" int
MPI_Irsend(void *buf, int count, MPI_Datatype datatype,
           int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_immediate_send(buf, count, datatype, dest, tag, comm, request,
                            &mpi_api::irsend);
}

extern "C" int
MPI_Recv(void *buf, int count, MPI_Datatype datatype,
         int source, int tag, MPI_Comm comm, MPI_Status *status)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  if (source == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  mpi_status stat;
  mpi->recv(count, mpi_type_id(datatype), get_mpi_source(source), mpi_tag(tag),
            commptr, &stat);

  if (status && status != MPI_STATUS_IGNORE) {
    fill_status(status, &stat, mpi);
  }

  set_mpi_payload_pt2pt(buf, stat.bytes_received(), datatype, stat.content());


  return MPI_SUCCESS;
}

extern "C" int
MPI_Irecv(void *buf, int count, MPI_Datatype datatype,
          int source, int tag, MPI_Comm comm, MPI_Request *request)
{

  if (source == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);

  mpi_request* req;
  mpi->irecv(count, mpi_type_id(datatype), get_mpi_source(source), mpi_tag(tag), commptr, req);
  *request = mpi->add_request_ptr(req);
  mpi->add_buffer(*request, buf);

  return MPI_SUCCESS;
}

extern "C" int
MPI_Recover_Payload(void *buf, MPI_Request request)
{
  mpi_api* mpi = current_mpi();
  mpi_request* req = mpi->get_request(request);

  if (!req->is_complete()) {
    return MPI_ERR_PENDING;
  }
  else {
    mpi_status* stat = &req->status();
    recover_payload(mpi, buf, stat);
    return MPI_SUCCESS;
  }
}

extern "C" int
MPI_Allreduce(void *sendbuf, void *recvbuf, int count,
              MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  sstmac::payload::const_ptr load;
  if (sendbuf == MPI_IN_PLACE) {
    load = get_mpi_payload(recvbuf, 0, count, datatype);
  }
  else {
    load = get_mpi_payload(sendbuf, 0, count, datatype);
  }

  sstmac::payload::const_ptr res;
  mpi->allreduce(count, mpi_type_id(datatype), get_mpi_op(op), commptr, load, res);

  set_mpi_payload_collective(recvbuf, 0, count, datatype, res);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Reduce(void *sendbuf, void *recvbuf, int count,
           MPI_Datatype datatype, MPI_Op op, int root, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  sstmac::payload::const_ptr load;
  if (sendbuf == MPI_IN_PLACE) {
    load = get_mpi_payload(recvbuf, 0, count, datatype);
  }
  else {
    load = get_mpi_payload(sendbuf, 0, count, datatype);
  }
  sstmac::payload::const_ptr res;
  mpi->reduce(count, mpi_type_id(datatype), get_mpi_op(op), mpi_id(root),
              commptr, load, res);
  if (mpi->comm_rank(commptr).id_ == root) {
    set_mpi_payload_collective(recvbuf, 0, count, datatype, res);
  }
  return MPI_SUCCESS;
}

extern "C"
int MPI_Barrier(MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->barrier(commptr);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Bcast(void *buffer, int count, MPI_Datatype datatype,
          int root, MPI_Comm comm)
{
  int rank;
  MPI_Comm_rank(comm, &rank);

  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  if (root == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  sstmac::payload::const_ptr load;
  if (rank == root) {
    load = get_mpi_payload(buffer, 0, count, datatype);
  }

  mpi->bcast(count, mpi_type_id(datatype), mpi_id(root), commptr, load);

  if (rank != root) {
    set_mpi_payload_collective(buffer, 0, count, datatype, load);
  }

  return MPI_SUCCESS;
}

extern "C" int
MPI_Scan(void *sendbuf, void *recvbuf, int count,
         MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  sstmac::payload::const_ptr load =
    get_mpi_payload(sendbuf, 0, count, datatype);
  sstmac::payload::const_ptr res;
  mpi->scan(count, mpi_type_id(datatype), get_mpi_op(op), commptr, load, res);
  set_mpi_payload_collective(recvbuf, 0, count, datatype, res);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Gather(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
           void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  int commrank = commptr->rank_;
  sstmac::payload::const_ptr load;

  if (sendbuf == MPI_IN_PLACE) {
    sendcnt = recvcnt;
    sendtype = recvtype;
    // my in place buffer begins at my rank offset
    int offset = commrank * sendcnt;
    load = get_mpi_payload(recvbuf, offset, sendcnt, sendtype);
  }
  else {
    int offset = 0;
    load = get_mpi_payload(sendbuf, offset, sendcnt, sendtype);
  }

  mpi_type_id hack_rtype = root == commrank ? mpi_type_id(recvtype) : mpi_type::mpi_null->id;

  std::vector<sstmac::payload::const_ptr> cc;
  mpi->gather(sendcnt, mpi_type_id(sendtype), recvcnt, hack_rtype,
              sstmac::sw::mpi_id(root), commptr, load, cc);
  if (mpi->comm_rank(commptr).id_ == root) {
    set_mpi_vector_payload(mpi, recvbuf, recvcnt, recvtype, cc);
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Gatherv(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
            void *recvbuf, int *recvcnts, int *displs, MPI_Datatype recvtype, int root,
            MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);
  int commrank = commptr->rank_;
  mpi_id group_size = mpi->comm_size(commptr);
  std::vector<int> cxx_recvcnts(group_size, 0);
  if (rank == root) {
    for (int i = 0; i < int(group_size); ++i) {
      cxx_recvcnts[i] = recvcnts[i];
    }
  }

  payload::const_ptr sendload;
  std::vector<sstmac::payload::const_ptr> recvload;

  if (sendbuf == MPI_IN_PLACE) {
    sendload = get_mpi_payload(recvbuf, displs[commptr->rank()], sendcnt, sendtype);
  }
  else {
    sendload = get_mpi_payload(sendbuf, 0, sendcnt, sendtype);
  }

  mpi_type_id hack_rtype = root == commrank ? mpi_type_id(recvtype) : mpi_type::mpi_null->id;

  mpi->gatherv(sendcnt, mpi_type_id(sendtype), cxx_recvcnts,
               hack_rtype, mpi_id(root), commptr, sendload, recvload);

  if (commptr->rank().id_ == root){
    set_displaced_payload_collective(recvbuf, commptr->size(), recvcnts, displs,
                          recvtype, recvload);
  }
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Allgather(void *sendbuf, int sendcount,
              MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
              MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  int commrank = commptr->rank_;
  sstmac::payload::const_ptr load;

  if (sendbuf == MPI_IN_PLACE) {
    load = get_mpi_payload(recvbuf, commrank * recvcount, recvcount, recvtype);
    sendcount = recvcount;
    sendtype = recvtype;
  }
  else {
    load = get_mpi_payload(sendbuf, 0, sendcount, sendtype);
  }

  std::vector<sstmac::payload::const_ptr> cc;
  mpi->allgather(sendcount, mpi_type_id(sendtype),
                 recvcount, mpi_type_id(recvtype),
                 commptr, load, cc);

  set_mpi_vector_payload(mpi, recvbuf, recvcount, recvtype, cc);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Allgatherv(void *sendbuf, int sendcount,
               MPI_Datatype sendtype, void *recvbuf, int *recvcounts, int *displs,
               MPI_Datatype recvtype, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  int commrank = commptr->rank_;

  mpi_id group_size = mpi->comm_size(commptr);
  std::vector<int> cxx_recvcnts(group_size);
  for (int i = 0; i < int(group_size); ++i) {
    cxx_recvcnts[i] = recvcounts[i];
  }

  sstmac::payload::const_ptr sendload;
  if (sendbuf == MPI_IN_PLACE) {
    sendload = get_mpi_payload(recvbuf, displs[commrank], cxx_recvcnts[commrank],
                               recvtype);
    sendcount = cxx_recvcnts[commrank];
    sendtype = recvtype;
  }
  else {
    sendload = get_mpi_payload(sendbuf, 0, sendcount, sendtype);
  }
  std::vector<sstmac::payload::const_ptr> recvload;

  mpi->allgatherv(sendcount, mpi_type_id(sendtype), cxx_recvcnts,
                  mpi_type_id(recvtype), commptr, sendload, recvload);

  set_displaced_payload_collective(recvbuf, commptr->size(), recvcounts, displs,
                        recvtype, recvload);
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int
MPI_Scatter(void *sendbuf, int sendcnt, MPI_Datatype sendtype,
            void *recvbuf, int recvcnt, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  sstmac::payload::const_ptr load;
  std::vector<sstmac::payload::const_ptr> cc(commptr->size());
  bool nonnull_payload = true;

  int scnt = sendcnt;
  int rcnt = recvcnt;

  mpi_type_id hack_rtype(recvtype);
  mpi_type_id hack_stype(sendtype);
  if (int(commptr->rank()) == root) {
    if (recvbuf == MPI_IN_PLACE) {
      hack_rtype = mpi_type::mpi_null->id;
      rcnt = 0;
    }
    nonnull_payload = get_mpi_vector_payload(sendbuf, commptr->size(), scnt,
                      sendtype, cc);
  }
  else {
    hack_stype = mpi_type::mpi_null->id;
    scnt = 0;
  }

  //vector payloads were actually built

  mpi->scatter(scnt, hack_stype, rcnt, hack_rtype,
    sstmac::sw::mpi_id(root), commptr, cc, load);

  if (recvbuf != MPI_IN_PLACE) {
    set_mpi_payload_collective(recvbuf, 0, recvcnt, recvtype, load);
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Scatterv(void *sendbuf, int *sendcnts, int *displs,
             MPI_Datatype sendtype, void *recvbuf, int recvcnt, MPI_Datatype recvtype,
             int root, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  mpi_id group_size = mpi->comm_size(commptr);
  std::vector<int> cxx_sendcnts(group_size);
  for (int i = 0; i < int(group_size); ++i) {
    cxx_sendcnts[i] = sendcnts[i];
  }

  sstmac::payload::const_ptr load;
  std::vector<sstmac::payload::const_ptr> cc;
  if (int(commptr->rank()) == root) {
    get_displaced_payload(sendbuf, group_size, sendcnts, displs, sendtype,
                          cc);
  }

  if (sendbuf != NULL || recvbuf != NULL) {
    //vector payloads were actually built
    mpi->scatterv(cxx_sendcnts, mpi_type_id(sendtype), recvcnt,
                  mpi_type_id(recvtype), sstmac::sw::mpi_id(root), commptr, cc, load);
  }
  else {
    //ignore the vector payloads
    mpi->scatterv(cxx_sendcnts, mpi_type_id(sendtype), recvcnt,
                  mpi_type_id(recvtype), sstmac::sw::mpi_id(root), commptr);
  }
  set_mpi_payload_collective(recvbuf, 0, recvcnt, recvtype, load);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Alltoall(void *sendbuf, int sendcount,
             MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype,
             MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  std::vector<sstmac::payload::const_ptr> load;
  std::vector<sstmac::payload::const_ptr> cc;
  bool nonnull_payload = get_mpi_vector_payload(sendbuf, commptr->size(),
                         sendcount, sendtype, cc);
  mpi->alltoall(sendcount, mpi_type_id(sendtype), recvcount,
                mpi_type_id(recvtype), commptr, cc, load);
  set_mpi_vector_payload(mpi, recvbuf, recvcount, recvtype, load);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Alltoallv(void *sendbuf, int *sendcnts, int *sdispls,
              MPI_Datatype sendtype, void *recvbuf, int *recvcnts, int *rdispls,
              MPI_Datatype recvtype, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);

  mpi_id group_size = mpi->comm_size(commptr);
  std::vector<int> cxx_sendcnts(group_size);
  std::vector<int> cxx_recvcnts(group_size);
  for (int i = 0; i < int(group_size); ++i) {
    cxx_sendcnts[i] = sendcnts[i];
    cxx_recvcnts[i] = recvcnts[i];
  }

  std::vector<payload::const_ptr> sendload;
  get_displaced_payload(sendbuf, commptr->size(), sendcnts, sdispls,
                        sendtype, sendload);
  std::vector<sstmac::payload::const_ptr> recvload;

  mpi->alltoallv(cxx_sendcnts, mpi_type_id(sendtype), cxx_recvcnts,
                 mpi_type_id(recvtype), commptr, sendload, recvload);

  set_displaced_payload_collective(recvbuf, commptr->size(), recvcnts, rdispls,
                        recvtype, recvload);
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" double
MPI_Wtime()
{
  mpi_api* app = current_mpi();

  return app->wtime().sec(); //yes, error handling would be nice
}

extern "C" double
MPI_Wtick()
{
  return timestamp::tick_interval_sec();
}

//----------------------------------------------------------------
// --- MPI Cart functions
//----------------------------------------------------------------
extern "C" int
MPI_Cart_create(MPI_Comm comm, int ndims, int *dims,
                int *periods, int reorder, MPI_Comm *outcomm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_comm* newc;
  mpi->mpi_cart_create(commptr, ndims, dims, periods, reorder, newc);
  *outcomm = mpi->add_comm_ptr(newc);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Cart_get(MPI_Comm comm, int maxdims, int *dims,
             int *periods, int *coords)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->mpi_cart_get(commptr, maxdims, dims, periods, coords);
  return MPI_SUCCESS;
}

extern "C" int MPI_Cart_rank(MPI_Comm comm, int *coords, int *rankptr)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->mpi_cart_rank(commptr, coords, rankptr);
  return MPI_SUCCESS;
}

extern "C" int MPI_Cart_shift(MPI_Comm comm, int direction, int displ,
                              int *source, int *dest)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->mpi_cart_shift(commptr, direction, displ, source, dest);
  if (*source == mpi_comm::proc_null) {
    *source = MPI_PROC_NULL;
  }
  if (*dest == mpi_comm::proc_null) {
    *dest = MPI_PROC_NULL;
  }
  return MPI_SUCCESS;
}

extern "C" int MPI_Cart_coords(MPI_Comm comm, int rank, int maxdims,
                               int *coords)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->mpi_cart_coords(commptr, rank, maxdims, coords);
  return MPI_SUCCESS;
}

extern "C" int MPI_Address(void *location, MPI_Aint *address)
{
  *address = reinterpret_cast<MPI_Aint>(location);
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Iprobe(int source, int tag, MPI_Comm comm, int *flag,
                          MPI_Status *status)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_status stat;
  mpi_id src = get_mpi_source(source);
  mpi_tag t = get_mpi_tag(tag);


  bool fl;
  mpi->iprobe(src,t, commptr, fl, &stat);
  *flag = (int) fl;
  if (fl) {
    fill_status(status, &stat, mpi);
  }
  status->MPI_ERROR = 0;
  return MPI_SUCCESS; //yes, error handling would be nice
}

typedef timestamp
(mpi_api::*mpiapi_send_init_fxn)(int count, mpi_type_id type,
                                 mpi_id target, mpi_tag tag, mpi_comm* comm,
                                 mpi_request* &req, void* buf);

static int
mpi_init_send(void *buf, int count, MPI_Datatype datatype, int dest, int tag,
              MPI_Comm comm, MPI_Request *request, mpiapi_send_init_fxn fxn)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_request* req;
  //sstmac::payload::const_ptr load = get_mpi_payload(buf, 0, count, datatype);

  (mpi->*fxn)(count, mpi_type_id(datatype), mpi_id(dest), mpi_tag(tag),
                    commptr, req, buf);

  *request = mpi->add_request_ptr(req);

  return MPI_SUCCESS;
}

extern "C" int MPI_Send_init(void *buf, int count, MPI_Datatype datatype,
                             int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_init_send(buf, count, datatype, dest, tag, comm, request,
                       &mpi_api::send_init);
}

extern "C" int MPI_Bsend_init(void *buf, int count, MPI_Datatype datatype,
                              int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_init_send(buf, count, datatype, dest, tag, comm, request,
                       &mpi_api::bsend_init);
}

extern "C" int MPI_Rsend_init(void *buf, int count, MPI_Datatype datatype,
                              int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_init_send(buf, count, datatype, dest, tag, comm, request,
                       &mpi_api::rsend_init);
}

extern "C" int MPI_Ssend_init(void *buf, int count, MPI_Datatype datatype,
                              int dest, int tag, MPI_Comm comm, MPI_Request *request)
{
  return mpi_init_send(buf, count, datatype, dest, tag, comm, request,
                       &mpi_api::ssend_init);
}

extern "C" int MPI_Recv_init(void *buf, int count, MPI_Datatype datatype,
                             int source, int tag, MPI_Comm comm, MPI_Request *request)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_request* req;

  if (source == MPI_PROC_NULL) {
    return MPI_SUCCESS;
  }

  mpi->recv_init(count, mpi_type_id(datatype), get_mpi_source(source),
                 mpi_tag(tag), commptr, req);
  *request = mpi->add_request_ptr(req);
  mpi->add_buffer(*request, buf);

  return MPI_SUCCESS;
}

extern "C" int MPI_Start(MPI_Request *request)
{
  mpi_api* mpi = current_mpi();
  mpi_request* req = mpi->get_request(*request);
  mpi->start(req);
  return MPI_SUCCESS;
}

extern "C" int MPI_Startall(int count, MPI_Request array_of_requests[])
{
  std::vector<mpi_request*> reqs;
  mpi_api* mpi = current_mpi();
  for (int i = 0; i < count; i++) {
    MPI_Request r = array_of_requests[i];
    mpi_request* req = mpi->get_request(r);
    reqs.push_back(req);
  }

  mpi->startall(reqs);

  return MPI_SUCCESS;
}

extern "C" int MPI_Testall(int count, MPI_Request array_of_requests[],
                           int *flag, MPI_Status array_of_statuses[])
{
  //we are actually going to avoid testall for code reuse
  int tmp_flag;
  int cumul_flag = 1; //set to true for now
  for (int i = 0; i < count; ++i) {
    MPI_Status* status =
      array_of_statuses == MPI_STATUS_IGNORE ? MPI_STATUS_IGNORE
      : &array_of_statuses[i];
    MPI_Test(&array_of_requests[i], &tmp_flag, status);
    cumul_flag = cumul_flag && tmp_flag; //only return true if ALL tests complete
  }
  *flag = cumul_flag;
  return MPI_SUCCESS;
}

//----------------------------------------------------------------
// --- MPI Derived Datatype
// --- Presently, payloads won't work with derived datatypes
//----------------------------------------------------------------

extern "C" int MPI_Type_contiguous(int count, MPI_Datatype old_type,
                                   MPI_Datatype *new_type_p)
{
  mpi_api* mpi = current_mpi();
  mpi_type_id cxx_new;
  mpi->type_contiguous(count, mpi_type_id(old_type), cxx_new);
  *new_type_p = cxx_new;
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Type_vector(int count, int blocklength, int stride,
                               MPI_Datatype old_type, MPI_Datatype *newtype_p)
{
  mpi_api* mpi = current_mpi();
  mpi_type_id cxx_new;
  mpi->type_vector(count, blocklength, stride,
                   mpi_type_id(old_type), cxx_new,
                   true);
  *newtype_p = cxx_new;
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Type_hvector(int count, int blocklength, MPI_Aint stride,
                                MPI_Datatype old_type, MPI_Datatype *newtype_p)
{
  mpi_api* mpi = current_mpi();
  mpi_type_id cxx_new;
  mpi->type_vector(count, blocklength, (int) stride,
                   mpi_type_id(old_type), cxx_new, false);
  *newtype_p = cxx_new;
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Type_struct(int count, int blocklens[], MPI_Aint indices[],
                               MPI_Datatype old_types[], MPI_Datatype *newtype)
{
  mpi_api* mpi = current_mpi();
  std::vector<int> cxx_blocklens(count);
  std::vector<mpi_type_id> cxx_old_types(count);
  std::vector<int> cxx_ind(count);
  for (int i = 0; i < count; ++i) {
    cxx_blocklens[i] = blocklens[i];
    cxx_ind[i] = indices[i];
    cxx_old_types[i] = mpi_type_id(old_types[i]);
  }
  mpi_type_id cxx_new;
  mpi->type_struct(count, cxx_blocklens, cxx_ind, cxx_old_types, cxx_new);
  *newtype = cxx_new;
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Type_commit(MPI_Datatype *datatype)
{
  current_mpi()->type_commit(*datatype);
  return MPI_SUCCESS; //yes, error handling would be nice
}

extern "C" int MPI_Type_free(MPI_Datatype *datatype)
{
  current_mpi()->type_free(*datatype);
  return MPI_SUCCESS; //yes, error handling would be nice
}

//----------------------------------------------------------------
// --- Misc functions added to support parmetis and libtopomap
//----------------------------------------------------------------

extern "C" MPI_Comm
MPI_Comm_f2c(MPI_Fint comm)
{
  return comm;
}
extern "C" MPI_Fint
MPI_Comm_c2f(MPI_Comm comm)
{
  return comm;
}

extern "C" int
MPI_Keyval_create(MPI_Copy_function *copy_fn, MPI_Delete_function *delete_fn,
                  int *key, void *extra_state)
{
  keyval* k = new keyval(keyval::get_new_key(), copy_fn, delete_fn, extra_state);
  mpi_api* mpi = current_mpi();
  mpi->add_keyval(k->key(), k);
  *key = k->key();

  return MPI_SUCCESS;
}

extern "C" int
MPI_Attr_put(MPI_Comm comm, int key, void *attr_value)
{
  mpi_api* mpi = current_mpi();
  mpi->check_key(key);
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->attr_put(commptr, mpi->get_keyval(key), attr_value);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Attr_get(MPI_Comm comm, int keyval, void *attr_value, int *flag)
{
  mpi_api* mpi = current_mpi();
  mpi->check_key(keyval);
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->attr_get(commptr, mpi->get_keyval(keyval), attr_value, flag);
  return (*flag == 0) ? MPI_SUCCESS : MPI_ERR_UNKNOWN;
}

template<typename T>
void
move_some_stuff(T* dest, T* from, size_t fromoffset, size_t num)
{
  memmove(dest, &from[fromoffset], num);
}

/**
 * GH: This function does not call the Helgi MPI, it uses other functions in this file.
 * It is not optimized, and is definitely not as efficient as the real thing.
 * I just wanted to get it working.
 */
extern "C" int
MPI_Reduce_scatter(void *sendbuf, void *recvbuf, int *recvcnts,
                   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  mpi_api* mpi = current_mpi();
  mpi_comm* commptr = mpi->get_comm(comm);
  int size = commptr->size();
  int rank = commptr->rank();

  std::vector<int> counts;
  long total = 0;
  long myindex = 0;
  for (int i = 0; i < size; i++) {
    counts.push_back(recvcnts[i]);
    if (i == rank) {
      myindex = total;
    }
    total += recvcnts[i];
  }

  void* temp = NULL;
  size_t typesize;
  if (sendbuf != NULL) {

    switch (datatype) {
      case MPI_INT:
        typesize = sizeof(int);
        break;

      case MPI_SHORT:
        typesize = sizeof(short);
        break;

      case MPI_DOUBLE:
        typesize = sizeof(double);
        break;

      case MPI_LONG:
        typesize = sizeof(long);
        break;

      default:
        spkt_throw_printf(sprockit::spkt_error, "MPI_Reduce_scatter: error creating temp buffer");
    }

    temp = malloc(typesize * total);
  }

  MPI_Allreduce(sendbuf, temp, total, datatype, op, comm);

  if (recvbuf != NULL) {
    //for(int i = 0; i < recvcnts[rank]; i++){
    // recvbuf[i] = temp[myindex + i];
    switch (datatype) {
      case MPI_INT:
        move_some_stuff<int> ((int*) recvbuf, (int*) temp, myindex,
                              typesize * recvcnts[rank]);
        break;

      case MPI_SHORT:
        move_some_stuff<short> ((short*) recvbuf, (short*) temp, myindex,
                                typesize * recvcnts[rank]);
        break;

      case MPI_DOUBLE:
        move_some_stuff<double> ((double*) recvbuf, (double*) temp, myindex,
                                 typesize * recvcnts[rank]);
        break;

      case MPI_LONG:
        move_some_stuff<long> ((long*) recvbuf, (long*) temp, myindex,
                               typesize * recvcnts[rank]);
        break;
        // }
    }
  }

  if (temp != NULL) {
    free(temp);
  }

  /*  std::vector<int> counts;
   int total = 0;
   for (int i = 0; i < size; i++)
   {
   counts.push_back(recvcnts[i]);
   total += recvcnts[i];
   }

   sstmac::payload::const_ptr load =
   get_mpi_payload(sendbuf, 0, total, datatype);

   sstmac::payload::const_ptr res;
   mpi->reduce_scatter(counts, get_mpi_type(datatype), get_mpi_op(op), commptr,
   load, res);
   set_mpi_payload(recvbuf, recvcnts[commptr->rank().id], 0, datatype, res);*/

  return MPI_SUCCESS;
}

extern "C" int
MPI_Cartdim_get(MPI_Comm comm, int *ndims)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi->mpi_cartdim_get(commptr, ndims);
  return MPI_SUCCESS;
}

extern "C" int
MPI_Topo_test(MPI_Comm comm, int *topo_type)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);

  switch (commptr->topo_type()) {
    case mpi_comm::TOPO_NONE:
      *topo_type = MPI_UNDEFINED;
      break;

    case mpi_comm::TOPO_GRAPH:
      *topo_type = MPI_GRAPH;
      break;

    case mpi_comm::TOPO_CART:
      *topo_type = MPI_CART;
      break;

    default:
      spkt_throw_printf(sprockit::spkt_error, "MPI_Topo_test: mpicomm is of unknown type");

  }

  return MPI_SUCCESS;
}

extern "C" int
MPI_Group_translate_ranks(MPI_Group group1, int n, int *ranks1,
    MPI_Group group2, int *ranks2)
{
  mpi_api* mpi = current_mpi();

  mpi_group* g1 = mpi->get_group(group1);
  mpi_group* g2 = mpi->get_group(group2);

  for (int i = 0; i < n; i++) {
    ranks2[i] = MPI_UNDEFINED;

    if(ranks1[i] == MPI_PROC_NULL) {
      ranks2[i] = ranks1[i];
    }
    else {

      for (uint j = 0; j < g2->size(); j++) {

        if (g2->at(j) == g1->at(ranks1[i])) {
          ranks2[i] = j;
          break;
        }

      }
    }
  }
  return MPI_SUCCESS;
}

extern "C" int
MPI_Graph_neighbors(MPI_Comm comm, int rank, int maxneighbors, int *neighbors)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Graph_neighbors not implemented");
}

extern "C" int
MPI_Graph_neighbors_count(MPI_Comm comm, int rank, int *nneighbors)
{
  spkt_throw_printf(sprockit::unimplemented_error,
                   "MPI_Graph_neighbors_count not implemented");
}

/**
 @param group [in] group (handle)
 @param n [in] number of triplets in array ranges (integer)
 @param ranges [in] a one-dimensional array of integer triplets, of the form (first rank, last rank, stride) indicating ranks in group or processes to be included in newgroup.
 @param newgroup [out] new group derived from above, in the order defined by ranges (handle)
 */
extern "C" int
MPI_Group_range_incl(MPI_Group group, int n, int ranges[][3],
                     MPI_Group *newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp_ptr = mpi->get_group(group);
  std::vector<task_id> vec;

  if (n > grp_ptr->size()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "MPI_Group_range_incl: invalid group size %d", n);
  }

  for (int i = 0; i < n; i++) {
    int first = ranges[i][0];
    int last = ranges[i][1];
    int stride = ranges[i][2];
    if(stride == 0) {
      spkt_throw_printf(sprockit::value_error,
                       "sstmac_mpi::MPI_Group_range_incl - got a zero stride at position %d", i);
    }
    int num = (last - first) / stride;
    //SSTMAC_DEBUG << "first: " << first << ", last: " << last << ", stride: " <<
    //             stride << ", num: " << num << "\n";
    for(int j = 0; j <= num; j++) {
      int rank = first + j * stride;
      vec.push_back(grp_ptr->at(rank));
    }
  }

  if (vec.size() > 0) {
    //SSTMAC_DEBUG << "returning group of size " << vec.size() << " \n";
    mpi_group* newgrp = new mpi_group(vec);
    *newgroup = mpi->add_group_ptr(newgrp);
  }
  else {
    //SSTMAC_DEBUG << "nothing in vector, returning empty group \n";
    *newgroup = MPI_GROUP_EMPTY;
  }
  return MPI_SUCCESS;
}

/**
 @param keyval [in] Frees the integer key value (integer)
 */
extern "C" int
MPI_Keyval_free(int *keyval)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Keyval_free");
  return MPI_SUCCESS;
}

/**
 @param result [out] integer which is MPI_IDENT if the contexts and groups are the same, MPI_CONGRUENT if different contexts but identical groups, MPI_SIMILAR if different contexts but similar groups, and MPI_UNEQUAL otherwise
 */
extern "C" int
MPI_Comm_compare(MPI_Comm comm1, MPI_Comm comm2, int *result)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_compare");
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype (handle)
 @param extent [out] datatype extent (integer)
 */
extern "C" int
MPI_Type_extent(MPI_Datatype datatype, MPI_Aint *extent)
{
  //for now assume no padding and that type=size
  mpi_type* ty = get_mpi_type(datatype);
  *extent = ty->extent();
  return MPI_SUCCESS;
}

/**
 @param type [in] datatype (handle)
 @param newtype [out] copy of type (handle)
 */
extern "C" int
MPI_Type_dup(MPI_Datatype datatype, MPI_Datatype *newtype)
{
  mpi_type_id newty;
  mpi_api* mpi = current_mpi();
  mpi->type_dup(mpi_type_id(datatype), newty);
  *newtype = newty;
  return MPI_SUCCESS;
}

/**
 @param type [in] datatype whose identifier is to be set (handle) _type
 @param name [in] the character string which is remembered as the name (string)
 */
extern "C" int
MPI_Type_set_name(MPI_Datatype type, char *typename_)
{
  current_mpi()->type_set_name(mpi_type_id(type), std::string(typename_));
  return MPI_SUCCESS;
}

/**
 @param count [in] number of blocks -- also number of entries in indices and blocklens
 @param blocklens [in] number of elements in each block (array of nonnegative integers)
 @param indices [in] displacement of each block in multiples of old_type (array of integers) _old
 @param type [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int
MPI_Type_indexed(int count, int _blocklens_[], int _indices_[],
                 MPI_Datatype old_type, MPI_Datatype *newtype)
{
  mpi_type_id newty;
  mpi_api* mpi = current_mpi();
  std::vector<int> ind(count);
  for (int i = 0; i < count; i++) {
    ind[i] = _indices_[i];
  }
  mpi->type_indexed(count, _blocklens_, ind,
                    mpi_type_id(old_type),
                    newty, true,
                    MPI_COMBINER_INDEXED);
  *newtype = newty;
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype (handle)
 @param size [out] datatype size (integer)
 */
extern "C" int
MPI_Type_size(MPI_Datatype datatype, int *size)
{
  mpi_type* ty = get_mpi_type(datatype);
  *size = ty->packed_size();
  return MPI_SUCCESS;
}

/**
 @param type [in] datatype whose name is to be returned (handle) _type
 @param name [out] the name previously stored on the datatype, or a empty string if no such name exists (string)
 @param resultlen [out] length of returned name (integer)
 */
extern "C" int
MPI_Type_get_name(MPI_Datatype datatype, char *type_name,
                  int *resultlen)
{
  mpi_type* ty = get_mpi_type(datatype);
  std::string name = ty->label;

  std::copy(name.begin(), name.end(), type_name);
  type_name[name.size()] = '\0'; // don't forget the terminating 0


  *resultlen = name.size();
  return MPI_SUCCESS;
}

/**
 @param comm [in] Local (intra)communicator _local
 @param leader [in] Rank in local_comm of leader (often 0) _peer
 @param comm [in] Local (intra)communicator _local
 @param leader [in] Rank in local_comm of leader (often 0) _peer_comm _
 [in] Communicator used to communicate between a designated process in the other communicator. Significant only at the process in local_comm with rank local_leader. _remote_leader _
 [in] Rank in peer_comm of remote leader (often 0)
 @param tag [in] Message tag to use in constructing intercommunicator; if multiple MPI_Intercomm_creates are being made, they should use different tags (more precisely, ensure that the local and remote leaders are using different tags for each MPI_intercomm_create). _comm
 @param out [out] Created intercommunicator
 */
extern "C" int
MPI_Intercomm_create(MPI_Comm local_comm, int local_leader,
                     MPI_Comm peer_comm, int remote_leader, int tag, MPI_Comm *newintercomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Intercomm_create");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator (handle)
 @param size [out] number of processes in the remote group of comm (integer)
 */
extern "C" int MPI_Comm_remote_size(MPI_Comm comm, int *size)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_remote_size");
  return MPI_SUCCESS;
}

/**
 @param errorcode [in] Error code returned by an MPI routine
 @param errorclass [out] Error class associated with errorcode
 */
extern "C" int MPI_Error_class(int errorcode, int *errorclass)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Error_class");
  return MPI_SUCCESS;
}

/**
 @param errorcode [in] Error code returned by an MPI routine or an MPI error class
 @param string [out] Text that corresponds to the errorcode
 @param resultlen [out] Length of string
 */
extern "C" int MPI_Error_string(int errorcode, char *string, int *resultlen)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Error_string");
  return MPI_SUCCESS;
}

/**
 @param buffer [in] initial buffer address (choice)
 @param size [in] buffer size, in bytes (integer)
 */
extern "C" int MPI_Buffer_attach(void *buffer, int size)
{
  mpi_api* mpi = current_mpi();
  mpi->buffer_attach(size);
  int rank = mpi->comm_world()->rank();
  mpi->attach_buffer(buffer, size);
  return MPI_SUCCESS;
}

/**
 @param buffer [out] initial buffer address (choice)
 @param size [out] buffer size, in bytes (integer)
 */
extern "C" int MPI_Buffer_detach(void *buffer, int *size)
{
  mpi_api* mpi = current_mpi();
  mpi->buffer_detach(*size);
  void** cast = (void**)buffer;
  mpi->detach_buffer(cast, size);
  return MPI_SUCCESS;
}

/**
 @param count [in] list length (integer) _array_of
 @param requests [in] array of requests (array of handles)
 @param index [out] index of operation that completed, or MPI_UNDEFINED if none completed (integer)
 @param flag [out] true if one of the operations is complete (logical)
 @param status [out] status object (Status). May be MPI_STATUS_IGNORE.
 */
extern "C" int MPI_Testany(int count, MPI_Request array_of_requests[],
                           int *index, int *flag, MPI_Status *status)
{
  mpi_api* mpi = current_mpi();

  std::vector<mpi_request*> sst_reqs(count, NULL);
  for (int i = 0; i < count; i++) {
    sst_reqs[i] = mpi->get_request(array_of_requests[i]);
  }

  int ind;
  bool f;
  mpi_status sst_stat;
  mpi->testany(sst_reqs, ind, f, &sst_stat);
  if (f){
    complete_wait(mpi, sst_reqs[ind], &array_of_requests[ind], &sst_stat, status);
    *index = ind;
    *flag = 1;
  } else {
    *index = MPI_UNDEFINED;
    *flag = 0;
  }

  return MPI_SUCCESS; //yes, error handling would be nice
}

/**
 @param incount [in] length of array_of_requests (integer) _array_of
 @param requests [in] array of requests (array of handles)
 @param outcount [out] number of completed requests (integer) _array_of
 @param indices [out] array of indices of operations that completed (array of integers) _array_of
 @param statuses [out] array of status objects for operations that completed (array of Status). May be MPI_STATUSES_IGNORE.
 */
extern "C" int
MPI_Testsome(int incount, MPI_Request array_of_requests[],
             int *outcount, int array_of_indices[], MPI_Status array_of_statuses[])
{
  mpi_api* mpi = current_mpi();
  std::vector<mpi_request*> sst_reqs;
  for (int i = 0; i < incount; i++) {
    sst_reqs.push_back(mpi->get_request(array_of_requests[i]));
  }

  std::vector<mpi_status> sst_stats;
  std::vector<int> sst_inds;
  mpi->testsome(sst_reqs, sst_inds, sst_stats);

  for (int i=0; i < sst_inds.size(); ++i){
    int idx = sst_inds[i];
    complete_wait(mpi, sst_reqs[idx], &array_of_requests[idx],
            &sst_stats[idx], &array_of_statuses[idx]);
  }
  *outcount = sst_inds.size();

  return MPI_SUCCESS; //yes, error handling would be nice
}

/**
 @param status [in] status object (Status)
 @param flag [out] true if the request was cancelled, false otherwise (logical)
 */
extern "C" int
MPI_Test_cancelled(MPI_Status *status, int *flag)
{
  if (status) {
    *flag = status->CANCELLED;
  }
  return MPI_SUCCESS;
}

/**
 @param buf [in/out] initial address of send and receive buffer (choice)
 @param count [in] number of elements in send and receive buffer (integer)
 @param datatype [in] type of elements in send and receive buffer (handle)
 @param dest [in] rank of destination (integer)
 @param sendtag [in] send message tag (integer)
 @param source [in] rank of source (integer)
 @param recvtag [in] receive message tag (integer)
 @param comm [in] communicator (handle)
 @param status [out] status object (Status)
 */
extern "C" int
MPI_Sendrecv_replace(void *buf, int count,
                     MPI_Datatype datatype, int dest, int sendtag, int source, int recvtag,
                     MPI_Comm comm, MPI_Status *status)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Sendrecv_replace");
  return MPI_SUCCESS;
}

/**
 @param count [in] number of blocks -- also number of entries in indices and blocklens
 @param blocklens [in] number of elements in each block (array of nonnegative integers)
 @param indices [in] byte displacement of each block (array of MPI_Aint) _old
 @param type [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int
MPI_Type_hindexed(int count, int _blocklens_[],
                  MPI_Aint _indices_[], MPI_Datatype old_type, MPI_Datatype *newtype)
{
  mpi_type_id newty;
  mpi_api* mpi = current_mpi();
  std::vector<int> ind(count);
  for (int i = 0; i < count; i++) {
    ind[i] = _indices_[i];
  }
  mpi->type_indexed(count, _blocklens_, ind, mpi_type_id(old_type), newty, false,
                    MPI_COMBINER_HINDEXED);
  *newtype = newty;
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype (handle)
 @param displacement [out] displacement of lower bound from origin, in bytes (integer)
 */
extern "C" int
MPI_Type_lb(MPI_Datatype datatype, MPI_Aint *displacement)
{
  mpi_type* ty = get_mpi_type(datatype);
  *displacement = ty->lb();
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype (handle)
 @param displacement [out] displacement of upper bound from origin, in bytes (integer)
 */
extern "C" int
MPI_Type_ub(MPI_Datatype datatype, MPI_Aint *displacement)
{
  mpi_type* ty = get_mpi_type(datatype);
  *displacement = ty->ub();
  return MPI_SUCCESS;
}

/**
 @param status [in] status
 @param datatype [in] datatype
 @param elements [out] elements
 */
extern "C" int
MPI_Get_elements(MPI_Status *status, MPI_Datatype datatype,
                 int *elements)
{
  if (status->COUNT == 0){
    *elements = 0;
    return MPI_SUCCESS;
  }
  else {
    mpi_type* ty = get_mpi_type(datatype);
    *elements = ty->bytes_to_elements(status->BYTES_RECV);
    return MPI_SUCCESS; //yes, error handling would be nice
  }
}

/**
 @param inbuf [in] input buffer
 @param incount [in] input count
 @param datatype [in] datatype
 @param outbuf [in] output buffer
 @param outcount [in] output count
 @param position [in] position
 @param comm [in] communicator
 */
extern "C" int
MPI_Pack(void *inbuf, int incount, MPI_Datatype datatype,
         void *outbuf, int outsize, int *position, MPI_Comm comm)
{
  mpi_type* ty = get_mpi_type(datatype);
  int offset = *position;
  int insize = incount * ty->packed_size();
  int pack_size = std::min(insize, outsize);
  void* offset_buffer = ((char*)outbuf) + offset;
  ty->pack(inbuf, offset_buffer, pack_size);
  offset += pack_size;
  *position = offset;
  return MPI_SUCCESS;
}

/**
 @param inbuf [in] input buffer start (choice)
 @param insize [in] size of input buffer, in bytes (integer)
 @param position [in] current position in bytes (integer)
 @param outbuf [out] output buffer start (choice)
 @param outcount [in] number of items to be unpacked (integer)
 @param datatype [in] datatype of each output data item (handle)
 @param comm [in] communicator for packed message (handle)
 */
extern "C" int
MPI_Unpack(void *inbuf, int insize, int *position, void *outbuf,
           int outcount, MPI_Datatype datatype, MPI_Comm comm)
{
  mpi_type* ty = get_mpi_type(datatype);
  int bytes = ty->packed_size() * outcount;
  bytes = std::min(insize, bytes); //can't unpack more than insize
  int offset = *position;
  void* offset_inbuf = ((char*)inbuf) + offset;
  ty->unpack(offset_inbuf, outbuf, bytes);
  offset += bytes;
  *position = offset;
  return MPI_SUCCESS;
}

/**
 @param incount [in] count argument to packing call (integer)
 @param datatype [in] datatype argument to packing call (handle)
 @param comm [in] communicator argument to packing call (handle)
 @param size [out] upper bound on size of packed message, in bytes (integer)
 */
extern "C" int
MPI_Pack_size(int incount, MPI_Datatype datatype, MPI_Comm comm,
              int *size)
{
  mpi_type* ty = get_mpi_type(datatype);
  *size = incount * ty->packed_size();
  return MPI_SUCCESS;
}

/**
 @param function [in] user defined function (function)
 @param commute [in] true if commutative; false otherwise. (logical)
 @param op [out] operation (handle)
 */
extern "C" int
MPI_Op_create(MPI_User_function *function, int commute,
              MPI_Op *op)
{
  mpi_api* mpi = current_mpi();
  mpi_op* otype;
  mpi->op_create(function, commute, otype);
  *op = otype->id;
  return MPI_SUCCESS;
}

/**
 @param op [in] operation (handle)
 */
extern "C" int
MPI_Op_free(MPI_Op *op)
{
  mpi_api* mpi = current_mpi();

  mpi->op_free(*op);
  *op = MPI_OP_NULL;
  return MPI_SUCCESS;
}

/**
 @param group [in] group (handle) Output Parameter:
 @param size [out] number of processes in the group (integer)
 */
extern "C" int
MPI_Group_size(MPI_Group group, int *size)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp = mpi->get_group(group);
  *size = grp->size();
  return MPI_SUCCESS;
}

/**
 @param group [in] group (handle)
 @param rank [out] rank of the calling process in group, or MPI_UNDEFINED if the process is not a member (integer)
 */
extern "C" int
MPI_Group_rank(MPI_Group group, int *rank)
{
  mpi_api* mpi = current_mpi();
  int r = mpi->comm_world()->rank();
  mpi_group* grp = mpi->get_group(group);
  *rank = grp->rank_of_task(sstmac::sw::task_id());
  if(*rank == -1) {
    *rank = MPI_UNDEFINED;
  }
  return MPI_SUCCESS;
}

/**
 @param result [out] integer which is MPI_IDENT if the order and members of the two groups are the same, MPI_SIMILAR if only the members are the same, and MPI_UNEQUAL otherwise
 */
extern "C" int
MPI_Group_compare(MPI_Group group1, MPI_Group group2,
                  int *result)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp_ptr1 = mpi->get_group(group1);
  mpi_group* grp_ptr2 = mpi->get_group(group2);
  spkt_unordered_map<int, int> check;

  if (group1 == group2) {
    //that was easy
    *result = MPI_IDENT;
    return MPI_SUCCESS;
  }

  if(grp_ptr1->size() != grp_ptr2->size()) {
    *result = MPI_UNEQUAL;
    return MPI_SUCCESS;
  }

  for (int i = 0; i < grp_ptr1->size(); i++) {
    check[grp_ptr1->at(i)] = i;
  }

  *result = MPI_IDENT;
  for (int i = 0; i < grp_ptr2->size(); i++) {
    if (check.find(grp_ptr2->at(i)) == check.end()) {
      *result = MPI_UNEQUAL;
      return MPI_SUCCESS;
    }
    else if (check[grp_ptr2->at(i)] != i) {
      *result = MPI_SIMILAR;
    }

  }

  return MPI_SUCCESS;
}

/**
 @param newgroup [out] union group (handle)
 */
extern "C" int
MPI_Group_union(MPI_Group group1, MPI_Group group2,
                MPI_Group *newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp_ptr1 = mpi->get_group(group1);
  mpi_group* grp_ptr2 = mpi->get_group(group2);
  std::vector<task_id> vec;
  std::map<int, int> check;


  for(int i = 0; i < grp_ptr1->size(); i++) {
    check[grp_ptr1->at(i)] = grp_ptr1->at(i);
  }

  for (int i = 0; i < grp_ptr2->size(); i++) {
    check[grp_ptr2->at(i)] = grp_ptr2->at(i);
  }

  std::map<int, int>::iterator it, end = check.end();
  for (it = check.begin(); it != end; it++) {
    vec.push_back(task_id(it->second));
  }

  if (vec.size() > 0) {
    mpi_group* newgrp = new mpi_group(vec);
    *newgroup = mpi->add_group_ptr(newgrp);
  }
  else {
    *newgroup = MPI_GROUP_EMPTY;
  }
  return MPI_SUCCESS;
}

/**
 @param newgroup [out] intersection group (handle)
 */
extern "C" int
MPI_Group_intersection(MPI_Group group1, MPI_Group group2,
                       MPI_Group *newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp_ptr1 = mpi->get_group(group1);
  mpi_group* grp_ptr2 = mpi->get_group(group2);
  std::vector<task_id> vec;
  spkt_unordered_map<int, int> check;

  for (int i = 0; i < grp_ptr1->size(); i++) {
    check[grp_ptr1->at(i)] = grp_ptr1->at(i);
  }

  for (int i = 0; i < grp_ptr2->size(); i++) {
    if (check.find(grp_ptr2->at(i)) != check.end()) {
      vec.push_back(grp_ptr2->at(i));
    }
  }

  if (vec.size() > 0) {
    mpi_group* newgrp = new mpi_group(vec);
    *newgroup = mpi->add_group_ptr(newgrp);
  }
  else {
    *newgroup = MPI_GROUP_EMPTY;
  }
  return MPI_SUCCESS;
}

/**
 @param newgroup [out] difference group (handle)
 */
extern "C" int
MPI_Group_difference(MPI_Group group1, MPI_Group group2,
                     MPI_Group *newgroup)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Group_difference");
  return MPI_SUCCESS;
}

int
mpi_group_excl(
 spkt_unordered_set<int>& ignore,
 MPI_Group oldgroup,
 MPI_Group* newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* old_grp_ptr = mpi->get_group(oldgroup);
  std::vector<task_id> retvec;

  if (ignore.size() > old_grp_ptr->size()) {
    spkt_throw_printf(sprockit::illformed_error,
        "MPI_Group_excl: invalid rank array size %d (group is of size %d)",
        ignore.size(), old_grp_ptr->size());
  }

  int nproc_old_grp = old_grp_ptr->size();
  int nproc_new_grp = nproc_old_grp - ignore.size();
  retvec.resize(nproc_new_grp);
  const std::vector<task_id>& tl = old_grp_ptr->ids();
  bool comm_world = tl.size() == 0;
  int grpidx = 0;
  for (int i = 0; i < nproc_old_grp; i++) {
    if (ignore.find(i) == ignore.end()){
      retvec[grpidx] = comm_world ? task_id(i) : tl[i];
      ++grpidx;
    }
  }

  if (retvec.size() > 0) {
    mpi_group* newgrp = new mpi_group(retvec);
    *newgroup = mpi->add_group_ptr(newgrp);
  }
  else {
    *newgroup = MPI_GROUP_EMPTY;
  }
  return MPI_SUCCESS;
}

/**
 @param group [in] group (handle)
 @param n [in] number of elements in array ranks (integer)
 @param ranks [in] array of integer ranks in group not to appear in newgroup
 @param newgroup [out] new group derived from above, preserving the order defined by group (handle)
 */
extern "C" int
MPI_Group_excl(MPI_Group group, int n, int *ranks,
               MPI_Group *newgroup)
{
  spkt_unordered_set<int> ignore;
  for (int i=0; i < n; ++i){
    ignore.insert(ranks[i]);

  }
  return mpi_group_excl(ignore, group, newgroup);
}

/**
 @param group [in] group (handle)
 @param n [in] number of elements in array ranks (integer)
 @param ranges [in] a one-dimensional array of integer triplets of the form (first rank, last rank, stride), indicating the ranks in group of processes to be excluded from the output group newgroup .
 @param newgroup [out] new group derived from above, preserving the order in group (handle)
 */
extern "C" int
MPI_Group_range_excl(MPI_Group group, int n, int ranges[][3],
                     MPI_Group *newgroup)
{
  mpi_api* mpi = current_mpi();
  mpi_group* grp_ptr = mpi->get_group(group);
  std::vector<task_id> retvec;

  spkt_unordered_set<int> ignore;
  for (int i = 0; i < n; i++) {
    int first = ranges[i][0];
    int last = ranges[i][1];
    int stride = ranges[i][2];

    if (stride == 0) {
      spkt_throw_printf(sprockit::value_error,
          "sstmac_mpi::MPI_Group_range_excl: found 0 stride at position %d", i);
    }

    int num = (last - first) / stride;
    for(int j  = 0; j <= num; j++ ) {
      int rank = first + j * stride;
      ignore.insert(rank);
    }
  }

  return mpi_group_excl(ignore, group, newgroup);
}

/**
 @param comm [in] communicator to test (handle)
 @param flag [out] true if this is an inter-communicator(logical)
 */
extern "C" int
MPI_Comm_test_inter(MPI_Comm comm, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_test_inter");
  return MPI_SUCCESS;
}

/**
 @param comm [in] Communicator (must be an intercommunicator) (handle)
 @param group [out] remote group of communicator (handle)
 */
extern "C" int
MPI_Comm_remote_group(MPI_Comm comm, MPI_Group *group)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_remote_group");
  return MPI_SUCCESS;
}

/**
 @param comm [in] Intercommunicator (handle)
 @param high [in] Used to order the groups within comm (logical) when creating the new communicator. This is a boolean value; the group that sets high true has its processes ordered _after_ the group that sets this value to false. If all processes in the intercommunicator provide the same value, the choice of which group is ordered first is arbitrary. _comm
 @param out [out] Created intracommunicator (handle)
 */
extern "C" int
MPI_Intercomm_merge(MPI_Comm intercomm, int high,
                    MPI_Comm *newintracomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Intercomm_merge");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to which attribute is attached (handle)
 @param keyval [in] The key value of the deleted attribute (integer)
 */
extern "C" int
MPI_Attr_delete(MPI_Comm comm, int keyval)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Attr_delete");
  return MPI_SUCCESS;
}

/**
 @param nnodes [in] number of nodes in a grid (integer)
 @param ndims [in] number of cartesian dimensions (integer)
 @param dims [in/out] integer array of size ndims specifying the number of nodes in each dimension. A value of 0 indicates that [MPI_Dims_create](MPI_Dims_create.html) should fill in a suitable value.
 */
extern "C" int
MPI_Dims_create(int nnodes, int ndims, int *dims)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Dims_create");
  return MPI_SUCCESS;
}

/**
 @param old [in] input communicator without topology (handle)
 @param nnodes [in] number of nodes in graph (integer)
 @param index [in] array of integers describing node degrees (see below)
 @param edges [in] array of integers describing graph edges (see below)
 @param reorder [in] ranking may be reordered (true) or not (false) (logical) _comm
 @param graph [out] communicator with graph topology added (handle)
 */
extern "C" int
MPI_Graph_create(MPI_Comm comm_old, int nnodes, int *index,
                 int *edges, int reorder, MPI_Comm *comm_graph)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Graph_create");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator for group with graph structure (handle)
 @param nnodes [out] number of nodes in graph (integer)
 @param nedges [out] number of edges in graph (integer)
 */
extern "C" int
MPI_Graphdims_get(MPI_Comm comm, int *nnodes, int *nedges)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Graphdims_get");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator with graph structure (handle)
 @param maxindex [in] length of vector index in the calling program (integer)
 @param maxedges [in] length of vector edges in the calling program (integer)
 @param index [out] array of integers containing the graph structure (for details see the definition of MPI_GRAPH_CREATE)
 @param edges [out] array of integers containing the graph structure
 */
extern "C" int
MPI_Graph_get(MPI_Comm comm, int maxindex, int maxedges,
              int *index, int *edges)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Graph_get");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator with cartesian structure (handle) _remain
 @param dims [in] the ith entry of remain_dims specifies whether the ith dimension is kept in the subgrid (true) or is dropped (false) (logical vector)
 @param newcomm [out] communicator containing the subgrid that includes the calling process (handle)
 */
extern "C" int
MPI_Cart_sub(MPI_Comm comm, int *remain_dims, MPI_Comm *newcomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Cart_sub");
  return MPI_SUCCESS;
}

/**
 @param comm [in] input communicator (handle)
 @param ndims [in] number of dimensions of Cartesian structure (integer)
 @param dims [in] integer array of size ndims specifying the number of processes in each coordinate direction
 @param periods [in] logical array of size ndims specifying the periodicity specification in each coordinate direction
 @param newrank [out] reordered rank of the calling process; MPI_UNDEFINED if calling process does not belong to grid (integer)
 */
extern "C" int
MPI_Cart_map(MPI_Comm comm_old, int ndims, int *dims,
             int *periods, int *newrank)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Cart_map");
  return MPI_SUCCESS;
}

/**
 @param comm [in] input communicator (handle)
 @param nnodes [in] number of graph nodes (integer)
 @param index [in] integer array specifying the graph structure, see MPI_GRAPH
 @param CREATE edges _
 [in] integer array specifying the graph structure
 @param newrank [out] reordered rank of the calling process; MPI_UNDEFINED if the calling process does not belong to graph (integer)
 */
extern "C" int
MPI_Graph_map(MPI_Comm comm_old, int nnodes, int *index,
              int *edges, int *newrank)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Graph_map");
  return MPI_SUCCESS;
}

/**
 @param name [out] A unique specifier for the actual (as opposed to virtual) node. This must be an array of size at least MPI_MAX_PROCESSOR_NAME.
 @param resultlen [out] Length (in characters) of the name
 */
extern "C" int
MPI_Get_processor_name(char *name, int *resultlen)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Get_processor_name");
  return MPI_SUCCESS;
}

/**
 @param version [out] Version of MPI
 @param subversion [out] Subversion of MPI
 */
extern "C" int
MPI_Get_version(int *version, int *subversion)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Get_version");
  return MPI_SUCCESS;
}

/**
 @param function [in] user defined error handling procedure
 @param errhandler [out] MPI error handler (handle)
 */
extern "C" int
MPI_Errhandler_create(MPI_Handler_function *function,
                                     MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Errhandler_create");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to get the error handler from (handle)
 @param errhandler [in] MPI error handler currently associated with communicator (handle)
 */
extern "C" int
MPI_Errhandler_set(MPI_Comm comm, MPI_Errhandler errhandler)
{
  current_mpi()->add_err_handler(comm, errhandler);
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to get the error handler from (handle)
 @param errhandler [out] MPI error handler currently associated with communicator (handle)
 */
extern "C" int
MPI_Errhandler_get(MPI_Comm comm, MPI_Errhandler *errhandler)
{
  *errhandler = current_mpi()->get_err_handler(comm);
  return MPI_SUCCESS;
}

/**
 @param errhandler [in/out] MPI error handler (handle). Set to MPI_ERRHANDLER_NULL on exit.
 */
extern "C" int
MPI_Errhandler_free(MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Errhandler_free");
  return MPI_SUCCESS;
}

/**
 @param level [in] Profiling level _... _
 other arguments (see notes)
 */
extern "C" int
MPI_Pcontrol(const int level, ...)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Pcontrol");
  return MPI_SUCCESS;
}

/**
 @param name [in] a port name (string)
 */
extern "C" int
MPI_Close_port(char *port_name)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Close_port");
  return MPI_SUCCESS;
}

/**
 @param name [in] port name (string, used only on root)
 @param info [in] implementation-dependent information (handle, used only on root)
 @param root [in] rank in comm of root node (integer)
 @param comm [in] comm intracommunicator over which call is collective (handle)
 @param newcomm [out] intercommunicator with client as remote group (handle)
 */
extern "C" int
MPI_Comm_accept(char *port_name, MPI_Info info, int root,
                               MPI_Comm comm, MPI_Comm *newcomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_accept");
  return MPI_SUCCESS;
}

/**
 @param name [in] network address (string, used only on root)
 @param info [in] implementation-dependent information (handle, used only on root)
 @param root [in] rank in comm of root node (integer)
 @param comm [in] intracommunicator over which call is collective (handle)
 @param newcomm [out] intercommunicator with server as remote group (handle)
 */
extern "C" int
MPI_Comm_connect(char *port_name, MPI_Info info, int root,
                 MPI_Comm comm, MPI_Comm *newcomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_connect");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator (handle)
 */
extern "C" int
MPI_Comm_disconnect(MPI_Comm * comm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_disconnect");
  return MPI_SUCCESS;
}

/**
 @param parent [out] the parent communicator (handle)
 */
extern "C" int
MPI_Comm_get_parent(MPI_Comm *parent)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_get_parent");
  return MPI_SUCCESS;
}

/**
 @param fd [in] socket file descriptor
 @param intercomm [out] new intercommunicator (handle)
 */
extern "C" int
MPI_Comm_join(int fd, MPI_Comm *intercomm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_join");
  return MPI_SUCCESS;
}

/**
 @param command [in] name of program to be spawned (string, significant only at root)
 @param argv [in] arguments to command (array of strings, significant only at root)
 @param maxprocs [in] maximum number of processes to start (integer, significant only at root)
 @param info [in] a set of key-value pairs telling the runtime system where and how to start the processes (handle, significant only at root)
 @param root [in] rank of process in which previous arguments are examined (integer)
 @param comm [in] intracommunicator containing group of spawning processes (handle)
 @param intercomm [out] intercommunicator between original group and the newly spawned group (handle) _array_of
 @param errcodes [out] one code per process (array of integer)
 */
extern "C" int
MPI_Comm_spawn(char *command, char *_argv_[], int maxprocs,
               MPI_Info info, int root, MPI_Comm comm, MPI_Comm *intercomm,
               int _array_of_errcodes_[])
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_spawn");
  return MPI_SUCCESS;
}

/**
 @param count [in] number of commands (positive integer, significant to MPI only at root _array_of
 @param commands [in] programs to be executed (array of strings, significant only at root) _array_of
 @param argv [in] arguments for commands (array of array of strings, significant only at root) _array_of
 @param maxprocs [in] maximum number of processes to start for each command (array of integer, significant only at root) _array_of
 @param info [in] info objects telling the runtime system where and how to start processes (array of handles, significant only at root)
 @param root [in] rank of process in which previous arguments are examined (integer)
 @param comm [in] intracommunicator containing group of spawning processes (handle)
 @param intercomm [out] intercommunicator between original group and newly spawned group (handle) _array_of
 @param errcodes [out] one error code per process (array of integer)
 */
extern "C" int
MPI_Comm_spawn_multiple(int count, char *_array_of_commands_[],
                        char**_array_of_argv_[], int _array_of_maxprocs_[],
                        MPI_Info _array_of_info_[], int root, MPI_Comm comm, MPI_Comm *intercomm,
                        int _array_of_errcodes_[])
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_spawn_multiple");
  return MPI_SUCCESS;
}

/**
 @param name [in] a service name (string)
 @param info [in] implementation-specific information (handle) _port
 @param name [in] a service name (string) _info _
 [in] implementation-specific information (handle) _port_name _
 [out] a port name (string)
 */
extern "C" int MPI_Lookup_name(char *service_name, MPI_Info info,
                               char *port_name)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Lookup_name");
  return MPI_SUCCESS;
}

/**
 @param info [in] implementation-specific information on how to establish a port for [MPI_Comm_accept](MPI_Comm_accept.html) (handle) _port
 @param name [out] newly established port (string)
 */
extern "C" int MPI_Open_port(MPI_Info info, char *port_name)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Open_port");
  return MPI_SUCCESS;
}

/**
 @param name [in] a service name to associate with the port (string)
 @param info [in] implementation-specific information (handle) _port
 @param name [in] a service name to associate with the port (string) _info _
 [in] implementation-specific information (handle) _port_name _
 [in] a port name (string)
 */
extern "C" int MPI_Publish_name(char *service_name, MPI_Info info,
                                char *port_name)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Publish_name");
  return MPI_SUCCESS;
}

/**
 @param name [in] a service name (string)
 @param info [in] implementation-specific information (handle) _port
 @param name [in] a service name (string) _info _
 [in] implementation-specific information (handle) _port_name _
 [in] a port name (string)
 */
extern "C" int MPI_Unpublish_name(char *service_name, MPI_Info info,
                                  char *port_name)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Unpublish_name");
  return MPI_SUCCESS;
}

/**
 @param addr [in] initial address of buffer (choice) _origin
 @param count [in] number of entries in buffer (nonnegative integer) _origin
 @param datatype [in] datatype of each buffer entry (handle) _target
 @param rank [in] rank of target (nonnegative integer) _target
 @param disp [in] displacement from start of window to beginning of target buffer (nonnegative integer) _target
 @param count [in] number of entries in buffer (nonnegative integer) _origin
 @param datatype [in] datatype of each buffer entry (handle) _target_rank _
 [in] rank of target (nonnegative integer) _target_disp _
 [in] displacement from start of window to beginning of target buffer (nonnegative integer) _target_count _
 [in] number of entries in target buffer (nonnegative integer) _target_datatype _
 [in] datatype of each entry in target buffer (handle)
 @param op [in] predefined reduce operation (handle)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Accumulate(void *origin_addr, int origin_count,
                              MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
                              int target_count, MPI_Datatype target_datatype, MPI_Op op, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Accumulate");
#if 0
  mpi_api* mpi = current_mpi();
  mpi_window* winptr = mpi->get_window(win);
  sstmac::payload::const_ptr load = get_mpi_payload_func(origin_addr, 0,
                                    origin_count, origin_datatype, true);

  mpi_type* ty = get_mpi_type(target_datatype);
  mpi_id trank(target_rank);
  mpi_request* req;
  win_rma_info info;

  info.buffer = origin_addr;
  info.count = origin_count;
  info.datatype = origin_datatype;
  info.win = winptr;
  info.type = win_rma_info::acc;
  info.op = get_mpi_op(op);

  mpi->win_accumulate(req, load, target_count, ty, trank, target_disp, info.op,
                      winptr);

  pending_rmas[req] = info;

  return MPI_SUCCESS;
#endif
}

/**
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_complete(MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_complete");
  return MPI_SUCCESS;
}

/**
 @param base [in] initial address of window (choice)
 @param size [in] size of window in bytes (nonnegative integer) _disp
 @param unit [in] local unit size for displacements, in bytes (positive integer)
 @param info [in] info argument (handle)
 @param comm [in] communicator (handle)
 @param win [out] window object returned by the call (handle)
 */
extern "C" int MPI_Win_create(void *base, MPI_Aint size, int disp_unit,
                              MPI_Info info, MPI_Comm comm, MPI_Win *win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_create");
#if 0
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);
  mpi_window* winptr;
  mpi_info* infptr;
  MPI_Win newwin = win_counters[rank]++;
  if (info != MPI_INFO_NULL) {
    infptr = get_info(rank, info);
  }
  mpi->win_create(base, size, disp_unit, infptr, commptr, winptr, newwin);

  if (winptr) {
    *win = add_win*(rank, winptr, newwin);
  }

  return MPI_SUCCESS;
#endif
}

/**
 @param assert [in] program assertion (integer)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_fence(int assert, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_fence");
#if 0
  mpi_api* mpi = current_mpi();

  int rank = mpi->comm_world()->rank();
  mpi_window* winptr = get_window(rank, win);

  spkt_unordered_map<mpi_request*, payload::const_ptr> loads;

  bool barr = (assert & MPI_MODE_NOPRECEDE) == 0;
  mpi->win_fence(winptr, barr, loads);

  //set payloads for gets
  spkt_unordered_map<mpi_request*, payload::const_ptr>::iterator it, end =
    loads.end();
  for (it = loads.begin(); it != end; it++) {
    if (!it->first) {
      spkt_throw_printf(sprockit::spkt_error, "sstmac_mpi::MPI_Win_fence - null request");
    }
    if (it->second) {
      if (pending_rmas.find(it->first) == pending_rmas.end()) {
        spkt_throw_printf(sprockit::spkt_error,
           "sstmac_mpi::MPI_Win_fence: didn't find get with request %s",
           it->first->to_string().c_str());
      }
      win_rma_info i = pending_rmas[it->first];
      if (i.type == win_rma_info::get) {
        set_mpi_payload_rma(i.buffer, i.count, i.datatype, it->second);
      }
      pending_rmas.erase(it->first);
    }
  }
#endif
  return MPI_SUCCESS;
}

/**
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_free(MPI_Win *win)
{
  mpi_api* mpi = current_mpi();
  mpi_window* winptr = mpi->get_window(*win);

  mpi->win_free(winptr);

  *win = MPI_WIN_NULL;

  return MPI_SUCCESS;
}

/**
 @param win [in] window object (handle)
 @param group [out] group of processes which share access to the window (handle)
 */
extern "C" int MPI_Win_get_group(MPI_Win win, MPI_Group *group)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_get_group");
  return MPI_SUCCESS;
}

/**
 @param type [in] Indicates whether other processes may access the target window at the same time (if MPI_LOCK_SHARED) or not (MPI_LOCK_EXCLUSIVE)
 @param rank [in] rank of locked window (nonnegative integer)
 @param assert [in] Used to optimize this call; zero may be used as a default. See notes. (integer)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_lock(int lock_type, int r, int assert, MPI_Win win)
{
  mpi_api* mpi = current_mpi();
  mpi_window* winptr = mpi->get_window(win);

  mpi_api::LOCK_TYPE t;
  if (lock_type == MPI_LOCK_SHARED) {
    t = mpi_api::mpi_lock_shared_;
  }
  else if (lock_type == MPI_LOCK_EXCLUSIVE) {
    t = mpi_api::mpi_lock_exclusive_;
  }
  else {
    spkt_throw_printf(sprockit::spkt_error, "MPI_Win_lock:: unknown lock type %d", lock_type);
  }

  mpi_api::WIN_FLAGS f;
  if (assert == MPI_MODE_NOCHECK) {
    f = mpi_api::mpi_win_mode_nocheck_;
  }
  else {
    f = mpi_api::mpi_flag_none_;
  }

  mpi->win_lock(t, mpi_id(r), f, winptr);
  return MPI_SUCCESS;
}

/**
 @param group [in] group of origin processes (handle)
 @param assert [in] Used to optimize this call; zero may be used as a default. See notes. (integer)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_post(MPI_Group group, int assert, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_post");
  return MPI_SUCCESS;
}

/**
 @param group [in] group of target processes (handle)
 @param assert [in] Used to optimize this call; zero may be used as a default. See notes. (integer)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_start(MPI_Group group, int assert, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_start");
  return MPI_SUCCESS;
}

/**
 @param win [in] window object (handle)
 @param flag [out] success flag (logical)
 */
extern "C" int MPI_Win_test(MPI_Win win, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_test");
  return MPI_SUCCESS;
}

/**
 @param rank [in] rank of window (nonnegative integer)
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_unlock(int r, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_unlock");
#if 0
  mpi_api* mpi = current_mpi();
  mpi_window* winptr = mpi->get_window(win);

  spkt_unordered_map<mpi_request*, payload::const_ptr> loads;

  mpi->win_unlock(mpi_id(r), winptr, loads);

  //set payloads for gets
  spkt_unordered_map<mpi_request*, payload::const_ptr>::iterator it, end =
    loads.end();
  for (it = loads.begin(); it != end; it++) {
    if (!it->first) {
      spkt_throw_printf(sprockit::spkt_error, "sstmac_mpi::MPI_Win_fence - null request");
    }
    if (it->second) {
      if (pending_rmas.find(it->first) == pending_rmas.end()) {
        std::cout << "request: " << it->first->to_string() << "\n";
        spkt_throw_printf(sprockit::spkt_error,
                         "sstmac_mpi::MPI_Win_fence - didn't find get with request");
      }
      win_rma_info i = pending_rmas[it->first];
      if (i.type == win_rma_info::get) {
        set_mpi_payload_rma(i.buffer, i.count, i.datatype, it->second);
      }
      pending_rmas.erase(it->first);
    }
  }

  return MPI_SUCCESS;
#endif
}

/**
 @param win [in] window object (handle)
 */
extern "C" int MPI_Win_wait(MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_wait");
  return MPI_SUCCESS;
}

/**
 @param sendbuf [in] starting address of send buffer (choice)
 @param sendcounts [in] integer array equal to the group size specifying the number of elements to send to each processor (integer)
 @param sdispls [in] integer array (of length group size). Entry j specifies the displacement in bytes (relative to sendbuf) from which to take the outgoing data destined for process j
 @param sendtypes [in] array of datatypes (of length group size). Entry j specifies the type of data to send to process j (handle)
 @param recvbuf [out] address of receive buffer (choice)
 @param recvcounts [in] integer array equal to the group size specifying the number of elements that can be received from each processor (integer)
 @param rdispls [in] integer array (of length group size). Entry i specifies the displacement in bytes (relative to recvbuf) at which to place the incoming data from process i
 @param recvtypes [in] array of datatypes (of length group size). Entry i specifies the type of data received from process i (handle)
 @param comm [in] communicator (handle)
 */
extern "C" int MPI_Alltoallw(void *sendbuf, int *sendcnts, int *sdispls,
                             MPI_Datatype *sendtypes, void *recvbuf, int *recvcnts, int *rdispls,
                             MPI_Datatype *recvtypes, MPI_Comm comm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Alltoallw");
  return MPI_SUCCESS;
}

/**
 @param sendbuf [in] starting address of send buffer (choice)
 @param recvbuf [out] starting address of receive buffer (choice)
 @param count [in] number of elements in input buffer (integer)
 @param datatype [in] data type of elements of input buffer (handle)
 @param op [in] operation (handle)
 @param comm [in] communicator (handle)
 */
extern "C" int MPI_Exscan(void *sendbuf, void *recvbuf, int count,
                          MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Exscan");
  return MPI_SUCCESS;
}

/**
 @param errorclass [out] New error class
 */
extern "C" int MPI_Add_error_class(int *errorclass)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Add_error_class");
  return MPI_SUCCESS;
}

/**
 @param errorclass [in] Error class to add an error code.
 @param errorcode [out] New error code for this error class.
 */
extern "C" int MPI_Add_error_code(int errorclass, int *errorcode)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Add_error_code");
  return MPI_SUCCESS;
}

/**
 @param errorcode [in] error code or class (integer)
 @param string [in] text corresponding to errorcode (string)
 */
extern "C" int MPI_Add_error_string(int errorcode, char *string)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Add_error_string");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator with error handler (handle)
 @param errorcode [in] error code (integer)
 */
extern "C" int MPI_Comm_call_errhandler(MPI_Comm comm, int errorcode)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_call_errhandler");
  return MPI_SUCCESS;
}

/**
 @param fn [in] Copy callback function for keyval _comm_delete_attr
 @param fn [in] Copy callback function for keyval _comm_delete_attr_fn _
 [in] Delete callback function for keyval

 _comm
 @param keyval [out] key value for future access (integer)  _extra
 @param state [in] Extra state for callback functions
 */
extern "C" int MPI_Comm_create_keyval(
  MPI_Comm_copy_attr_function *comm_copy_attr_fn,
  MPI_Comm_delete_attr_function *comm_delete_attr_fn, int *comm_keyval,
  void *extra_state)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_create_keyval");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to which attribute is attached (handle) _comm
 @param keyval [in] The key value of the deleted attribute (integer)
 */
extern "C" int MPI_Comm_delete_attr(MPI_Comm comm, int commkeyval_)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_delete_attr");
  return MPI_SUCCESS;
}

/**
 @param keyval [in] Frees the integer key value (integer)
 */
extern "C" int MPI_Comm_free_keyval(int *comm_keyval)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_free_keyval");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to which attribute is attached (handle) _comm
 @param keyval [in] key value (integer) _attribute
 @param val [out] attribute value, unless flag = false
 @param flag [out] true if an attribute value was extracted; false if no attribute is associated with the key
 */
extern "C" int MPI_Comm_get_attr(MPI_Comm comm, int commkeyval_,
                                 void *attribute_val, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_get_attr");
  return MPI_SUCCESS;
}

/**
 @param comm [in] Communicator to get name of (handle) _comm
 @param name [out] On output, contains the name of the communicator. It must be an array of size at least MPI_MAX_OBJECT_NAME.
 @param resultlen [out] Number of characters in name
 */
extern "C" int MPI_Comm_get_name(MPI_Comm comm, char *commname_, int *resultlen)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);
  std::string n = commptr->name();
  strcpy(commname_, n.c_str());
  *resultlen = n.size();
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to which attribute will be attached (handle)
 @param keyval [in] key value, as returned by [MPI_Comm_create_keyval](MPI_Comm_create_keyval.html) (integer) _attribute
 @param val [in] attribute value
 */
extern "C" int MPI_Comm_set_attr(MPI_Comm comm, int commkeyval_,
                                 void *attribute_val)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_set_attr");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator to name (handle) _comm
 @param name [in] Name for communicator (string)
 */
extern "C" int MPI_Comm_set_name(MPI_Comm comm, char *commname_)
{
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_comm* commptr = mpi->get_comm(comm);
  commptr->set_name(commname_);
  return MPI_SUCCESS;
}

/**
 @param fh [in] MPI file with error handler (handle)
 @param errorcode [in] error code (integer)
 */
extern "C" int MPI_File_call_errhandler(MPI_File fh, int errorcode)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_File_call_errhandler");
  return MPI_SUCCESS;
}

/**
 @param request [in] Generalized request to mark as complete
 */
extern "C" int MPI_Grequest_complete(MPI_Request request)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Grequest_complete");
  return MPI_SUCCESS;
}

/**
 @param fn [in] callback function invoked when request status is queried (function) _free
 @param fn [in] callback function invoked when request status is queried (function) _free
 @param fn [in] callback function invoked when request status is queried (function) _free_fn _
 [in] callback function invoked when request is freed (function) _cancel_fn _
 [in] callback function invoked when request is cancelled (function) _extra
 @param state [in] Extra state passed to the above functions.
 @param request [out] Generalized request (handle)
 */
extern "C" int MPI_Grequest_start(MPI_Grequest_query_function *query_fn,
                                  MPI_Grequest_free_function *free_fn,
                                  MPI_Grequest_cancel_function *cancel_fn, void *extra_state,
                                  MPI_Request *request)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Grequest_start");
  return MPI_SUCCESS;
}

/**
 @param argc [in] Pointer to the number of arguments
 @param argv [in] Pointer to the argument vector
 @param required [in] Level of desired thread support
 @param provided [out] Level of provided thread support
 */
extern "C" int MPI_Init_thread(int *argc, char ***argv, int required,
                               int *provided)
{
  *provided = required;
  return MPI_Init(argc, argv);
}

/**
 @param flag [out] Flag is true if [MPI_Init](MPI_Init.html) or [MPI_Init_thread](MPI_Init_thread.html) has been called by this thread and false otherwise. (logical)
 */
extern "C" int MPI_Is_thread_main(int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Is_thread_main");
  return MPI_SUCCESS;
}

/**
 @param provided [out] Level of thread support provided. This is the same value that was returned in the provided argument in [MPI_Init_thread](MPI_Init_thread.html).
 */
extern "C" int MPI_Query_thread(int *provided)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Query_thread");
  return MPI_SUCCESS;
}

/**
 @param status [in] status to associate cancel flag with (Status)
 @param flag [in] if true indicates request was cancelled (logical)
 */
extern "C" int MPI_Status_set_cancelled(MPI_Status *status, int flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Status_set_cancelled");
  return MPI_SUCCESS;
}

/**
 @param status [in] status to associate count with (Status)
 @param datatype [in] datatype associated with count (handle)
 @param count [in] number of elements to associate with status (integer)
 */
extern "C" int MPI_Status_set_elements(MPI_Status *status,
                                       MPI_Datatype datatype, int count)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Status_set_elements");
  return MPI_SUCCESS;
}

/**
 @param fn [in] copy callback function for type_keyval (function) _type_delete_attr
 @param fn [in] copy callback function for type_keyval (function) _type_delete_attr_fn _
 [in] delete callback function for type_keyval (function) _type
 @param keyval [out] key value for future access (integer) _extra
 @param state [in] extra state for callback functions
 */
extern "C" int MPI_Type_create_keyval(
  MPI_Type_copy_attr_function *type_copy_attr_fn,
  MPI_Type_delete_attr_function *type_delete_attr_fn, int *type_keyval,
  void *extra_state)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_keyval");
  return MPI_SUCCESS;
}

/**
 @param type [in] MPI datatype to which attribute is attached (handle) _type
 @param keyval [in] The key value of the deleted attribute (integer)
 */
extern "C" int MPI_Type_delete_attr(MPI_Datatype type, int typekeyval_)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_delete_attr");
  return MPI_SUCCESS;
}

/**
 @param keyval [in] Frees the integer key value (integer)
 */
extern "C" int MPI_Type_free_keyval(int *type_keyval)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_free_keyval");
  return MPI_SUCCESS;
}

/**
 @param type [in] datatype to which the attribute is attached (handle) _type
 @param keyval [in] key value (integer) _attribute
 @param val [out] attribute value, unless flag = false
 @param flag [out] false if no attribute is associated with the key (logical)
 */
extern "C" int MPI_Type_get_attr(MPI_Datatype type, int typekeyval_,
                                 void *attribute_val, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_get_attr");
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype _max
 @param integers [in] max integers _max
 @param addresses [in] max addresses _max
 @param datatypes [in] max datatypes _array_of
 @param integers [in] max integers _max
 @param addresses [in] max addresses _max
 @param datatypes [in] max datatypes _array_of_integers _
 [out] integers _array_of_addresses _
 [out] addresses _array_of_datatypes _
 [out] datatypes
 */
extern "C" int MPI_Type_get_contents(MPI_Datatype datatype, int max_integers,
                                     int max_addresses, int max_datatypes, int _array_of_integers_[],
                                     MPI_Aint _array_of_addresses_[], MPI_Datatype _array_of_datatypes_[])
{
  mpi_api* mpi = current_mpi();
  mpi_type* ty = get_mpi_type(datatype);
  ty->contents(_array_of_integers_, (int*)_array_of_addresses_,
                 (long*)_array_of_datatypes_);
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype _num
 @param integers [out] num integers _num
 @param addresses [out] num addresses _num
 @param datatypes [out] num datatypes
 @param combiner [out] combiner
 */
extern "C" int MPI_Type_get_envelope(MPI_Datatype datatype, int *num_integers,
                                     int *num_addresses, int *num_datatypes, int *combiner)
{
  mpi_api* mpi = current_mpi();
  mpi_type* ty = get_mpi_type(datatype);
  ty->envelope(num_integers, num_addresses, num_datatypes, combiner);
  return MPI_SUCCESS;
}

/**
 @param type [in] MPI Datatype to which attribute will be attached (handle)
 @param keyval [in] key value, as returned by [MPI_Type_create_keyval](MPI_Type_create_keyval.html) (integer) _attribute
 @param val [in] attribute value
 */
extern "C" int MPI_Type_set_attr(MPI_Datatype type, int typekeyval_,
                                 void *attribute_val)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_set_attr");
  return MPI_SUCCESS;
}

/**
 @param typeclass [in] generic type specifier (integer)
 @param size [in] size, in bytes, of representation (integer)
 @param datatype [out] datatype with correct type, size (handle)
 */
extern "C" int MPI_Type_match_size(int typeclass, int size,
                                   MPI_Datatype *datatype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_match_size");
  return MPI_SUCCESS;
}

/**
 @param win [in] window with error handler (handle)
 @param errorcode [in] error code (integer)
 */
extern "C" int MPI_Win_call_errhandler(MPI_Win win, int errorcode)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_call_errhandler");
  return MPI_SUCCESS;
}

/**
 @param fn [in] copy callback function for win_keyval (function) _win_delete_attr
 @param fn [in] copy callback function for win_keyval (function) _win_delete_attr_fn _
 [in] delete callback function for win_keyval (function) _win
 @param keyval [out] key value for future access (integer) _extra
 @param state [in] extra state for callback functions
 */
extern "C" int MPI_Win_create_keyval(
  MPI_Win_copy_attr_function *win_copy_attr_fn,
  MPI_Win_delete_attr_function *win_delete_attr_fn, int *win_keyval,
  void *extra_state)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_create_keyval");
  return MPI_SUCCESS;
}

/**
 @param win [in] window from which the attribute is deleted (handle) _win
 @param keyval [in] key value (integer)
 */
extern "C" int MPI_Win_delete_attr(MPI_Win win, int winkeyval_)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_delete_attr");
  return MPI_SUCCESS;
}

/**
 @param keyval [in] key value (integer)
 */
extern "C" int MPI_Win_free_keyval(int *win_keyval)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_free_keyval");
  return MPI_SUCCESS;
}

/**
 @param win [in] window to which the attribute is attached (handle) _win
 @param keyval [in] key value (integer) _attribute
 @param val [out] attribute value, unless flag is false
 @param flag [out] false if no attribute is associated with the key (logical)
 */
extern "C" int MPI_Win_get_attr(MPI_Win win, int winkeyval_,
                                void *attribute_val, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_get_attr");
  return MPI_SUCCESS;
}

/**
 @param win [in] window whose name is to be returned (handle) _win
 @param name [out] the name previously stored on the window, or a empty string if no such name exists (string)
 @param resultlen [out] length of returned name (integer)
 */
extern "C" int MPI_Win_get_name(MPI_Win win, char *winname_, int *resultlen)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_get_name");
  return MPI_SUCCESS;
}

/**
 @param win [in] MPI window object to which attribute will be attached (handle)
 @param keyval [in] key value, as returned by [MPI_Win_create_keyval](MPI_Win_create_keyval.html) (integer) _attribute
 @param val [in] attribute value
 */
extern "C" int MPI_Win_set_attr(MPI_Win win, int winkeyval_,
                                void *attribute_val)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_set_attr");
  return MPI_SUCCESS;
}

/**
 @param win [in] window whose identifier is to be set (handle) _win
 @param name [in] the character string which is remembered as the name (string)
 */
extern "C" int MPI_Win_set_name(MPI_Win win, char *winname_)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_set_name");
  return MPI_SUCCESS;
}

/**
 @param fh [in] C file handle (handle)
 */
extern "C" int MPI_File_c2f(MPI_File mpi_fh)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_File_c2f");
  return MPI_SUCCESS;
}

/**
 @param size [in] size of memory segment in bytes (nonnegative integer)
 @param info [in] info argument (handle)
 @param baseptr [out] pointer to a pointer to the beginning of the memory segment allocated
 */
extern "C" int MPI_Alloc_mem(MPI_Aint size, MPI_Info info, void *baseptr)
{
  void** cast = (void**) baseptr;
  *cast = malloc(size);
  return MPI_SUCCESS;
}

/**
 @param function user defined error handling procedure (function)
 @param errhandler MPI error handler (handle)
 */
extern "C" int MPI_Comm_create_errhandler(MPI_Comm_errhandler_fn *function,
    MPI_Errhandler *errhandler)
{
  // spkt_throw_printf(sprockit::unimplemented_error, "MPI_Comm_create_errhandler");
  return MPI_SUCCESS;
}

/**
 @param comm [in] communicator (handle)
 @param errhandler [out] handler currently associated with communicator (handle)
 */
extern "C" int MPI_Comm_get_errhandler(MPI_Comm comm,
                                       MPI_Errhandler *errhandler)
{
  return MPI_Errhandler_get(comm, errhandler);
}

/**
 @param comm [in] communicator (handle)
 @param errhandler [in] new error handler for communicator (handle)
 */
extern "C" int MPI_Comm_set_errhandler(MPI_Comm comm, MPI_Errhandler errhandler)
{
  return MPI_Errhandler_set(comm, errhandler);
}

/**
 @param function [in] user defined error handling procedure (function)
 @param errhandler [out] MPI error handler (handle)
 */
extern "C" int MPI_File_create_errhandler(
  MPI_File_errhandler_function *function, MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_File_create_errhandler");
  return MPI_SUCCESS;
}

/**
 @param file [in] MPI file (handle)
 @param errhandler [out] handler currently associated with file (handle)
 */
extern "C" int MPI_File_get_errhandler(MPI_File file,
                                       MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_File_get_errhandler");
  return MPI_SUCCESS;
}

/**
 @param file [in] MPI file (handle)
 @param errhandler [in] new error handler for file (handle)
 */
extern "C" int MPI_File_set_errhandler(MPI_File file, MPI_Errhandler errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_File_set_errhandler");
  return MPI_SUCCESS;
}

/**
 @param flag [out] Flag is true if [MPI_Finalize](MPI_Finalize.html) has been called and false otherwise. (logical)
 */
extern "C" int MPI_Finalized(int *flag)
{
  mpi_api* mpi = current_mpi();
  *flag = mpi->finalized();
  return MPI_SUCCESS;
}

/**
 @param base [in] initial address of memory segment allocated by MPI_ALLOC_MEM (choice)
 */
extern "C" int MPI_Free_mem(void *base)
{
  free(base);
  return MPI_SUCCESS;
}

/**
 @param location [in] location in caller memory (choice)
 @param address [out] address of location (address)
 */
extern "C" int MPI_Get_address(void *location, MPI_Aint *address)
{
  *address = reinterpret_cast<MPI_Aint>(location);
  return MPI_SUCCESS;
}

/**
 @param info [out] info object created (handle)
 */
extern "C" int MPI_Info_create(MPI_Info *info)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_create");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param key [in] key (string)
 */
extern "C" int MPI_Info_delete(MPI_Info info, char *key)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_delete");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param newinfo [out] duplicate of info object (handle)
 */
extern "C" int MPI_Info_dup(MPI_Info info, MPI_Info *newinfo)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_dup");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object to be freed (handle)
 */
extern "C" int MPI_Info_free(MPI_Info *info)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_free");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param key [in] key (string)
 @param valuelen [in] length of value argument (integer)
 @param value [out] value (string)
 @param flag [out] true if key defined, false if not (boolean)
 */
extern "C" int MPI_Info_get(MPI_Info info, char *key, int valuelen,
                            char *value, int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_get");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param nkeys [out] number of defined keys (integer)
 */
extern "C" int MPI_Info_get_nkeys(MPI_Info info, int *nkeys)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_get_nkeys");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param n [in] key number (integer) _key_
 [out] key (string). The maximum number of characters is MPI_MAX_INFO_KEY.
 */
extern "C" int MPI_Info_get_nthkey(MPI_Info info, int n, char *key)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_get_nthkey");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param key [in] key (string)
 @param valuelen [out] length of value argument (integer)
 @param flag [out] true if key defined, false if not (boolean)
 */
extern "C" int MPI_Info_get_valuelen(MPI_Info info, char *key, int *valuelen,
                                     int *flag)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_get_valuelen");
  return MPI_SUCCESS;
}

/**
 @param info [in] info object (handle)
 @param key [in] key (string)
 @param value [in] value (string)
 */
extern "C" int MPI_Info_set(MPI_Info info, char *key, char *value)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Info_set");
  return MPI_SUCCESS;
}

/**
 @param datarep [in] data representation (string)
 @param inbuf [in] input buffer start (choice)
 @param incount [in] number of input data items (integer)
 @param datatype [in] datatype of each input data item (handle)
 @param outbuf [out] output buffer start (choice)
 @param outcount [in] output buffer size, in bytes (integer)
 @param position [in/out] current position in buffer, in bytes (integer)
 */
extern "C" int MPI_Pack_external(char *datarep, void *inbuf, int incount,
                                 MPI_Datatype datatype, void *outbuf, MPI_Aint outcount, MPI_Aint *position)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Pack_external");
  return MPI_SUCCESS;
}

/**
 @param datarep [in] data representation (string)
 @param incount [in] number of input data items (integer)
 @param datatype [in] datatype of each input data item (handle)
 @param size [out] output buffer size, in bytes (integer)
 */
extern "C" int MPI_Pack_external_size(char *datarep, int incount,
                                      MPI_Datatype datatype, MPI_Aint *size)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Pack_external_size");
  return MPI_SUCCESS;
}

/**
 @param request [in] request (handle)
 @param flag [out] true if operation has completed (logical)
 @param status [out] status object (Status). May be MPI_STATUS_IGNORE.
 */
extern "C" int
MPI_Request_get_status(MPI_Request request, int *flag,
                       MPI_Status *status)
{
  mpi_api* mpi = current_mpi();

  mpi_request* req = mpi->get_request(request);


  if (!req->is_cancelled()) {
    if (req->is_complete()) {
      if (status != MPI_STATUS_IGNORE){
        fill_status(status, &req->status(), mpi);
      }
      *flag = true;
    }
    else {
      *flag = false;
    }
  }
  else {
    *flag = true;
    status->CANCELLED = true;
  }
  return MPI_SUCCESS;
}

/**
 @param size [in] size of process group (positive integer)
 @param rank [in] rank in process group (nonnegative integer)
 @param ndims [in] number of array dimensions as well as process grid dimensions (positive integer) _array_of
 @param gsizes [in] number of elements of type oldtype in each dimension of global array (array of positive integers) _array_of
 @param distribs [in] distribution of array in each dimension (array of state) _array_of
 @param dargs [in] distribution argument in each dimension (array of positive integers) _array_of
 @param psizes [in] size of process grid in each dimension (array of positive integers)
 @param order [in] array storage order flag (state)
 @param oldtype [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_darray(int size, int rank, int ndims,
                                      int _array_of_gsizes_[], int _array_of_distribs_[], int _array_of_dargs_[],
                                      int _array_of_psizes_[], int order, MPI_Datatype oldtype,
                                      MPI_Datatype *newtype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_darray");
  return MPI_SUCCESS;
}

/**
 @param count [in] number of blocks --- also number of entries in displacements and blocklengths (integer)
 @param blocklengths [in] number of elements in each block (array of nonnegative integers)
 @param displacements [in] byte displacement of each block (array of integer)
 @param oldtype [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_hindexed(int count, int _blocklengths_[],
                                        MPI_Aint _displacements_[], MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_hindexed");
  return MPI_SUCCESS;
}

/**
 @param count [in] number of blocks (nonnegative integer)
 @param blocklength [in] number of elements in each block (nonnegative integer)
 @param stride [in] number of bytes between start of each block (integer)
 @param oldtype [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_hvector(int count, int blocklength,
                                       MPI_Aint stride, MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_hvector");
  return MPI_SUCCESS;
}

/**
 @param count [in] length of array of displacements (integer)
 @param blocklength [in] size of block (integer) _array_of
 @param displacements [in] array of displacements (array of integer)
 @param oldtype [in] old datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_indexed_block(int count, int blocklength,
    int _array_of_displacements_[], MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  mpi_type_id newty;
  mpi_api* mpi = current_mpi();
  int* blocklens = (int*) malloc(sizeof(int) * count);
  for (int i = 0; i < count; i++) {
    blocklens[i] = blocklength;
  }

  std::vector<int> ind(count);
  for (int i = 0; i < count; i++) {
    ind[i] = _array_of_displacements_[i];
  }
  mpi->type_indexed(count, blocklens, ind, mpi_type_id(oldtype), newty, true,
                    MPI_COMBINER_INDEXED_BLOCK);
  free(blocklens);
  *newtype = newty;
  return MPI_SUCCESS;
  return MPI_SUCCESS;
}

/**
 @param oldtype [in] input datatype (handle)
 @param lb [in] new lower bound of datatype (integer)
 @param extent [in] new extent of datatype (integer)
 @param newtype [out] output datatype (handle)
 */
extern "C" int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb,
                                       MPI_Aint extent, MPI_Datatype *newtype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_resized");
  return MPI_SUCCESS;
}

/**
 @param count [in] number of blocks (integer) --- also number of entries in arrays array_of_types, array_of_displacements and array_of
 @param blocklengths array_of
 @param blocklength [in] number of elements in each block (array of integer) _array_of
 @param displacements [in] byte displacement of each block (array of integer) _array_of
 @param types [in] type of elements in each block (array of handles to datatype objects)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_struct(int count, int _array_of_blocklengths_[],
                                      MPI_Aint _array_of_displacements_[], MPI_Datatype _array_of_types_[],
                                      MPI_Datatype *newtype)
{
  mpi_api* mpi = current_mpi();
  std::vector<int> cxx_blocklens(count);
  std::vector<mpi_type_id> cxx_old_types(count);
  std::vector<int> cxx_ind(count);
  for (int i = 0; i < count; ++i) {
    cxx_blocklens[i] = _array_of_blocklengths_[i];
    cxx_ind[i] = _array_of_displacements_[i];
    cxx_old_types[i] = mpi_type_id(_array_of_types_[i]);
  }
  mpi_type_id cxx_new;
  mpi->type_struct(count, cxx_blocklens, cxx_ind, cxx_old_types, cxx_new);
  *newtype = cxx_new;
  return MPI_SUCCESS; //yes, error handling would be nice
}

/**
 @param ndims [in] number of array dimensions (positive integer) _array_of
 @param sizes [in] number of elements of type oldtype in each dimension of the full array (array of positive integers) _array_of
 @param subsizes [in] number of elements of type oldtype in each dimension of the subarray (array of positive integers) _array_of
 @param starts [in] starting coordinates of the subarray in each dimension (array of nonnegative integers)
 @param order [in] array storage order flag (state)
 @param oldtype [in] array element datatype (handle)
 @param newtype [out] new datatype (handle)
 */
extern "C" int MPI_Type_create_subarray(int ndims, int _array_of_sizes_[],
                                        int _array_of_subsizes_[], int _array_of_starts_[], int order,
                                        MPI_Datatype oldtype, MPI_Datatype *newtype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Type_create_subarray");
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype to get information on (handle)
 @param lb [out] lower bound of datatype (integer)
 @param extent [out] extent of datatype (integer)
 */
extern "C" int
MPI_Type_get_extent(MPI_Datatype datatype, MPI_Aint *lb, MPI_Aint *extent)
{
  //for now assume no padding and that type=size
  mpi_type* ty = get_mpi_type(datatype);
  *extent = ty->extent();
  *lb = ty->lb();
  return MPI_SUCCESS;
}

/**
 @param datatype [in] datatype to get information on (handle) _true
 @param lb [out] true lower bound of datatype (integer) _true
 @param extent [out] true size of datatype (integer)
 */
extern "C" int
MPI_Type_get_true_extent(MPI_Datatype datatype, MPI_Aint *true_lb, MPI_Aint *true_extent)
{
  //for now assume no padding and that type=size
  mpi_type* ty = get_mpi_type(datatype);
  *true_lb = ty->true_lb();
  *true_extent = ty->true_extent();
  return MPI_SUCCESS;
}

/**
 @param datarep [in] data representation (string)
 @param inbuf [in] input buffer start (choice)
 @param insize [in] input buffer size, in bytes (integer)
 @param outcount [in] number of output data items (integer)
 @param datatype [in] datatype of output data item (handle)
 @param position [in/out] current position in buffer, in bytes (integer)
 @param outbuf [out] output buffer start (choice)
 */
extern "C" int
MPI_Unpack_external(char *datarep, void *inbuf, MPI_Aint insize,
                    MPI_Aint *position, void *outbuf, int outcount, MPI_Datatype datatype)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Unpack_external");
  return MPI_SUCCESS;
}

/**
 @param function [in] user defined error handling procedure (function)
 @param errhandler [out] MPI error handler (handle)
 */
extern "C" int
MPI_Win_create_errhandler(MPI_Win_errhandler_function *function,
    MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_create_errhandler");
  return MPI_SUCCESS;
}

/**
 @param win [in] window (handle)
 @param errhandler [out] error handler currently associated with window (handle)
 */
extern "C" int
MPI_Win_get_errhandler(MPI_Win win, MPI_Errhandler *errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_get_errhandler");
  //*errhandler = win_err_handlers[win];
  return MPI_SUCCESS;
}

/**
 @param win [in] window (handle)
 @param errhandler [in] new error handler for window (handle)
 */
extern "C" int
MPI_Win_set_errhandler(MPI_Win win, MPI_Errhandler errhandler)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Win_set_errhandler");
  //win_err_handlers[win] = errhandler;
  return MPI_SUCCESS;
}

/**
 @param origin_addr [in] Address of the buffer in which to receive the data
 @param origin_count [in] number of entries in origin buffer (nonnegative integer)
 @param origin_datatype [in] datatype of each entry in origin buffer (handle)
 @param target_rank [in] rank of target (nonnegative integer)
 @param target_disp [in] displacement from window start to the beginning of the target buffer (nonnegative integer)
 @param target_count [in] number of entries in target buffer (nonnegative integer)
 @param target_datatype [in] datatype of each entry in target buffer (handle)
 @param win [in] window object used for communication (handle)
 */
extern "C" int
MPI_Get(void *origin_addr, int origin_count,
        MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
        int target_count, MPI_Datatype target_datatype, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Get");
#if 0
  mpi_api* mpi = current_mpi();
  mpi_window* winptr = mpi->get_window(win);
  sstmac::payload::const_ptr load;
  mpi_id trank(target_rank);
  mpi_request* req;
  win_rma_info info;
  info.buffer = origin_addr;
  info.count = origin_count;
  info.datatype = origin_datatype;
  info.win = winptr;
  info.type = win_rma_info::get;

  mpi->win_get(req, origin_count,
               mpi_type_id(origin_datatype),
               trank, target_disp, target_count,
               mpi_type_id(target_datatype),
               winptr);

  pending_rmas[req] = info;
#endif
  return MPI_SUCCESS;
}

/**
 @param origin_addr [in] initial address of origin buffer (choice)
 @param origin_count [in] number of entries in origin buffer (nonnegative integer)
 @param origin_datatype [in] datatype of each entry in origin buffer (handle)
 @param target_rank [in] rank of target (nonnegative integer)
 @param target_disp [in] displacement from start of window to target buffer (nonnegative integer)
 @param target_count [in] number of entries in target buffer (nonnegative integer)
 @param target_datatype [in] datatype of each entry in target buffer (handle)
 @param win [in] window object used for communication (handle)
 */
extern "C" int
MPI_Put(void *origin_addr, int origin_count,
         MPI_Datatype origin_datatype, int target_rank, MPI_Aint target_disp,
         int target_count, MPI_Datatype target_datatype, MPI_Win win)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Put");
#if 0
  mpi_api* mpi = current_mpi();
  int rank = mpi->comm_world()->rank();
  mpi_window* winptr = get_window(rank, win);
  sstmac::payload::const_ptr load = get_mpi_payload_func(origin_addr, 0,
                                    origin_count, origin_datatype, true);

  mpi_id trank(target_rank);
  mpi_request* req;
  win_rma_info info;

  info.buffer = origin_addr;
  info.count = origin_count;
  info.datatype = origin_datatype;
  info.win = winptr;
  info.type = win_rma_info::put;

  mpi->win_put(req, load, target_count, mpi_type_id(target_datatype),
                trank, target_disp, winptr);

  pending_rmas[req] = info;
#endif
  return MPI_SUCCESS;
}

/**
 @param sendbuf starting address of send buffer (choice)
 @param recvcount element count per block (non-negative integer)
 @param datatype data type of elements of input buffer (handle)
 @param op operation (handle)
 @param comm communicator (handle)
 @param recvbuf starting address of receive buffer (choice)
 */
extern "C" int MPI_Reduce_scatter_block(void *sendbuf, void *recvbuf,
                                        int recvcount, MPI_Datatype datatype, MPI_Op op, MPI_Comm comm)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Reduce_scatter_block");
  return MPI_SUCCESS;
}

/**
 @param comm_old input communicator (handle)
 @param indegree size of sources and sourceweights arrays (non-negative integer)
 @param sources ranks of processes for which the calling process is a destination (array of non-negative integers)
 @param sourceweights weights of the edges into the calling process (array of non-negative integers or MPI_UNWEIGHTED)
 @param outdegree size of destinations and destweights arrays (non-negative integer)
 @param destinations ranks of processes for which the calling process is a source (array of non-negative integers)
 @param destweights weights of the edges out of the calling process (array of non-negative integers or MPI_UNWEIGHTED)
 @param info hints on optimization and interpretation of weights (handle)
 @param reorder the ranks may be reordered (true) or not (false) (logical)
 @param comm_dist_graph communicator with distributed graph topology (handle)
 */
extern "C" int
MPI_Dist_graph_create_adjacent(MPI_Comm comm_old, int indegree,
                               int sources[], int sourceweights[], int outdegree, int destinations[],
                               int destweights[], MPI_Info info, int reorder, MPI_Comm *comm_dist_graph)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Dist_graph_create_adjacent");
  return MPI_SUCCESS;
}

/**
 @param comm_old input communicator (handle)
 @param n number of source nodes for which this process specifies edges (non-negative integer)
 @param sources array containing the n source nodes for which this process specifies edges (array of non-negative integers)
 @param degrees array specifying the number of destinations for each source node in the source node array (array of non-negative integers)
 @param destinations destination nodes for the source nodes in the source node array (array of non-negative integers)
 @param weights weights for source to destination edges (array of non-negative integers or MPI_UNWEIGHTED)
 @param info hints on optimization and interpretation of weights (handle)
 @param reorder the process may be reordered (true) or not (false) (logical)
 @param comm_dist_graph communicator with distributed graph topology added (handle)
 */
extern "C" int
MPI_Dist_graph_create(MPI_Comm comm_old, int n, int sources[],
                      int degrees[], int destinations[], int weights[], MPI_Info info,
                      int reorder, MPI_Comm *comm_dist_graph)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Dist_graph_create_adjacent");
  return MPI_SUCCESS;
}

/**
 @param comm communicator with distributed graph topology (handle)
 @param indegree number of edges into this process (non-negative integer)
 @param outdegree number of edges out of this process (non-negative integer)
 @param weighted false if MPI_UNWEIGHTED was supplied during creation, true otherwise (logical)
 */
extern "C" int
MPI_Dist_graph_neighbors_count(MPI_Comm comm, int *indegree,
                               int *outdegree, int *weighted)
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Dist_graph_neighbors_count");
  return MPI_SUCCESS;
}

/**
 @param comm communicator with distributed graph topology (handle)
 @param maxindegree size of sources and sourceweights arrays (non-negative integer)
 @param maxoutdegree size of destinations and destweights arrays (non-negative integer)
 @param sources processes for which the calling process is a destination (array of non-negative integers)
 @param sourceweights weights of the edges into the calling process (array of non-negative integers)
 @param destinations processes for which the calling process is a source (array of non-negative integers)
 @param destweights weights of the edges out of the calling process (array of non-negative integers)
 */
extern "C" int
MPI_Dist_graph_neighbors(MPI_Comm comm, int maxindegree, int sources[],
                         int sourceweights[], int maxoutdegree, int destinations[],
                         int destweights[])
{
  spkt_throw_printf(sprockit::unimplemented_error, "MPI_Dist_graph_neighbors");
  return MPI_SUCCESS;
}


