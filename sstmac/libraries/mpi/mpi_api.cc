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

#include <sstmac/common/thread_lock.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/messages/sleep_message.h>

#include <sstmac/libraries/mpi/mpi_queue/mpi_queue.h>
#include <sstmac/libraries/mpi/mpi_queue/user_thread_mpi_queue.h>

#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_api_persistent.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_request.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective_payload.h>

#include <sstmac/hardware/node/node.h>
#include <sstmac/hardware/topology/structured_topology.h>

#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/api.h>
#include <sstmac/software/process/thread.h>

#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_strategy.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allreduce.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_core_allgather.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/engines/mpi_barrier_engine.h>
#include <sstmac/libraries/mpi/mpi_strategies/mpi_collective/mpi_collective.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_cart.h>
#include <sstmac/libraries/mpi/mpi_types.h>
#include <sstmac/libraries/mpi/mpi_server.h>

#include <sstmac/software/libraries/unblock_handler.h>
#include <sstmac/software/launch/hostname_allocation.h>

#include <sprockit/errors.h>
#include <sprockit/statics.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/malloc.h>
#include <sprockit/keyword_registration.h>

ImplementAPI(sstmac::sw, mpi_api, "mpi")


#define mpi_api_debug(flags, ...) \
  mpi_debug(worldcomm_->rank(), flags, __VA_ARGS__) 


DeclareDebugSlot(mpi_check)
RegisterDebugSlot(mpi_check,
    "validation flag that performs various sanity checks to ensure MPI application"
    " runs and terminates cleanly");

sprockit::StaticNamespaceRegister mpi_ns_reg("mpi");
sprockit::StaticNamespaceRegister queue_ns_reg("queue");

#define maybe_skip_comm_world()  if (comm == worldcomm_ && skip_comm_world_) return os_->now()

namespace sstmac {
namespace sw {

SpktRegister("mpi", api, mpi_api, "Create bindings for MPI runtime");

key::category mpi_api::default_key_category("MPI");
key::category mpi_api::poll_key_category("MPI Poll");
key::category mpi_api::memcpy_key_category("MPI Memcpy");


static sprockit::need_delete_statics<mpi_api> del_statics;

mpi_api*
sstmac_mpi()
{
  thread* t = operating_system::current_thread();
  return t->get_api<mpi_api> ();
}

//
// Catch calls on an un-initialized MPI environment.
//
inline void
mpi_api::assert_initialized() const
{
  if (status_ != is_initialized) {
    spkt_throw_printf(sprockit::illformed_error,
       "MPI function called before initialiation");
  }
}

// Test whether a communicator is valid for sending or receiving.
inline void
validcomm(mpi_comm* comm)
{
  if (comm == mpi_comm::comm_null) {
    spkt_throw_printf(sprockit::value_error,
                     "can't communicate on MPI_COMM_NULL");
  }
}

// Test whether we are sending a valid number of elements.
inline void
validcount(int count)
{
  if (count < 0) {
    spkt_throw_printf(sprockit::value_error,
                     "negative array length");
  }
}

// Test whether the send type is acceptable.
inline void
validtype(mpi_type* type)
{
  if (type->packed_size() <= 0) {
    spkt_throw_printf(sprockit::value_error,
                     "MPI type %s does not have size greater than zero",
                     type->label.c_str());
  }
}

// Test whether the given id is valid for sending.
inline void
validsendid(mpi_id target, mpi_comm* comm)
{
  if (int(target) < 0 || target >= comm->size()) {
    spkt_throw_printf(sprockit::value_error,
                     "id: %d sending to invalid rank %d",
                     int(comm->rank()), int(target));
  }
}

// Test whether the given id is valid for receiving.
inline void
validrecvid(mpi_id source, mpi_comm* comm)
{
  if ((int(source) < 0 || source >= comm->size()) && (source
      != mpi::any_source)) {
    spkt_throw_printf(sprockit::value_error,
                     "invalid source rank %d",
                     int(source));
  }
}

// Test wheter the given tag is valid for sending.
inline void
validsendtag(mpi_tag tag)
{
  if (int(tag) < 0) {
    spkt_throw_printf(sprockit::value_error,
                     "send tag must be >= 0");
  }
}

// Test wheter the given tag is valid for receiving.
inline void
validrecvtag(mpi_tag tag)
{
  if ((int(tag) < 0) && (tag != mpi::any_tag))
    spkt_throw_printf(sprockit::value_error,
      "receive-tag must be >= 0 or mpitag::any_tag");
}

// Test paramters for sending a single element.
inline void
validsend(int count, mpi_id target,
          mpi_tag tag, mpi_comm* comm)
{
  validcount(count);
  validcomm(comm);
  validsendid(target, comm);
  validsendtag(tag);
}

// Test paramters for receiving a single element.
inline void
validrecv(int count, mpi_id source,
          mpi_tag tag, mpi_comm* comm)
{
  validcount(count);
  validcomm(comm);
  validrecvid(source, comm);
  validrecvtag(tag);
}

// Test paramters for sending a vector of elements.
inline void
validsends(const std::vector<int> &counts,
           mpi_id target, mpi_tag tag, mpi_comm* comm)
{
  std::vector<int>::const_iterator it, end = counts.end();
  for (it = counts.begin(); it != end; ++it) {
    validcount(*it);
  }
  // validtype(sendtype);
  validcomm(comm);
  validsendid(target, comm);
  validsendtag(tag);
}

// Test paramters for receiving a vector of elements.
inline void
validrecvs(const std::vector<int> &counts,
           mpi_id source, mpi_tag tag, mpi_comm* comm)
{
  std::vector<int>::const_iterator it, end = counts.end();
  for (it = counts.begin(); it != end; ++it) {
    validcount(*it);
  }
  //  validtype(recvtype);
  validcomm(comm);
  validrecvid(source, comm);
  validrecvtag(tag);
}

// Test arguments for a broadcast.
inline void
validbcast(int count, mpi_type_id sendtype, mpi_id root,
           mpi_tag tag, mpi_comm* comm)
{
  validcount(count);
  //   validtype(sendtype);
  validcomm(comm);
  validsendid(root, comm);
  validsendtag(tag);
}

// Test whether all the given ranks are valid in a communicator.
inline void
validranks(const std::vector<mpi_id> &rank, mpi_comm* comm)
{
  const int ub = comm->size();
  std::vector<mpi_id>::const_iterator it, end = rank.end();
  for (it = rank.begin(); it != end; ++it) {
    if (it->id_ < 0 || it->id_ >= ub) {
      spkt_throw(sprockit::value_error, "invalid rank found");
    }
  }
}

//
// Build a new mpiapi.
//
mpi_api::mpi_api() :
  api(mpi_api::default_key_category),
  saved_payload_(payload::null()),
  status_(is_fresh),
  next_type_id_(0),
  group_counter_(0),
  info_counter_(0),
  req_counter_(0),
  queue_(0),
  strategy_(0),
  comm_factory_(0),
  worldcomm_(0),
  selfcomm_(0)
{
}

void
mpi_api::init_factory_params(sprockit::sim_parameters* params)
{
  skip_comm_world_ = params->get_optional_bool_param("skip_comm_world", false);

  strategy_ = new mpi_strategy;
  api::init_factory_params(params);
  mpi_allreduce_strategy* default_allreduce = mpi_allreduce_strategy_factory::get_optional_param(
    "allreduce", "rabenseifner", params);
  strategy_->set_allreduce(default_allreduce);


  mpi_allgather_strategy* default_allgather = mpi_allgather_strategy_factory::get_optional_param(
    "allgather", "ring", params);
  strategy_->set_allgather(default_allgather);


  sprockit::sim_parameters* queue_params = params->get_optional_namespace("queue");
  /**
    sstkeyword {
        docstring=Whether MPI runs as an asynchronous progress thread [service] or
        blocks the application [user] thread.;
    }
  */
  queue_ = mpi_queue_factory::get_optional_param("type", "user", queue_params, id_);
  queue_->set_api(this);
}

void
mpi_api::finalize_init()
{
}

void
mpi_api::init_param1(const software_id& id)
{
  api::init_param1(id);
  process_manager::init_param1(id);
  id_ = id;
  rank_ = mpi_id(int(id.task_));
  libname_ = "mpiapi" + id.to_string();
}

void
mpi_api::init_os(operating_system* os)
{
  api::init_os(os);
  process_manager::init_os(os);
}

struct fi {
  float a;
  int b;
};

struct di {
  double a;
  int b;
};
struct ii {
  int a;
  int b;
};
struct si {
  short a;
  int b;
};
struct li {
  long a;
  int b;
};
struct ldi {
  long double a;
  int b;
};

struct complex {
  float r;
  float i;
};

struct dcomplex {
  double r;
  double i;
};

struct ldcomplex {
  long double r;
  long double i;
};


void
mpi_api::precommit_types()
{
  static const int builtin_sizes[] = {1, 2, 4, 6, 8, 12, 16, 20, 32, 48, 64};
  static const int num_builtins = sizeof(builtin_sizes) / sizeof(int);

  static thread_lock lock;
  lock.lock();
  if (!mpi_type::mpi_null->committed()){
    mpi_type::mpi_null->init_primitive("MPI_NULL", 0, 0, MPI_COMBINER_NAMED,
             new mpi_null_op);

    mpi_type::mpi_char->init_primitive("MPI_CHAR", 1, 4, MPI_COMBINER_NAMED,
             new mpi_prim_bit_op<char>);

    mpi_type::mpi_signed_char->init_primitive("MPI_SIGNED_CHAR", 1, 4,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<signed char>);

    mpi_type::mpi_wchar->init_primitive("MPI_WCHAR", 2, 4, MPI_COMBINER_NAMED,
                                   new mpi_prim_bit_op<char>);

    mpi_type::mpi_unsigned_long_long->init_primitive("MPI_UNSIGNED_LONG_LONG", 8, 8,
                                 MPI_COMBINER_NAMED,
                                 new mpi_prim_bit_op<unsigned long long>);

    mpi_type::mpi_lb->init_primitive("MPI_LB", 0, 0, MPI_COMBINER_NAMED,
                                new mpi_prim_bit_op<int>);

    mpi_type::mpi_ub->init_primitive("MPI_UB", 0, 0, MPI_COMBINER_NAMED,
                                new mpi_prim_bit_op<int>);

    mpi_type::mpi_unsigned_char->init_primitive("MPI_UNSIGNED_CHAR", 1, 4,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<unsigned char>);

    mpi_type::mpi_byte->init_primitive("MPI_BYTE", 1, 4, MPI_COMBINER_NAMED,
         new mpi_prim_bit_op<char>);

    mpi_type::mpi_short->init_primitive("MPI_SHORT", 2, 4, MPI_COMBINER_NAMED,
         new mpi_prim_bit_op<short>);

    mpi_type::mpi_unsigned_short->init_primitive("MPI_UNSIGNED_SHORT", 2, 4,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<unsigned short>);

    mpi_type::mpi_int->init_primitive("MPI_INT", 4, 4, MPI_COMBINER_NAMED,
        new mpi_prim_bit_op<int>);

    mpi_type::mpi_unsigned->init_primitive("MPI_UNSIGNED", 4, 4,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<unsigned>);

    mpi_type::mpi_long->init_primitive("MPI_LONG", 8, 8, MPI_COMBINER_NAMED,
        new mpi_prim_bit_op<long>);

    mpi_type::mpi_unsigned_long->init_primitive("MPI_UNSIGNED_LONG", 8, 8,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<unsigned long>);

    mpi_type::mpi_float->init_primitive("MPI_FLOAT", 4, sizeof(float),
        MPI_COMBINER_NAMED, new mpi_prim_op<float>);

    mpi_type::mpi_double->init_primitive("MPI_DOUBLE", 8, 8, MPI_COMBINER_NAMED,
        new mpi_prim_op<double>);

    mpi_type::mpi_long_double->init_primitive("MPI_LONG_DOUBLE", 16, 16,
        MPI_COMBINER_NAMED, new mpi_prim_op<long double>);

    mpi_type::mpi_long_long_int->init_primitive("MPI_LONG_LONG_INT", 8, 8,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<long long int>);

    mpi_type::mpi_long_long->init_primitive("MPI_LONG_LONG", 8, 8,
        MPI_COMBINER_NAMED, new mpi_prim_bit_op<long long>);

    mpi_type::mpi_packed->init_primitive("MPI_PACKED", 1, 0, MPI_COMBINER_NAMED,
       new mpi_prim_bit_op<char>);

    mpi_type::mpi_float_int->init_primitive("MPI_FLOAT_INT", mpi_type::mpi_float,
                                       mpi_type::mpi_int, sizeof(fi), MPI_COMBINER_NAMED,
                                       new mpi_pair_op<float, int>);

    mpi_type::mpi_double_int->init_primitive("MPI_DOUBLE_INT",
                                        mpi_type::mpi_double, mpi_type::mpi_int, sizeof(di), MPI_COMBINER_NAMED,
                                        new mpi_pair_op<double, int>);

    mpi_type::mpi_long_int->init_primitive("MPI_LONG_INT", mpi_type::mpi_long,
                                      mpi_type::mpi_int, sizeof(li), MPI_COMBINER_NAMED,
                                      new mpi_pair_op<long, int>);

    mpi_type::mpi_short_int->init_primitive("MPI_SHORT_INT", mpi_type::mpi_short,
                                       mpi_type::mpi_int, sizeof(si), MPI_COMBINER_NAMED,
                                       new mpi_pair_op<short, int>);

    mpi_type::mpi_2int->init_primitive("MPI_2INT", mpi_type::mpi_int,
                                  mpi_type::mpi_int, sizeof(ii), MPI_COMBINER_NAMED,
                                  new mpi_pair_op<int, int>);

    mpi_type::mpi_long_double_int->init_primitive("MPI_LONG_DOUBLE_INT",
        mpi_type::mpi_long_double, mpi_type::mpi_int, sizeof(ldi),
        MPI_COMBINER_NAMED, new mpi_pair_op<long double, int>);

    //fortran nonsense
    mpi_type::mpi_complex->init_primitive("MPI_COMPLEX", mpi_type::mpi_float,
                                     mpi_type::mpi_float, sizeof(complex), MPI_COMBINER_NAMED,
                                     new mpi_pair_op<float, float>);

    mpi_type::mpi_double_complex->init_primitive("MPI_DOUBLE_COMPLEX",
        mpi_type::mpi_double, mpi_type::mpi_double, sizeof(dcomplex),
        MPI_COMBINER_NAMED, new mpi_pair_op<double, double>);

    mpi_type::mpi_logical->init_primitive("MPI_LOGICAL", 1, 4, MPI_COMBINER_NAMED,
                                     new mpi_prim_bit_op<char>);

    mpi_type::mpi_real->init_primitive("MPI_REAL", 4, 4, MPI_COMBINER_NAMED,
                                  new mpi_prim_op<float>);

    mpi_type::mpi_double_precision->init_primitive("MPI_DOUBLE_PRECISION", 8, 8,
        MPI_COMBINER_NAMED, new mpi_prim_op<double>);

    mpi_type::mpi_integer->init_primitive("MPI_INTEGER", 4, 4, MPI_COMBINER_NAMED,
                                     new mpi_prim_bit_op<int>);

    mpi_type::mpi_integer1->init_primitive("MPI_INTEGER1", 1, 4,
                                      MPI_COMBINER_NAMED, new mpi_prim_bit_op<char>);

    mpi_type::mpi_integer2->init_primitive("MPI_INTEGER2", 2, 4,
                                      MPI_COMBINER_NAMED, new mpi_prim_bit_op<short>);

    mpi_type::mpi_integer4->init_primitive("MPI_INTEGER4", 4, 4,
                                      MPI_COMBINER_NAMED, new mpi_prim_bit_op<int>);

    mpi_type::mpi_integer8->init_primitive("MPI_INTEGER8", 8, 8,
                                      MPI_COMBINER_NAMED, new mpi_prim_bit_op<long>);

    mpi_type::mpi_real4->init_primitive("MPI_REAL4", 4, 4, MPI_COMBINER_NAMED,
                                   new mpi_prim_op<float>);

    mpi_type::mpi_real8->init_primitive("MPI_REAL8", 8, 8, MPI_COMBINER_NAMED,
                                   new mpi_prim_op<double>);

    mpi_type::mpi_real16->init_primitive("MPI_REAL16", 16, 16, MPI_COMBINER_NAMED,
                                    new mpi_prim_op<long double>);

    mpi_type::mpi_complex8->init_primitive("MPI_COMPLEX8", mpi_type::mpi_float,
                                      mpi_type::mpi_float, sizeof(complex), MPI_COMBINER_NAMED,
                                      new mpi_pair_op<float, float>);

    mpi_type::mpi_complex16->init_primitive("MPI_COMPLEX16", mpi_type::mpi_double,
                                       mpi_type::mpi_double, sizeof(dcomplex), MPI_COMBINER_NAMED,
                                       new mpi_pair_op<double, double>);

    mpi_type::mpi_complex32->init_primitive("MPI_COMPLEX32",
                                       mpi_type::mpi_long_double, mpi_type::mpi_long_double, sizeof(ldcomplex),
                                       MPI_COMBINER_NAMED, new mpi_pair_op<long double, long double>);

    //fortran pairs
    mpi_type::mpi_2integer->init_primitive("MPI_2INTEGER", mpi_type::mpi_int,
                                      mpi_type::mpi_int, sizeof(ii), MPI_COMBINER_NAMED,
                                      new mpi_pair_op<int, int>);

    mpi_type::mpi_2complex->init_primitive("MPI_2COMPLEX", mpi_type::mpi_complex,
                                      mpi_type::mpi_complex, 16, MPI_COMBINER_NAMED,
                                      new mpi_null_op);

    mpi_type::mpi_2double_complex->init_primitive("MPI_2DOUBLE_COMPLEX",
        mpi_type::mpi_double_complex, mpi_type::mpi_double_complex, 16,
        MPI_COMBINER_NAMED, new mpi_null_op);

    mpi_type::mpi_2real->init_primitive("MPI_2REAL", mpi_type::mpi_real,
                                   mpi_type::mpi_real, 8, MPI_COMBINER_NAMED,
                                   new mpi_pair_op<float, float>);

    mpi_type::mpi_2double_precision->init_primitive("MPI_2DOUBLE_PRECISION",
        mpi_type::mpi_double_precision, mpi_type::mpi_double_precision, 16,
        MPI_COMBINER_NAMED, new mpi_pair_op<double, double>);

    mpi_type::mpi_character->init_primitive("MPI_CHARACTER", 1, 4,
         MPI_COMBINER_NAMED, new mpi_prim_bit_op<char>);

    for (int i=0; i < num_builtins; ++i){
      int size = builtin_sizes[i];
      std::string label = sprockit::printf("Built-in type size %d", size);
      mpi_type::builtins[size].init_primitive(label,
        size, 0, MPI_COMBINER_NAMED, new mpi_null_op);
    }
  }

  precommit_type(mpi_type::mpi_null, MPI_NULL);

  precommit_type(mpi_type::mpi_char, MPI_CHAR);

  precommit_type(mpi_type::mpi_unsigned_char, MPI_UNSIGNED_CHAR);

  precommit_type(mpi_type::mpi_signed_char, MPI_SIGNED_CHAR);

  precommit_type(mpi_type::mpi_wchar, MPI_WCHAR);

  precommit_type(mpi_type::mpi_unsigned_long_long, MPI_UNSIGNED_LONG_LONG);

  precommit_type(mpi_type::mpi_lb, MPI_LB);

  precommit_type(mpi_type::mpi_ub, MPI_UB);

  precommit_type(mpi_type::mpi_byte, MPI_BYTE);

  precommit_type(mpi_type::mpi_double, MPI_DOUBLE);

  precommit_type(mpi_type::mpi_int, MPI_INT);

  precommit_type(mpi_type::mpi_unsigned, MPI_UNSIGNED);

  precommit_type(mpi_type::mpi_short, MPI_SHORT);

  precommit_type(mpi_type::mpi_unsigned_short, MPI_UNSIGNED_SHORT);

  precommit_type(mpi_type::mpi_long, MPI_LONG);

  precommit_type(mpi_type::mpi_long_long_int, MPI_LONG_LONG_INT);

  precommit_type(mpi_type::mpi_unsigned_long, MPI_UNSIGNED_LONG);

  precommit_type(mpi_type::mpi_float, MPI_FLOAT);

  precommit_type(mpi_type::mpi_double_int, MPI_DOUBLE_INT);

  precommit_type(mpi_type::mpi_2int, MPI_2INT);

  precommit_type(mpi_type::mpi_float_int, MPI_FLOAT_INT);

  precommit_type(mpi_type::mpi_long_int, MPI_LONG_INT);

  precommit_type(mpi_type::mpi_short_int, MPI_SHORT_INT);

  precommit_type(mpi_type::mpi_long_double, MPI_LONG_DOUBLE);

  precommit_type(mpi_type::mpi_long_double_int, MPI_LONG_DOUBLE_INT);

  precommit_type(mpi_type::mpi_packed, MPI_PACKED);

  //fortran nonsense
  precommit_type(mpi_type::mpi_complex, MPI_COMPLEX);

  precommit_type(mpi_type::mpi_double_complex, MPI_DOUBLE_COMPLEX);

  precommit_type(mpi_type::mpi_logical, MPI_LOGICAL);

  precommit_type(mpi_type::mpi_real, MPI_REAL);

  precommit_type(mpi_type::mpi_double_precision, MPI_DOUBLE_PRECISION);

  precommit_type(mpi_type::mpi_integer, MPI_INTEGER);

  precommit_type(mpi_type::mpi_integer1, MPI_INTEGER1);

  precommit_type(mpi_type::mpi_integer2, MPI_INTEGER2);

  precommit_type(mpi_type::mpi_integer4, MPI_INTEGER4);

  precommit_type(mpi_type::mpi_integer8, MPI_INTEGER8);

  precommit_type(mpi_type::mpi_real4, MPI_REAL4);

  precommit_type(mpi_type::mpi_real8, MPI_REAL8);

  precommit_type(mpi_type::mpi_real16, MPI_REAL16);

  precommit_type(mpi_type::mpi_complex8, MPI_COMPLEX8);

  precommit_type(mpi_type::mpi_complex16, MPI_COMPLEX16);

  precommit_type(mpi_type::mpi_complex32, MPI_COMPLEX32);

    //fortran pairs
  precommit_type(mpi_type::mpi_2integer, MPI_2INTEGER);
  //precommit_type(mpi_type::mpi_2complex, MPI_2COMPLEX);
  //precommit_type(mpi_type::mpi_2double_complex, MPI_2DOUBLE_COMPLEX);

  precommit_type(mpi_type::mpi_2real, MPI_2REAL);

  precommit_type(mpi_type::mpi_2double_precision, MPI_2DOUBLE_PRECISION);

  precommit_type(mpi_type::mpi_character, MPI_CHARACTER);

  for (int i=0; i < num_builtins; ++i){
    int size = builtin_sizes[i];
    allocate_type_id(&mpi_type::builtins[size]);
  }

  lock.unlock();
}

void
mpi_api::delete_statics()
{
}

mpi_api::~mpi_api()
{
#if SSTMAC_ENABLE_MPI_TIMELINE
  //this will only actually print if finalize not called
  if (status_ != is_finalized)
    queue_->print_log();
#endif
  //MUST DELETE HERE
  //cannot delete in finalize
  //this is weird with context switching
  //an unblock finishes finalize... so finalize is called while the DES thread is still inside the queue
  //the queue outlives mpi_api::finalize!
  delete queue_;
}

void
mpi_api::attr_put(mpi_comm* comm, keyval* k, void* val)
{
  comm->set_keyval(k, val);
}

void
mpi_api::attr_get(mpi_comm* comm, keyval* k, void* val, int* flag)
{
  comm->get_keyval(k, val, flag);
}

/*
 * Methods exposing MPI calls and other key methods to
 * derived objects.
 */

//
// Get count.
//
int
mpi_api::count(mpi_status* status, mpi_type_id type)
{
  assert_initialized();
  mpi_type* typeobj = type_from_id(type);
  int count = status->bytes_received() / typeobj->packed_size();
  return count;
}

/* Set up and tear down */

//
// Test whether MPI has bee initialized.
//
bool
mpi_api::initialized()
{
  return (status_ == is_initialized);
}

//
// Test whether MPI has been finalized.
//
bool
mpi_api::finalized()
{
  return (status_ == is_finalized);
}

//
// Initialize MPI.
//
timestamp
mpi_api::init()
{
  SSTMACBacktrace("MPI_Init");
  start_api_call();

  if (!os_) {
    spkt_throw(sprockit::null_error, "mpiapi::init: os has not been initialized yet");
  }

  app_manager* env = os_->env(id_.app_);
  if (!env) {
    spkt_throw(sprockit::null_error, "mpi_api::init: no environment found");
  }

  /** Make sure all the default types are known */
  precommit_types();

  std::string server_libname = sprockit::printf("mpiserver-%d", int(id_.app_));
  library* mpilib = os_->lib(server_libname);
  mpi_server* mpiserv;
  if (mpilib == 0) { // only do one mpiserver per app per node
    mpiserv = new mpi_server(int(id_.app_));
  } else {
    mpiserv = safe_cast(mpi_server, mpilib);
  }
  register_lib(mpiserv);

  if (mpilib == 0) //first guy
    mpiserv->start();

  queue_->set_mpi_server(mpiserv);

  queue_->set_event_parent(os_->parent());

  mpiserv->add_task(id_, queue_, rank_);

  //we are all done with mpiserv - clear for help track mem leaks
  mpiserv = 0;
  mpilib = 0;

  comm_factory_ = new mpi_comm_factory(id_.app_, this);
  comm_factory_->init(env, rank_);

  worldcomm_ = comm_factory_->world();
  selfcomm_ = comm_factory_->self();

  mpi_api_debug(sprockit::dbg::mpi, "MPI_Init()");

  add_comm_ptr(worldcomm_);

  add_comm_ptr(selfcomm_);

  status_ = is_initialized;

  if (worldcomm_->rank() == 0) {
    starttime_ = os_->now();
  }

  barrier(worldcomm_);

  end_api_call();

  return os_->now();

}

//
// Finalize MPI.
//
timestamp
mpi_api::finalize()
{  
  SSTMACBacktrace("MPI_Finalize");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Finalize()");
  start_api_call();
  assert_initialized();

  barrier(worldcomm_);

  status_ = is_finalized;

  if (worldcomm_->rank() == 0) {
    runtime_ = os_->now() - starttime_;
    debug_printf(sprockit::dbg::mpi_check,
      "MPI application with ID %s passed barrier in finalize on Rank 0\n"
      "at simulation time %10.6e seconds. This generally validates the \n"
      "simulation meaning everyhing has cleanly terminate\n",
      id_.to_string().c_str(),
      os_->now().sec());
  }
  comm_factory_->finalize();


#if SSTMAC_ENABLE_MPI_TIMELINE
  queue_->print_log();
#endif

  end_api_call();

  queue_->server()->unregister(id_, queue_);
  queue_->unregister_all_libs();
  os_->unregister_all_libs(this);


  delete comm_factory_;
  delete strategy_;

  return os_->now();
}

//
// Get current time.
//
timestamp
mpi_api::wtime()
{
  return os_->now();
}

/* Create and destroy communicators. */

timestamp
mpi_api::group_incl(int *ranks, int num_ranks,
                    mpi_group* oldgrp, mpi_group*& newgrp)
{
  if (num_ranks > oldgrp->size()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "MPI_Group_incl: invalid group size %d", num_ranks);
  }

  std::vector<task_id> vec_ranks(num_ranks, task_id(0));
  for (int i = 0; i < num_ranks; i++) {
    vec_ranks[i] = oldgrp->at(ranks[i]);
  }
  newgrp = new mpi_group(vec_ranks);
  return os_->now();
}

//
// Duplicate a communicator.
//
timestamp
mpi_api::comm_dup(mpi_comm* input, mpi_comm* &output)
{
  SSTMACBacktrace("MPI_Comm_dup");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_dup(...)");
  start_api_call();
  assert_initialized();
  output = comm_factory_->comm_dup(input);
  add_comm_ptr(output);
  end_api_call();
  return os_->now();
}

//
// Create a communicator containing a subset of an existing comm.
//
timestamp
mpi_api::comm_create(mpi_comm* input, mpi_group* group,
                     mpi_comm* &output)
{
  SSTMACBacktrace("MPI_Comm_create");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_create(...)");
  start_api_call();
  assert_initialized();
  // validranks(members, input);
  output = comm_factory_->comm_create(input, group);
  add_comm_ptr(output);
  end_api_call();
  return os_->now();
}

//
// Split a communicator.  This one is a little weird.
//
timestamp
mpi_api::comm_split(mpi_comm* incomm, int color, int key,
                    mpi_comm* &outcomm)
{
  SSTMACBacktrace("MPI_Comm_split");
  mpi_api_debug(sprockit::dbg::mpi,
      "MPI_Comm_split(%s,%d,%d) enter",
       comm_str(incomm).c_str(), color, key);
  start_api_call();
  assert_initialized();
  outcomm = comm_factory_->comm_split(incomm, color, key);
  add_comm_ptr(outcomm);
  mpi_api_debug(sprockit::dbg::mpi,
     "MPI_Comm_split(%s,%d,%d) exit-> %s, rank %d",
      comm_str(incomm).c_str(), color, key,
      comm_str(outcomm).c_str(), int(outcomm->rank()));
  end_api_call();
  return os_->now();
}

//
// Destroy a communicator.
//
timestamp
mpi_api::comm_free(mpi_comm* comm)
{
  SSTMACBacktrace("MPI_Comm_free");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Comm_free(...)");
  start_api_call();
  assert_initialized();
#if !SSTMAC_ENABLE_MPI_TIMELINE
  erase_comm_ptr(comm->id());
  delete comm;
#endif
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cart_create(mpi_comm* &comm, int ndims, int *dims,
                         int *periods, int reorder, mpi_comm* &outcomm)
{
  SSTMACBacktrace("MPI_Cart_create");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_create(...)");
  start_api_call();
  assert_initialized();
  outcomm = comm_factory_->create_cart(comm, ndims, dims, periods, reorder);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cart_get(mpi_comm* &comm, int maxdims, int *dims,
                      int *periods, int *coords)
{
  SSTMACBacktrace("MPI_Cart_get");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_get(...)");
  start_api_call();

  mpi_comm_cart* c = safe_cast(mpi_comm_cart, comm,
    "mpiapi::mpi_cart_get: mpi comm did not cast to mpicomm_cart");

  for (int i = 0; i < maxdims; i++) {
    dims[i] = c->get_dim(i);
    periods[i] = c->get_period(i);
  }

  c->set_coords(c->rank(), coords);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cartdim_get(mpi_comm* &comm, int *ndims)
{
  SSTMACBacktrace("MPI_Cartdim_get");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cartdim_get(...)");
  start_api_call();
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, comm,
    "mpiapi::mpi_cart_get: mpi comm did not cast to mpicomm_cart");
  *ndims = c->get_ndims();
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cart_rank(mpi_comm* comm, int *coords, int *rank)
{
  SSTMACBacktrace("MPI_Cart_rank");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_rank(...)");
  start_api_call();
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, comm,
    "mpiapi::mpi_cart_get: mpi comm did not cast to mpicomm_cart");
  *rank = c->get_rank(coords);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cart_shift(mpi_comm* comm, int direction, int displ,
                        int *source, int *dest)
{
  SSTMACBacktrace("MPI_Cart_shift");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_shift(...)");
  start_api_call();
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, comm,
    "mpiapi::mpi_cart_get: mpi comm did not cast to mpicomm_cart");
  *source = c->shift(direction, -1 * displ);
  *dest = c->shift(direction, displ);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::mpi_cart_coords(mpi_comm* comm, int rank, int maxdims,
                         int *coords)
{
  SSTMACBacktrace("MPI_Cart_coords");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cart_coords(...)");
  start_api_call();
  mpi_comm_cart* c = safe_cast(mpi_comm_cart, comm,
    "mpiapi::mpi_cart_get: mpi comm did not cast to mpicomm_cart");
  c->set_coords(rank, coords);
  end_api_call();
  return os_->now();
}

/* Basic point-to-point operations. */

//
// Blocking send.
//
timestamp
mpi_api::send(int count, mpi_type_id type, mpi_id target,
              mpi_tag tag, mpi_comm* comm,
              const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Send");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Send(%d,%s,%d:%d,%s,%s)",
    count, type_str(type).c_str(), int(target), int(comm->peer_task(target)),
    tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();

  assert_initialized();
  validsend(count, target, tag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  this->do_isend(count, type, target, tag, comm, req, content);

  this->queue_->progress_loop(req);

  delete req;

  end_api_call();
  return os_->now();
}

//
// Blocking send using a user-defined buffer (see buffer_attach).
//
timestamp
mpi_api::bsend(int count, mpi_type_id type, mpi_id target,
               mpi_tag tag, mpi_comm* comm,
               const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Bsend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt, "MPI_Bsend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, target, tag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  pending_bsends_.push_back(req);
  this->do_ibsend(count, type, target, tag, comm, req, content);
  //timestamp finish = queue_->progress_loop(req);

  end_api_call();
  return os_->now();
}

//
// Blocking syncronous send.
//
timestamp
mpi_api::ssend(int count, mpi_type_id type, mpi_id target,
               mpi_tag tag, mpi_comm* comm,
               const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Ssend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Ssend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, target, tag, comm);
  mpi_request* req = mpi_request::construct(default_key_category);
  this->do_issend(count, type, target, tag, comm, req, content);
  timestamp finish = queue_->progress_loop(req);
  end_api_call();
  return finish;
}

//
// Ready send (as in "ready or not comes the data").
timestamp
mpi_api::rsend(int count, mpi_type_id type, mpi_id target,
               mpi_tag tag, mpi_comm* comm,
               const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Rsend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Rsend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  validsend(count, target, tag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  timestamp entry = os_->now();
  this->do_irsend(count, type, target, tag, comm, req, content);
  timestamp finish = queue_->progress_loop(req);

  return finish;
}

//
// Nonblocking send.
//
timestamp
mpi_api::isend(int count, mpi_type_id type, mpi_id dest,
               mpi_tag tag, mpi_comm* comm, mpi_request* &req,
               const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Isend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Isend(%d,%s,%d:%d,%s,%s)",
    count, type_str(type).c_str(), int(dest), int(comm->peer_task(dest)),
    tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, dest, tag, comm);
  timestamp entry = os_->now();

  req = mpi_request::construct(default_key_category);
  mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request=%p", req);

  this->do_isend(count, type, dest, tag, comm, req, content);
  timestamp finish = os_->now();
  end_api_call();
  return finish;
}

/// Nonblocking send.
/// wait for this request has the same completion as send.
/// \return time when the request has been registered (likely current time).
timestamp
mpi_api::do_isend(int count, mpi_type_id type, mpi_id dest,
                  mpi_tag tag, mpi_comm* comm, mpi_request* req,
                  const payload::const_ptr& content)
{
  strategy_->send()->execute(req, queue_, count, type, dest, tag, comm,
                             content, os_);

  return os_->now();
}

//
// Nonblocking user-buffered send.
//
timestamp
mpi_api::ibsend(int count, mpi_type_id type, mpi_id dest,
                mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Ibsend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Ibsend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(dest), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, dest, tag, comm);
  timestamp entry = os_->now();
  req = mpi_request::construct(default_key_category);
  this->do_ibsend(count, type, dest, tag, comm, req, content);
  timestamp finish = os_->now();
  end_api_call();
  return finish;
}

//
// Nonblocking user-buffered send.
//
timestamp
mpi_api::do_ibsend(int count, mpi_type_id type, mpi_id dest,
                   mpi_tag tag, mpi_comm* comm, mpi_request* req,
                   const payload::const_ptr& content)
{
  return this->do_isend(count, type, dest, tag, comm, req, content);
}

//
// Nonblocking synchronous send.
//
timestamp
mpi_api::issend(int count, mpi_type_id type, mpi_id dest,
                mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Issend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Issend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(dest), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, dest, tag, comm);
  timestamp entry = os_->now();
  req = mpi_request::construct(default_key_category);
  this->do_issend(count, type, dest, tag, comm, req, content);
  timestamp finish = os_->now();
  end_api_call();
  return finish;
}

//
// Nonblocking synchronous send.
//
timestamp
mpi_api::do_issend(int count, mpi_type_id type, mpi_id dest,
                   mpi_tag tag, mpi_comm* comm, mpi_request* req,
                   const payload::const_ptr& content)
{
  strategy_->ssend()->execute(req, queue_, count, type, dest, tag, comm,
                              content, os_);

  return os_->now();
}

//
// Nonblocking ready send.
//
timestamp
mpi_api::irsend(int count, mpi_type_id type, mpi_id dest,
                mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Irsend");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Irsend(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(dest), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validsend(count, dest, tag, comm);
  req = mpi_request::construct(default_key_category);
  this->do_irsend(count, type, dest, tag, comm, req, content);
  timestamp finish = os_->now();
  end_api_call();
  return finish;
}

//
// Nonblocking ready send.
//
timestamp
mpi_api::do_irsend(int count, mpi_type_id type, mpi_id dest,
                   mpi_tag tag, mpi_comm* comm, mpi_request* req,
                   const payload::const_ptr& content)
{
  strategy_->rsend()->execute(req, queue_, count, type, dest, tag, comm,
                              content, os_);

  return os_->now();
}

//
// Synchronous receive.
//
timestamp
mpi_api::recv(int count, mpi_type_id type, mpi_id source,
              mpi_tag tag, mpi_comm* comm, mpi_status* stat)
{
  SSTMACBacktrace("MPI_Recv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Recv(%d,%s,%s,%s,%s)",
    count, type_str(type).c_str(), src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validrecv(count, source, tag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);

  this->do_irecv(count, type, source, tag, comm, req);
  timestamp finish = queue_->progress_loop(req);

  if (stat != mpi_status::ignore){
    (*stat) = req->status();
  }

  delete req;

  end_api_call();
  return finish;
}

//
// Nonblocking receive
//
timestamp
mpi_api::irecv(int count, mpi_type_id type, mpi_id source,
               mpi_tag tag, mpi_comm* comm, mpi_request* &req)
{
  SSTMACBacktrace("MPI_Irecv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request | sprockit::dbg::mpi_pt2pt,
    "MPI_Irecv(%d,%s,%s,%s,%s)",
    count, type_str(type).c_str(),
    src_str(comm, source).c_str(), tag_str(tag).c_str(),
    comm_str(comm).c_str());
  start_api_call();
  assert_initialized();
  validrecv(count, source, tag, comm);

  req = mpi_request::construct(default_key_category);
  mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request=%p\n", req);

  this->do_irecv(count, type, source, tag, comm, req);
  timestamp finish = os_->now();
  end_api_call();
  return finish;
}

//
// Asynchronous receive.
//
timestamp
mpi_api::do_irecv(int count, mpi_type_id type, mpi_id source,
                  mpi_tag tag, mpi_comm* comm, mpi_request* req)
{
  assert_initialized();
  validrecv(count, source, tag, comm);
  strategy_->recv()->execute(req, queue_, count, type, source, tag, comm,
                             os_);
  return os_->now();
}

//
// Send/receieve exchange.
//
timestamp
mpi_api::sendrecv(int sendcount, mpi_type_id sendtype,
                  mpi_id target, mpi_tag sendtag, int recvcount,
                  mpi_type_id recvtype, mpi_id source, mpi_tag recvtag,
                  mpi_comm* comm, mpi_status* stat,
                  const payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Sendrecv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Sendrecv(%d,%s,%d,%s,%d,%s,%s,%s,%s)",
    sendcount, type_str(sendtype).c_str(), int(target), tag_str(sendtag).c_str(),
    recvcount, type_str(recvtype).c_str(),
    src_str(source).c_str(), tag_str(recvtag).c_str(),
    comm_str(comm).c_str());

  start_api_call();
  assert_initialized();

  validsend(sendcount, target, sendtag, comm);
  validrecv(recvcount, source, recvtag, comm);

  mpi_request* req1 = mpi_request::construct(default_key_category);
  mpi_request* req2 = mpi_request::construct(default_key_category);
  this->do_isend(sendcount, sendtype, target, sendtag, comm, req1, content);
  this->do_irecv(recvcount, recvtype, source, recvtag, comm, req2);
  std::vector<mpi_request*> reqs(2,NULL);
  reqs[0] = req1;
  reqs[1] = req2;

  wait(&req1);
  if (stat == mpi_status::ignore){
    wait(&req2);
  } else {
    wait(&req2, stat);
  }

  end_api_call();
  return os_->now();
}

//
// Send and receive using a single buffer.
//
timestamp
mpi_api::sendrecv_replace(int count, mpi_type_id sendtype,
                          mpi_id dest, mpi_tag sendtag, mpi_id source,
                          mpi_tag recvtag, mpi_comm* comm,
                          mpi_status* stat)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Sendrecv_replace(%d,%s,%d,%s,%s,%s,%s)\n",
    count, type_str(sendtype).c_str(), int(dest), tag_str(sendtag).c_str(),
    src_str(source).c_str(), tag_str(recvtag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  validsend(count, dest, sendtag, comm);
  validrecv(count, source, recvtag, comm);

  mpi_request* req1 = mpi_request::construct(default_key_category);
  mpi_request* req2 = mpi_request::construct(default_key_category);


  payload::const_ptr cont;
  this->do_isend(count, sendtype, dest, sendtag, comm, req1, cont);
  this->do_irecv(count, sendtype, source, recvtag, comm, req2);
  std::vector<mpi_request*> reqs(2, NULL);
  reqs[0] = req1;
  reqs[1] = req2;
  waitall(reqs);
  *stat = req2->status();
  timestamp fini = os_->now();
  return fini;
}

/* Stepwise point-to-point (not implemented, but
 * should be fairly straight-forward to complete). */

//
// Initiate a send.
//
timestamp
mpi_api::send_init(int count, mpi_type_id type, mpi_id target,
                   mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                   void* buf)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Send_init(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  req = new persistent_send(this, &mpi_api::do_isend,
                     count, type, target, tag, comm, buf);
  return this->wtime();
}

//
// Initiate a buffered send.
//
timestamp
mpi_api::bsend_init(int count, mpi_type_id type, mpi_id target,
                    mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                    void* buf)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Bsend_init(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  validsend(count, target, tag, comm);
  req = new persistent_send(this, &mpi_api::do_ibsend,
                     count, type, target, tag, comm, buf);
  return this->wtime();
}

//
// Initiate a synchronous send.
//
timestamp
mpi_api::ssend_init(int count, mpi_type_id type, mpi_id target,
                    mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                    void* buf)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Ssend_init(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(),
    int(target), tag_str(tag).c_str(),
    comm_str(comm).c_str());
  assert_initialized();
  validsend(count, target, tag, comm);
  req = new persistent_send(this, &mpi_api::do_issend,
                  count, type, target, tag, comm, buf);
  return this->wtime();
}

//
// Initiate a ready send.
//
timestamp
mpi_api::rsend_init(int count, mpi_type_id type, mpi_id target,
                    mpi_tag tag, mpi_comm* comm, mpi_request* &req,
                    void* buf)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Rsend_init(%d,%s,%d,%s,%s)",
    count, type_str(type).c_str(), int(target), tag_str(tag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  validsend(count, target, tag, comm);
  req = new persistent_send(this, &mpi_api::do_irsend,
                    count, type, target, tag, comm, buf);
  return this->wtime();
}

//
// Initiate a receive.
//
timestamp
mpi_api::recv_init(int count, mpi_type_id type, mpi_id target,
                   mpi_tag tag, mpi_comm* comm, mpi_request* &req)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_pt2pt,
    "MPI_Recv_init(%d,%s,%s,%s,%s)",
    count, type_str(type).c_str(), src_str(target).c_str(),
    tag_str(tag).c_str(), comm_str(comm).c_str());
  assert_initialized();
  validrecv(count, target, tag, comm);
  req = new persistent_recv(this, count, type, target, tag, comm);
  return this->wtime();
}

//
// Start a previously initiated request.
//
timestamp
mpi_api::start(mpi_request* req)
{
  SSTMACBacktrace("MPI_Start");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Start(...)");
  start_api_call();
  persistent* pers = safe_cast(persistent, req,
    "mpiapi::start: incorrect request handle type");
  pers->start();
  end_api_call();
  return this->wtime();
}

//
// Start a collection of previously initiated requests.
//
timestamp
mpi_api::startall(std::vector<mpi_request*> &requests)
{
  SSTMACBacktrace("MPI_Startall");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Startall(...)");
  start_api_call();
  typedef std::vector<mpi_request*>::iterator iterator_t;
  for (iterator_t it = requests.begin(); it != requests.end(); ++it) {
    this->start(*it);
  }
  end_api_call();
  return this->wtime();
}

//
// Cancel a request that had been previously initiated.
//
timestamp
mpi_api::cancel(mpi_request* req)
{
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Cancel(...)");
  SSTMACBacktrace("MPI_Cancel");
  start_api_call();
  assert_initialized();
  if (!req->is_complete()) {
    req->cancel();
  }
  end_api_call();
  return this->wtime();
}

void
mpi_api::free_request(mpi_request** reqptr)
{
  mpi_request* req = *reqptr;
  if (req && !req->is_persistent()){
    *reqptr = 0;
    delete req;
  }
}

void
mpi_api::free_requests(std::vector<mpi_request*>& reqs)
{
  for (int i=0; i < reqs.size(); ++i){
    free_request(&reqs[i]);
  }
}

void
mpi_api::free_requests(std::vector<mpi_request*>& reqs,
  const std::vector<int>& inds)
{
  for (int i=0; i < inds.size(); ++i){
    int idx = inds[i];
    free_request(&reqs[idx]);
  }
}

void
mpi_api::free_request(std::vector<mpi_request*>& reqs, int index)
{
  if (index != MPI_UNDEFINED){
    delete reqs[index];
    reqs[index] = 0;
  }
}

void
mpi_api::build_statuses(std::vector<mpi_request*>& reqs, std::vector<mpi_status>& stats)
{
  stats.resize(reqs.size());
  for (int i=0; i < reqs.size(); ++i){
    stats[i] = reqs[i]->status();
  }
}

void
mpi_api::build_statuses(std::vector<mpi_request*>& reqs,
    const std::vector<int>& inds,
    std::vector<mpi_status>& stats)
{
  stats.resize(inds.size());
  for (int i=0; i < inds.size(); ++i){
    stats[i] = reqs[inds[i]]->status();
  }
}

//
// Wait for a request to finish.
//
timestamp
mpi_api::do_wait(mpi_request* req)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Wait(...)");
  mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request=%p", req);

  SSTMACBacktrace("MPI_Wait");
  start_api_call();
  assert_initialized();

  if (req == NULL || req->is_cancelled()) {
    // Null request is instantly complete
  }
  else if (req->is_persistent()) {
    persistent* pers = safe_cast(persistent, req);
    if (pers->started_){
      queue_->progress_loop(req);
    }
  }
  else {
    queue_->progress_loop(req);
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::wait(mpi_request** reqptr)
{
  mpi_request* req = *reqptr;
  do_wait(req);
  free_request(reqptr);
  return os_->now();
}

timestamp
mpi_api::wait(mpi_request** reqptr, mpi_status *stat)
{
  mpi_request* req = *reqptr;
  do_wait(req);
  *stat = (req)->status();
  free_request(reqptr);
  return os_->now();
}

timestamp
mpi_api::do_waitall(std::vector<mpi_request*>& reqs)
{
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Waitall(...)");

  for (int i=0; i < reqs.size(); ++i){

  }

  SSTMACBacktrace("MPI_Waitall");
  start_api_call();
  assert_initialized();

  int numreqs = reqs.size();
  for (int i=0; i < numreqs; ++i) {
    mpi_request* req = reqs[i];
    if (req == 0){
      mpi_api_debug(sprockit::dbg::mpi_request,
        "Null MPI_Request[%d]=%p", i, req);
      continue;
    }

    if (!req->is_complete()) {
      mpi_api_debug(sprockit::dbg::mpi_request,
        "Blocking on MPI_Request[%d]=%p", i, req);
      queue_->progress_loop(req);
    }
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::waitall(std::vector<mpi_request*>& reqs)
{
  do_waitall(reqs);
  free_requests(reqs);
  return os_->now();
}

timestamp
mpi_api::waitall(std::vector<mpi_request*>& reqs, std::vector<mpi_status>& stats)
{
  do_waitall(reqs);
  build_statuses(reqs, stats);
  free_requests(reqs);
  return os_->now();
}

timestamp
mpi_api::do_waitany(
  std::vector<mpi_request*>& reqs,
  int &index,
  timestamp timeout)
{
  SSTMACBacktrace("MPI_Waitany");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Waitany(...)");
  for (int i=0; i < reqs.size(); ++i){
    mpi_api_debug(sprockit::dbg::mpi_request,
        "MPI_Request[%d]=%p", i, reqs[i]);
  }
  start_api_call();
  assert_initialized();

  index = -1;

  if (reqs.empty()){
    end_api_call();
    return os_->now();
  }

  for (uint i = 0; i < reqs.size(); i++) {
    mpi_request* req = reqs[i];
    if (req && req->is_complete()) {
      index = i;
      end_api_call();
      return os_->now();
    }
  }

  queue_->start_progress_loop(reqs, timeout);

  for (uint i = 0; i < reqs.size(); i++) {
    mpi_request* req = reqs[i];
    if (req && req->is_complete()) {
      //this is the one
      index = i;
      mpi_api_debug(sprockit::dbg::mpi_request,
        "Completed MPI_Request[%d]=%p",
        index, req);
      break;
    }
  }

  queue_->finish_progress_loop(reqs);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::waitany(std::vector<mpi_request*>& reqs, int &index)
{
  return waitany(reqs, index, timestamp(0));
}

timestamp
mpi_api::waitany(std::vector<mpi_request*>& reqs,
  int &index, timestamp timeout)
{
  do_waitany(reqs, index, timeout);
  if (index >= 0){
    free_request(reqs, index);
  }
  return os_->now();
}

timestamp
mpi_api::waitany(std::vector<mpi_request*>& reqs,
 int &index,
 mpi_status* stat)
{
  return waitany(reqs, index, stat, timestamp(0));
}

timestamp
mpi_api::waitany(std::vector<mpi_request*>& reqs,
 int &index,
 mpi_status* stat,
 timestamp timeout)
{
  do_waitany(reqs, index, timeout);
  if (index >= 0){
    *stat = reqs[index]->status();
    free_request(reqs, index);
  }
  return os_->now();
}

timestamp
mpi_api::do_waitsome(
  std::vector<mpi_request*>& reqs,
  std::vector<int> &indices)
{
  SSTMACBacktrace("MPI_Waitsome");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request,
    "MPI_Waitsome(...)");

  for (int i=0; i < reqs.size(); ++i){
    mpi_api_debug(sprockit::dbg::mpi_request,
        "MPI_Request[%d]=%p",
        i, reqs[i]);
  }

  start_api_call();
  assert_initialized();

  indices.clear();
  if (reqs.empty()){
    end_api_call();
    return os_->now();
  }

  for (uint i = 0; i < reqs.size(); i++) {
    mpi_request* req = reqs[i];
    if (req != 0 && req->is_complete()) {
      indices.push_back(i);
    }
  }

  /** If no requests are done, we have to block until somebody finishes */
  if (indices.size() == 0) {
    queue_->start_progress_loop(reqs);
    for (uint i = 0; i < reqs.size(); i++) {
      mpi_request* req = reqs[i];
      if (req != 0 && req->is_complete()) {
        indices.push_back(i);
        mpi_api_debug(sprockit::dbg::mpi_request,
          "MPI_Request[%d]=%p complete",
          i, req);
      }
    }
  }

  //cleanup
  queue_->finish_progress_loop(reqs);

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::waitsome(std::vector<mpi_request*>& reqs,
    std::vector<int>& indices)
{
  do_waitsome(reqs, indices);
  free_requests(reqs, indices);
  return os_->now();
}

timestamp
mpi_api::waitsome(std::vector<mpi_request*>& reqs,
  std::vector<int>& indices,
  std::vector<mpi_status>& stats)
{
  do_waitsome(reqs, indices);
  build_statuses(reqs, indices, stats);
  free_requests(reqs, indices);
  return os_->now();
}

/* Testing of outstanding requests */
timestamp
mpi_api::do_test(mpi_request* req, bool& flag)
{
  SSTMACBacktrace("MPI_Test");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Test(...)");
  mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request=%p", req);
  start_api_call();
  assert_initialized();

  if (req == 0) {
    flag = true;
  }
  else if (req->is_persistent()) {
    flag = true;
  }
  else {
    flag = req->is_complete();
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::test(mpi_request** reqptr, bool& flag)
{
  mpi_request* req = *reqptr;
  do_test(req, flag);
  if (flag){
    delete req;
    *reqptr = 0;
  }
  return os_->now();
}

timestamp
mpi_api::test(mpi_request** reqptr, bool& flag, mpi_status* stat)
{
  mpi_request* req = *reqptr;
  do_test(req, flag);
  if (flag){
    *stat = req->status();
    delete req;
    *reqptr = 0;
  }
  return os_->now();
}

timestamp
mpi_api::do_testall(std::vector<mpi_request*> &reqs, bool &flag)
{
  SSTMACBacktrace("MPI_Testall");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Testall(...)");

  for (int i=0; i < reqs.size(); ++i){
    mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request[%d]=%p",
        i, reqs[i]);
  }

  start_api_call();
  assert_initialized();

  if (reqs.empty()){
    flag = false;
    end_api_call();
    return os_->now();
  }

  flag = true;
  for (size_t id = 0; id < reqs.size(); ++id) {
    mpi_request* req = reqs[id];
    if (req != NULL && !req->is_complete()) {
        flag = false;
        end_api_call();
        return os_->now();
    }
    mpi_api_debug(sprockit::dbg::mpi_request, "All MPI_Requests complete");
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::testall(std::vector<mpi_request*>& reqs, bool &flag)
{
  do_testall(reqs, flag);
  if (flag){
    free_requests(reqs);
  }
  return os_->now();
}

timestamp
mpi_api::testall(std::vector<mpi_request*>& reqs, bool &flag, std::vector<mpi_status>& stats)
{
  do_testall(reqs, flag);
  if (flag){
    build_statuses(reqs, stats);
    free_requests(reqs);
  }
  return os_->now();
}


timestamp
mpi_api::do_testany(std::vector<mpi_request*>& req, int &index, bool &flag)
{
  SSTMACBacktrace("MPI_Testany");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Testany(...)");

  for (int i=0; i < req.size(); ++i){
    mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request[%d]=%p", i, req[i]);
  }

  start_api_call();
  assert_initialized();

  index = -1;
  flag = false;

  if (req.empty()){
    end_api_call();
    return os_->now();
  }


  flag = false;
  for (size_t id = 0; id < req.size(); ++id) {
    if (req[id] != NULL && req[id]->is_complete()) {
      mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request[%d]=%p complete", id, req[id]);
      index = id;
      flag = true;
        break;
    }
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::testany(std::vector<mpi_request*>& reqs, int &index, bool &flag)
{
  do_testany(reqs, index, flag);
  if (flag){
    free_request(reqs, index);
  }
  return os_->now();
}

timestamp
mpi_api::testany(std::vector<mpi_request*>& reqs, int &index, bool &flag, mpi_status *stat)
{
  do_testany(reqs, index, flag);
  if (flag){
    *stat = reqs[index]->status();
    free_request(reqs, index);
  }
  return os_->now();
}

//
// Test whether some of the given requests have completed.
//
timestamp
mpi_api::do_testsome(std::vector<mpi_request*>& reqs, std::vector<int> &indices)
{
  SSTMACBacktrace("MPI_Testsome");

  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_request, "MPI_Testsome(...)");

  for (int i=0; i < reqs.size(); ++i){
    mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request[%d]=%p", i, reqs[i]);
  }
  start_api_call();
  assert_initialized();

  indices.clear();

  if (reqs.empty()){
    end_api_call();
    return os_->now();
  }

  for (size_t id = 0; id < reqs.size(); ++id) {
    mpi_request* req = reqs[id];
    if (req != NULL && req->is_complete()) {
      mpi_api_debug(sprockit::dbg::mpi_request, "MPI_Request[%d]=%p complete", id, req);
      indices.push_back(id);
    }
  }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::testsome(std::vector<mpi_request*>& reqs, std::vector<int>& indices)
{
  do_testsome(reqs, indices);
  free_requests(reqs, indices);
  return os_->now();
}

timestamp
mpi_api::testsome(std::vector<mpi_request*>& reqs, std::vector<int>& indices, std::vector<mpi_status>& stats)
{
  do_testsome(reqs, indices);
  build_statuses(reqs, indices, stats);
  free_requests(reqs, indices);
  return os_->now();
}

//
// Blocking probe for a message that matches the given signature.
//
timestamp
mpi_api::probe(mpi_id source, mpi_tag tag,
               mpi_comm* comm, mpi_status* stat)
{
  SSTMACBacktrace("MPI_Probe");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Probe(%s,%s,%s)",
    src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();

  mpi_request* req = mpi_request::construct(default_key_category);
  event_handler* done = queue_->progress_done_handler(os_, req);
  if (source == mpi::proc_null) {
    stat->set_count(0);
    stat->set_source(source);
    stat->set_tag(mpi::any_tag);
  }
  else {
    queue_->probe(req, comm, source, tag, done);
    queue_->progress_loop(req);
    (*stat) = req->status();
    //this can return a negative count - I don't know how
    stat->set_count(labs(stat->count()));
  }

  delete req;
  if (done) delete done;

  end_api_call();
  return os_->now();
}

//
// Non-blocking probe for messages.
//
timestamp
mpi_api::iprobe(mpi_id source, mpi_tag tag,
                mpi_comm* comm, bool &flag, mpi_status* stat)
{
  SSTMACBacktrace("MPI_Iprobe");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Iprobe(%s,%s,%s)",
    src_str(source).c_str(), tag_str(tag).c_str(), comm_str(comm).c_str());
  start_api_call();
  assert_initialized();

  if (source == mpi::proc_null) {
    flag = true;
    stat->set_count(0);
    stat->set_source(source);
    stat->set_tag(mpi::any_tag);
  }
  else {
    flag = queue_->iprobe(comm, source, tag, stat);
    if (flag) {
      stat->set_count(labs(stat->count()));
    }
    else {
      flag = false;
    }
  }

  timestamp fini = os_->now();

  end_api_call();
  return fini;
}

/* Mucking with MPI state. */

//
// Attach a user-defined buffer for *bsend operations.
//
timestamp
mpi_api::buffer_attach(int bytes)
{
  assert_initialized();
  return os_->now();
}

//
// Detach the user-defined buffer.
//
timestamp
mpi_api::buffer_detach(int &bytes)
{
  SSTMACBacktrace("MPI_Buffer_detach");
  start_api_call();
  assert_initialized();
  waitall(pending_bsends_);
  pending_bsends_.clear();
  end_api_call();
  return os_->now();
}

/* Collective operations */
//
// Set a barrier.
//
timestamp
mpi_api::barrier(mpi_comm* comm)
{
  SSTMACBacktrace("MPI_Barrier");
  start_api_call();
  assert_initialized();
  mpi_tag barriertag = comm->next_collective_tag();
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Barrier(%s) on tag %d",
    comm_str(comm).c_str(), int(barriertag));

  mpi_request* req = mpi_request::construct(default_key_category);

  mpi_collective* coll = strategy_->barrier()->execute(req, queue_, barriertag, comm, os_);

  timestamp fini = queue_->progress_loop(req);

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Broadcast data without payload
//
timestamp
mpi_api::bcast(int count, mpi_type_id type, mpi_id root, mpi_comm* comm)
{
  payload::const_ptr content = payload::null();
  return this->bcast(count, type, root, comm, content);
}

//
// Broadcast data with payload.
//
timestamp
mpi_api::bcast(int count, mpi_type_id type, mpi_id root,
               mpi_comm* comm, payload::const_ptr& content)
{
  SSTMACBacktrace("MPI_Bcast");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Bcast(%d,%s,%d,%s)",
    count, type_str(type).c_str(), int(root), comm_str(comm).c_str());

  start_api_call();

  mpi_tag bcasttag = comm->next_collective_tag();
  assert_initialized();
  validbcast(count, type, root, bcasttag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->bcast()->execute(
    req, queue_, count, type, root, bcasttag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);
  content = req->status().content();

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Scatter data uniformly.
//
timestamp
mpi_api::scatter(int sendcount, mpi_type_id sendtype, int recvcount,
                 mpi_type_id recvtype, mpi_id root, mpi_comm* comm)
{
  std::vector<payload::const_ptr> vals;

  payload::const_ptr result;
  timestamp fini = this->scatter(sendcount, sendtype, recvcount, recvtype,
                                 root, comm, vals, result, false);
  return fini;
}

//
// Scatter data with payload.
//
timestamp
mpi_api::scatter(int sendcount, mpi_type_id sendtype, int recvcount,
                 mpi_type_id recvtype, mpi_id root, mpi_comm* comm,
                 const std::vector<payload::const_ptr>& cc,
                 payload::const_ptr& result,
                 bool usingpayload)
{
  SSTMACBacktrace("MPI_Scatter");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Scatter(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  start_api_call();
  mpi_tag scattertag = comm->next_collective_tag();
  assert_initialized();

  if (comm->rank() == root) {
    if (usingpayload && int(comm->size()) != (int) cc.size()) {
      spkt_throw_printf(sprockit::value_error,
                       "mpiapi: scatter: content size %lu does not match comm size %d",
                       cc.size(), int(comm->size()));
    }
    validsend(sendcount, root, scattertag, comm);
  }
  else {
    validrecv(recvcount, root, scattertag, comm);
  }

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->scatter()->execute(
    req, queue_, sendcount, sendtype, recvcount, recvtype, root, scattertag, comm, cc, os_);

  timestamp fini = queue_->progress_loop(req);

  payload::const_ptr collpay = ptr_test_cast(const payload, req->status().content());

  if (usingpayload && !collpay) {
    spkt_throw(sprockit::value_error, "mpiapi:scatter - null mpicollpayload");
  }

  result = collpay;

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Scatter data non-uniformly.
//
timestamp
mpi_api::scatterv(const std::vector<int> &sendcounts,
                  mpi_type_id sendtype, int recvcount, mpi_type_id recvtype,
                  mpi_id root, mpi_comm* comm)
{
  std::vector<payload::const_ptr> vals;

  if (comm->rank() == root) {
    for (mpi_id id = mpi_id(0); id < comm->size(); ++id) {
      vals.push_back(payload::null());
    }
  }

  payload::const_ptr result;
  timestamp fini = this->scatterv(sendcounts, sendtype, recvcount,
                                  recvtype, root, comm, vals, result, false);
  return fini;
}

//
// Scatter data non-uniformly.
//
timestamp
mpi_api::scatterv(const std::vector<int> &sendcounts,
                  mpi_type_id sendtype, int recvcount, mpi_type_id recvtype,
                  mpi_id root, mpi_comm* comm,
                  const std::vector<payload::const_ptr>& cc,
                  payload::const_ptr& result,
                  bool usingpayload)
{
  SSTMACBacktrace("MPI_Scatterv");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Scatterv(<...>,%s,%d,%s,%d,%s)",
    type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  start_api_call();
  mpi_tag scattertag = comm->next_collective_tag();
  assert_initialized();
  if (comm->rank() == root) {
    if (int(comm->size()) != (int) cc.size()) {
      spkt_throw(sprockit::value_error,
        "mpiapi: scatter - content size does not match comm size");
    }
    validsends(sendcounts, root, scattertag, comm);
  }
  else {
    validrecv(recvcount, root, scattertag, comm);
  }

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->scatterv()->execute(req, queue_, sendcounts, sendtype,
                                 recvcount, recvtype, root, scattertag, comm, cc, os_);

  timestamp fini = queue_->progress_loop(req);

  payload::const_ptr collpay = ptr_test_cast(const payload, req->status().content());

  if (usingpayload && !collpay) {
    spkt_throw(sprockit::value_error, "mpiapi:scatter: null mpicollpayload");
  }

  result = collpay;

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Gather uniformly distributed data.
//
timestamp
mpi_api::gather(int sendcount, mpi_type_id sendtype, int recvcount,
                mpi_type_id recvtype, mpi_id root, mpi_comm* comm)
{
  std::vector<payload::const_ptr> rv;
  return gather(sendcount, sendtype,
                recvcount, recvtype,
                root, comm,
                payload::null(), rv);
}

//
// Gather uniformly distributed data.
//
timestamp
mpi_api::gather(int sendcount, mpi_type_id sendtype, int recvcount,
                mpi_type_id recvtype, mpi_id root, mpi_comm* comm,
                const payload::const_ptr& content, std::vector<payload::const_ptr>& cc)
{
  SSTMACBacktrace("MPI_Gather");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Gather(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());

  start_api_call();

  mpi_tag gathertag = comm->next_collective_tag();
  assert_initialized();
  if (comm->rank() == root) {
    validrecv(recvcount, root, gathertag, comm);
  }
  else {
    validsend(sendcount, root, gathertag, comm);
  }

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->gather()->execute(req, queue_, sendcount, sendtype, recvcount,
                               recvtype, root, gathertag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  if (comm->rank() == root) {
    if (content) {
      mpi_collective_payload::const_ptr coll = ptr_safe_cast(const mpi_collective_payload,
        req->status().content(),
        "mpiapi::gather:  NULL mpicollpayload");
      cc = coll->get_content();
    }
  }

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Gather non-uniformly distributed data.
//
timestamp
mpi_api::gatherv(int sendcount, mpi_type_id sendtype,
                 const std::vector<int> &recvcnts, mpi_type_id recvtype,
                 mpi_id root, mpi_comm* comm)
{
  std::vector<payload::const_ptr> cc;
  return gatherv(sendcount, sendtype, recvcnts, recvtype, root, comm,
                 payload::null(), cc);
}

//
// Gather non-uniformly distributed data.
//
timestamp
mpi_api::gatherv(int sendcount, mpi_type_id sendtype,
                 const std::vector<int> &recvcnts, mpi_type_id recvtype,
                 mpi_id root, mpi_comm* comm,
                 const payload::const_ptr& cc,
                 std::vector<payload::const_ptr>& result)
{
  SSTMACBacktrace("MPI_Scatterv");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Gatherv(%d,%s,<...>,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  start_api_call();
  mpi_tag scattertag = comm->next_collective_tag();
  assert_initialized();
  if (comm->rank() == root) {
    validrecvs(recvcnts, root, scattertag, comm);
  }
  else {
    validsend(sendcount, root, scattertag, comm);
  }

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->gatherv()->execute(req, queue_, sendcount, sendtype, recvcnts,
                                recvtype, root, scattertag, comm, cc, os_);

  timestamp fini = queue_->progress_loop(req);

  if (comm->rank() == root) {
    mpi_collective_payload::const_ptr coll = ptr_safe_cast(const mpi_collective_payload,
        req->status().content(),
        "mpiapi::gather: NULL mpicollpayload");

    result = coll->get_content();
  }

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

std::string
mpi_api::op_str(mpi_op* op)
{
  return op->label;
}

std::string
mpi_api::type_str(mpi_type_id mid)
{
  mpi_type* ty = type_from_id(mid);
  switch(ty->type())
  {
    case mpi_type::PRIM:
      return ty->label;
    case mpi_type::PAIR:
      return "PAIR";
    case mpi_type::VEC:
      return "VEC";
    case mpi_type::IND:
      return "IND";
    case mpi_type::NONE:
      return "NONE";
  }
}

std::string
mpi_api::comm_str(mpi_comm_id comm)
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
mpi_api::tag_str(mpi_tag tag)
{
  if (tag==mpi::any_tag){
    return "MPI_TAG_ANY";
  }
  else {
    return sprockit::printf("%d", int(tag));
  }
}

std::string
mpi_api::src_str(mpi_id id)
{
  if (id == mpi::any_source){
    return "MPI_SOURCE_ANY";
  }
  else {
    return sprockit::printf("%d", int(id));
  }
}

std::string
mpi_api::src_str(mpi_comm* comm, mpi_id id)
{
  if (id == mpi::any_source){
    return "MPI_SOURCE_ANY";
  }
  else {
    return sprockit::printf("%d:%d", int(id), int(comm->peer_task(id)));
  }
}

//
// Gather uniformly distributed data and make it available to all nodes.
//
timestamp
mpi_api::allgather(int sendcount, mpi_type_id sendtype, int recvcount,
                   mpi_type_id recvtype, mpi_comm* comm)
{
  std::vector<payload::const_ptr> rv;
  return allgather(sendcount, sendtype, recvcount, recvtype, comm,
                   payload::null(), rv);
}

timestamp
mpi_api::run_allgather(int sendcount, mpi_type_id sendtype, int recvcount,
                   mpi_type_id recvtype, mpi_comm* comm,
                   const payload::const_ptr& content,
                   std::vector<payload::const_ptr>& result)
{
  mpi_tag mytag = comm->next_collective_tag();
  assert_initialized();
  validsend(sendcount, comm->rank(), mytag, comm);
  validrecv(recvcount, comm->rank(), mytag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_allgather_strategy* ag = strategy_->allgather();

  mpi_collective* coll = ag->execute(req, queue_, sendcount, sendtype, recvcount, recvtype, mytag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  if (content) {
    mpi_collective_payload::const_ptr coll = ptr_safe_cast(const mpi_collective_payload,
        req->status().content());
    result = coll->get_content();
  }

  delete req;
  delete coll;

  return fini;
}

//
// Gather uniformly distributed data and make it available to all nodes.
//
timestamp
mpi_api::allgather(int sendcount, mpi_type_id sendtype, int recvcount,
                   mpi_type_id recvtype, mpi_comm* comm,
                   const payload::const_ptr& content,
                   std::vector<payload::const_ptr>& result)
{
  maybe_skip_comm_world();

  SSTMACBacktrace("MPI_Allgather");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allgather(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());

  start_api_call();

  timestamp fini = run_allgather(sendcount, sendtype, recvcount, recvtype, comm, content, result);

  end_api_call();
  return fini;
}

//
// Gather non-uniformly distributed data onto all nodes.
//
timestamp
mpi_api::allgatherv(int sendcnt, mpi_type_id sendtype,
                    const std::vector<int> &recvcnts, mpi_type_id recvtype,
                    mpi_comm* comm)
{
  std::vector<payload::const_ptr> rv;
  return allgatherv(sendcnt, sendtype, recvcnts, recvtype, comm,
                    payload::null(), rv);
}

//
// Gather non-uniformly distributed data onto all nodes.
//
timestamp
mpi_api::allgatherv(int sendcnt, mpi_type_id sendtype,
                    const std::vector<int> &recvcnts, mpi_type_id recvtype,
                    mpi_comm* comm, const payload::const_ptr& content,
                    std::vector<payload::const_ptr>& result)
{
  maybe_skip_comm_world();

  SSTMACBacktrace("MPI_Allgatherv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allgatherv(%d,%s,<...>,%s,%s)",
    sendcnt, type_str(sendtype).c_str(),
    type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  start_api_call();
  mpi_tag mytag = comm->next_collective_tag();
  assert_initialized();
  validsend(sendcnt, comm->rank(), mytag, comm);
  validsends(recvcnts, comm->rank(), mytag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);

  mpi_collective* coll = strategy_->allgatherv()->execute(req, queue_, sendcnt, sendtype,
                                   recvcnts, recvtype, mytag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  mpi_collective_payload::const_ptr load = ptr_safe_cast(const mpi_collective_payload,
    req->status().content(),
    "mpiapi::allgatherv:  NULL mpicollpayload");

  result = load->get_content();

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Send uniformly distributed data from all to all processes.
//
timestamp
mpi_api::alltoall(int sendcount, mpi_type_id sendtype, int recvcount,
                  mpi_type_id recvtype, mpi_comm* comm)
{
  /** Create a mass of null content. The all to all later on expects a payload
   for each node. */
  std::vector<payload::const_ptr> cn(int(comm->size()), payload::null());
  std::vector<payload::const_ptr> rv;
  return alltoall(sendcount, sendtype, recvcount, recvtype, comm, cn, rv);
}

//
// Send uniformly distributed data from all to all processes.
//
timestamp
mpi_api::alltoall(int sendcount, mpi_type_id sendtype, int recvcount,
                  mpi_type_id recvtype, mpi_comm* comm,
                  const std::vector<payload::const_ptr>& content,
                  std::vector<payload::const_ptr>& result)
{
  maybe_skip_comm_world();

  SSTMACBacktrace("MPI_Alltoall");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Alltoall(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());

  start_api_call();
  mpi_tag a2atag = comm->next_collective_tag();
  assert_initialized();
  validsend(sendcount, comm->rank(), a2atag, comm);
  validrecv(recvcount, comm->rank(), a2atag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);

  mpi_collective* coll = strategy_->alltoall()->execute(req, queue_, sendcount, sendtype,
                                 recvcount, recvtype, a2atag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  mpi_collective_payload::const_ptr pay = ptr_safe_cast(const mpi_collective_payload,
    req->status().content(),
    "mpiapi::alltoall:  NULL mpicollpayload");

  result = pay->get_content();

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//
// Send non-uniformly distributed data from all to all processors.
//
timestamp
mpi_api::alltoallv(const std::vector<int> &sendcnts,
                   mpi_type_id sendtype, const std::vector<int> &recvcnts,
                   mpi_type_id recvtype, mpi_comm* comm)
{
  std::vector<payload::const_ptr> cn(comm->size(), payload::const_ptr());
  std::vector<payload::const_ptr> rv(comm->size(), payload::const_ptr());
  return alltoallv(sendcnts, sendtype, recvcnts, recvtype, comm, cn, rv);
}

void
print_quantiles(const std::vector<int>& counts){
  int max_size = 0; int min_size = 1e9;
  int nranks = counts.size();
  for (int i=0; i < nranks; ++i){
    max_size = std::max(max_size, counts[i]);
    min_size = std::min(min_size, counts[i]);
  }
  int range = max_size - min_size;
  int qtile_34 = range*0.75 + min_size;
  int qtile_12 = range*0.5 + min_size;
  int qtile_14 = range*0.25 + min_size;
  int qtile_0 = min_size + 1;
  int hist[6] = {0,0,0,0,0,0};
  for (int i=0; i < nranks; ++i){
    int idx = ceil((counts[i] - min_size)*5.0 / range);
    hist[idx]++;
  }
  printf("      %5d %5d %5d %5d %5d\n", max_size, qtile_34, qtile_12, qtile_14, qtile_0);
  printf("        MAX   Q34   Q12   Q14   MIN\n");
  printf("  %5d %5d %5d %5d %5d %5d\n", hist[5], hist[4], hist[3], hist[2], hist[1], hist[0]);
}

//
// Send non-uniformly distributed data from all to all processors.
//
timestamp
mpi_api::alltoallv(const std::vector<int> &sendcnts,
                   mpi_type_id sendtype, const std::vector<int> &recvcnts,
                   mpi_type_id recvtype, mpi_comm* comm,
                   const std::vector<payload::const_ptr>& content,
                   std::vector<payload::const_ptr>& result)
{
  int my_rank = comm->rank();
  //if (my_rank == 0){
  //  printf("Send Counts\n");
  //  print_quantiles(sendcnts);
  //  printf("Recv Counts\n");
  //  print_quantiles(recvcnts);
  //}
  maybe_skip_comm_world();

  SSTMACBacktrace("MPI_Alltoallv");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Alltoallv(<...>,%s,<...>,%s,%s)",
    type_str(sendtype).c_str(), type_str(recvtype).c_str(), comm_str(comm).c_str());

  start_api_call();
  mpi_tag a2avtag = comm->next_collective_tag();
  assert_initialized();
  validsends(sendcnts, comm->rank(), a2avtag, comm);
  validrecvs(recvcnts, comm->rank(), a2avtag, comm);


  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->alltoallv()->execute(req, queue_, sendcnts, sendtype,
                                  recvcnts, recvtype, a2avtag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  mpi_collective_payload::const_ptr pay = ptr_safe_cast(const mpi_collective_payload,
    req->status().content(),
    "mpiapi::alltoall:  NULL mpicollpayload");

  result = pay->get_content();

  delete coll;
  delete req;

  end_api_call();
  return fini;
}

//
// Reduce data from all nodes onto the root node.
//
timestamp
mpi_api::reduce(int count, mpi_type_id type, mpi_op* op,
                mpi_id root, mpi_comm* comm)
{
  payload::const_ptr result;
  return reduce(count, type, op, root, comm, payload::null(), result);
}

//
// Reduce data from all nodes onto the root node.
//
timestamp
mpi_api::reduce(int count, mpi_type_id type, mpi_op* op,
                mpi_id root, mpi_comm* comm,
                const payload::const_ptr& content,
                payload::const_ptr& result)
{
  SSTMACBacktrace("MPI_Reduce");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Reduce(%d,%s,%s,%d,%s)", count, type_str(type).c_str(),
    op_str(op).c_str(), int(root), comm_str(comm).c_str());

  start_api_call();
  mpi_tag reducetag = comm->next_collective_tag();
  assert_initialized();
  validsend(count, comm->rank(), reducetag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->reduce()->execute(req, queue_, count, type, op, root,
                               reducetag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  if (comm->rank() == root) {
    result = req->status().content();
  }
  else {
    result = 0;
  }

  delete coll;
  delete req;

  end_api_call();
  return fini;
}

//
// Reduce data from all nodes to all nodes.
//
timestamp
mpi_api::allreduce(int count, mpi_type_id type, mpi_op* op,
                   mpi_comm* comm)
{
  payload::const_ptr result;
  return allreduce(count, type, op, comm, payload::null(), result);
}

timestamp
mpi_api::allreduce(int count, mpi_type_id type, mpi_op* op,
                   mpi_comm* comm, const payload::const_ptr& content,
                   payload::const_ptr& result)
{
  SSTMACBacktrace("MPI_Allreduce");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allreduce(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(op).c_str(), comm_str(comm).c_str());

  start_api_call();
  mpi_tag allreducetag = comm->next_collective_tag();
  assert_initialized();
  validsend(count, comm->rank(), allreducetag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->allreduce()->execute(req, queue_, count, type, op,
                                  allreducetag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  result = req->status().content();

  delete coll;
  delete req;

  end_api_call();
  return fini;
}

//
// Reduce data and scatter results.
//
timestamp
mpi_api::reduce_scatter(const std::vector<int> &recvcnts,
                        mpi_type_id type, mpi_op* op, mpi_comm* comm)
{
  payload::const_ptr content = payload::null();
  payload::const_ptr result = payload::null();
  return reduce_scatter(recvcnts, type, op, comm, content, result);
}

//
// Reduce data and scatter results.
//
timestamp
mpi_api::reduce_scatter(const std::vector<int> &recvcnts,
                        mpi_type_id type, mpi_op* op, mpi_comm* comm,
                        const payload::const_ptr& content,
                        payload::const_ptr& result)
{
  SSTMACBacktrace("MPI_Reducescatter");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Reduce_scatter(<...>,%s,%s,%s)",
    type_str(type).c_str(), op_str(op).c_str(), comm_str(comm).c_str());
  start_api_call();
  static mpi_tag rscattag(0xFFAA1A);
  assert_initialized();
  validsends(recvcnts, comm->rank(), rscattag, comm);
  //
  timestamp entry = os_->now();
  mpi_request* req = mpi_request::construct(default_key_category);
  strategy_->reduce_scatter()->execute(req, queue_, recvcnts, type, op,
                                       rscattag, comm, content, os_);

  timestamp fini = queue_->progress_loop(req);

  result = req->status().content();

  end_api_call();
  return fini;
}

//
// Compute partial reductions on data.
//
timestamp
mpi_api::scan(int count, mpi_type_id type, mpi_op* op,
              mpi_comm* comm)
{
  payload::const_ptr content = payload::null();
  payload::const_ptr result = payload::null();
  return this->scan(count, type, op, comm, content, result);
}

//
// Compute partial reductions and aggregate data.
//
timestamp
mpi_api::scan(int count, mpi_type_id type, mpi_op* op,
              mpi_comm* comm, const payload::const_ptr& content,
              payload::const_ptr& result)
{
  SSTMACBacktrace("MPI_Scan");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Scan(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(op).c_str(), comm_str(comm).c_str());
  start_api_call();

  mpi_tag scantag = comm->next_collective_tag();
  assert_initialized();
  validsend(count, comm->rank(), scantag, comm);

  mpi_request* req = mpi_request::construct(default_key_category);
  mpi_collective* coll = strategy_->scan()->execute(req, queue_, count, type, op, scantag, comm,
                             content, os_);

  timestamp fini = queue_->progress_loop(req);
  result = req->status().content();

  delete req;
  delete coll;

  end_api_call();
  return fini;
}

//----------------------------------------------------------------
// --- MPI Derived Datatype (experimental)
//----------------------------------------------------------------

//
// Creates a contiguous datatype
//
timestamp
mpi_api::type_contiguous(int count, mpi_type_id old_type,
                         mpi_type_id& new_type)
{
  mpi_type* new_type_obj = new mpi_type;
  mpi_type* old_type_obj = type_from_id(old_type);
  new_type_obj->init_vector("contiguous-" + old_type_obj->label,
                        old_type_obj,
                        count, 1,
                        1, true, MPI_COMBINER_CONTIGUOUS);

  allocate_type_id(new_type_obj);
  new_type = new_type_obj->id;
  return os_->now();
}

/// Creates a vector (strided) datatype
timestamp
mpi_api::type_vector(int count, int blocklength, int stride,
                     mpi_type_id old_type, mpi_type_id &new_type, bool stride_in_elem)
{
  int comb = (stride_in_elem) ? MPI_COMBINER_VECTOR : MPI_COMBINER_HVECTOR;
  std::stringstream ss;
  ss << "vector-" << type_label(old_type) << "\n";

  mpi_type* new_type_obj = new mpi_type;
  new_type_obj->init_vector(ss.str(), type_from_id(old_type),
                        count, blocklength, stride,
                        stride_in_elem, comb);

  allocate_type_id(new_type_obj);
  new_type = new_type_obj->id;
  return os_->now();
}

//
// Creates a struct datatype
//
timestamp
mpi_api::type_struct(const int count, const std::vector<int> &blocklens,
                     const std::vector<int> &indices, const std::vector<mpi_type_id> &old_types,
                     mpi_type_id &newtype)
{
  mpi_type* new_type_obj = new mpi_type;
  if (count > 0) {
    inddata* idata = new inddata;
    int maxdisp = -1;
    int maxindex = 0;
    int typesizes = 0;
    int maxsize = 0;
    int mindisp = INT_MAX;
    int index = 0;
    int ub = -1;
    int lb = -1;
    int align = 0;
    bool ubset = false;
    bool lbset = false;
    for (int i = 0; i < count; i++) {
      if (old_types[i] == mpi_type::mpi_lb->id) {
        lb = indices[i];
        lbset = true;
      }
      else if (old_types[i] == mpi_type::mpi_ub->id) {
        ub = indices[i];
        ubset = true;

        //if (ub > maxdisp)
        // {
        //  maxdisp = ub;
        // maxindex = i;
        // maxsize = 0;
        // }

      }
      else if (blocklens[i] > 0) {
        mpi_type* old_type_obj = type_from_id(old_types[i]);
        idata->blocks[index] = ind_block();
        idata->blocks[index].base = old_type_obj;
        idata->blocks[index].disp = indices[i];
        idata->blocks[index].num = blocklens[i];
        typesizes += old_type_obj->packed_size() * blocklens[i];
        if (indices[i] > maxdisp) {
          maxdisp = indices[i];
          maxindex = i;
          maxsize = old_type_obj->extent();
        }

        if (indices[i] < mindisp) {
          mindisp = indices[i];
        }

        if (old_type_obj->type() == mpi_type::PRIM) {
          align = std::max(align, old_type_obj->align());
        }
        index++;
      }
    }

    if (!lbset) {
      lb = mindisp;
    }
    if (!ubset) {
      if (index == 0) {
        //there was no data
        ub = 0;
      }
      else {
        ub = maxdisp + maxsize * blocklens[maxindex];
      }
      if (align > 0) {
        while ((double) ub / (double) align != ub / align) {
          ub++;
        }
      }

    }
    idata->ub_ = ub;
    idata->lb_ = lb;
    idata->mindisp_ = mindisp;
    idata->maxbyte_ = maxdisp + maxsize * blocklens[maxindex];

    new_type_obj->init_indexed("struct", idata, typesizes, ub - lb,
                         index * 2 + 1, index, MPI_COMBINER_STRUCT);

    //SSTMAC_DEBUG << "mpiapi: building struct, maxdisp: " << maxdisp
    //             << ", maxsize: " << maxsize << ", block len of max: "
    //             << blocklens[maxindex] << ", ub: " << ub << ", lb: " << lb
    //             << ", extent: " << new_type_obj->extent() << "\n";


  }
  else {
    inddata* idata = new inddata;
    idata->ub_ = 0;
    idata->lb_ = 0;
    idata->mindisp_ = 0;
    idata->maxbyte_ = 0;

    new_type_obj->init_indexed("struct", idata, 0, 0, 1, 0, MPI_COMBINER_STRUCT);
  }
  allocate_type_id(new_type_obj);
  newtype = new_type_obj->id;
  return os_->now();
}

int
mpi_api::type_size(mpi_type_id type)
{
  return type_from_id(type)->packed_size();
}

timestamp
mpi_api::type_dup(mpi_type_id intype, mpi_type_id &outtype)
{
  mpi_type* new_type_obj = type_from_id(intype);
  if (new_type_obj->comb() == MPI_COMBINER_NAMED) {
    new_type_obj->set_comb(MPI_COMBINER_DUP);
  }
  allocate_type_id(new_type_obj);
  outtype = new_type_obj->id;
  return this->wtime();
}

timestamp
mpi_api::type_set_name(mpi_type_id id, const std::string &name)
{
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
    spkt_throw_printf(sprockit::value_error,
        "mpi_api::type_set_name: cannot set name %s for unknown id %d",
        name.c_str(), int(id));
  }
  it->second->label = name;
  return this->wtime();
}

timestamp
mpi_api::type_indexed(int count, int lens[], const std::vector<int> &ind,
                      mpi_type_id intype, mpi_type_id &outtype, bool in_elem, int comb)
{
  mpi_type* out_type_obj = new mpi_type;
  if (count > 0) {
    mpi_type* in_type_obj = type_from_id(intype);
    inddata* idata = new inddata;
    int maxdisp = -1;
    int maxindex = 0;
    int mindisp = INT_MAX;
    int index = 0;
    int typesizes = 0;
    for (int i = 0; i < count; i++) {
      if (lens[i] > 0) {
        int bytesdisp;
        if (in_elem) {
          bytesdisp = ind[i] * in_type_obj->extent();
        }
        else {
          bytesdisp = ind[i];
        }


        idata->blocks[index] = ind_block();
        idata->blocks[index].base = in_type_obj;
        idata->blocks[index].disp = bytesdisp;
        idata->blocks[index].num = lens[i];
        typesizes += lens[i] * in_type_obj->packed_size();
        //  size += intype.size * lens[i];
        if (bytesdisp > maxdisp) {
          maxdisp = bytesdisp;
          maxindex = i;
        }

        if (bytesdisp < mindisp) {
          mindisp = bytesdisp;
        }
        index++;
      }

    }

    idata->ub_ = maxdisp + in_type_obj->extent() * lens[maxindex];
    idata->lb_ = mindisp;
    idata->mindisp_ = mindisp;
    idata->maxbyte_ = maxdisp + in_type_obj->extent() * lens[maxindex];

    out_type_obj->init_indexed(in_type_obj->label, idata, typesizes,
                       idata->ub_ - idata->lb_, index * 2 + 1, 1, comb);

    //SSTMAC_DEBUG << "mpiapi: building indexed, maxdisp: " << maxdisp
    //             << ", maxsize: " << idata->ub_ << ", block len of max: "
    //             << lens[maxindex] << ", size: " << out_type_obj->packed_size()
    //             << ", extent: " << out_type_obj->extent() << "\n";


  }
  else {
    inddata* idata = new inddata;
    idata->ub_ = 0;
    idata->lb_ = 0;
    idata->mindisp_ = 0;
    idata->maxbyte_ = 0;

    out_type_obj->init_indexed("struct", idata, 0, 0, 1, 0, comb);
  }
  allocate_type_id(out_type_obj);
  outtype = out_type_obj->id;
  return os_->now();
}

std::string
mpi_api::type_label(mpi_type_id tid)
{
  mpi_type* ty = type_from_id(tid);
  return ty->label;
}

//
// A datatype object has to be committed before use in communication.
//
timestamp
mpi_api::type_commit(mpi_type_id type)
{
  mpi_type* type_obj = type_from_id(type);
  type_obj->set_committed(true);
  return os_->now();
}

void
mpi_api::type_commit(MPI_Datatype dtype)
{
  type_commit(mpi_type_id(dtype));
}

void
mpi_api::allocate_type_id(mpi_type* type)
{
  spkt_unordered_map<mpi_type_id, mpi_type*>::iterator it, end = known_types_.end();
  while ((it = known_types_.find(next_type_id_)) != end){
    ++next_type_id_;
  }
  type->id = next_type_id_;
  known_types_[type->id] = type;
}

void
mpi_api::precommit_type(mpi_type* type, int id)
{
  mpi_type_id tid(id);
  if (known_types_.find(tid) != known_types_.end()){
    spkt_throw_printf(sprockit::value_error,
      "mpi_api::precommit_type: %d already exists",
      id);
  }
  type->id = tid;
  known_types_[tid] = type;
  known_types_[tid]->set_committed(true);
}

//
// Mark datatype for deallocation.
//
timestamp
mpi_api::type_free(mpi_type_id type)
{
  mpi_type* type_obj = type_from_id(type);
  type_obj->set_committed(false);
  return os_->now();
}

void
mpi_api::type_free(MPI_Datatype dtype)
{
  type_free(mpi_type_id(dtype));
}

//
// Get the derived mpitype mapped to an id
//
mpi_type*
mpi_api::type_from_id(mpi_type_id id)
{
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
    spkt_throw_printf(sprockit::invalid_key_error,
        "mpi_api: unknown type id %d",
        int(id));
  }
  return it->second;
}

mpi_op*
mpi_api::created_op(unsigned long id)
{
  if (created_op_.find(id) == created_op_.end()) {
    spkt_throw_printf(sprockit::value_error, "mpiapi::get_created_op - could not find id %lu",
                     id);
  }

  return created_op_[id];
}

timestamp
mpi_api::op_create(mpi_op::op_fxn fxn, int comm, mpi_op* &output)
{
  output = new mpi_op(fxn, comm);
  created_op_[output->id] = output;
  return os_->now();
}

timestamp
mpi_api::op_free(long id)
{
  created_op_.erase(id);
  return os_->now();
}

timestamp
mpi_api::win_create(void* base, size_t size, int disp, mpi_info* inf,
                    mpi_comm* comm, mpi_window* &win, long id)
{
  SSTMACBacktrace("MPI_Win_create");
  mpi_window* ret = new mpi_window(base, size, disp, inf, comm, id);

  barrier(comm);

  win = ret;

  queue_->win_create(ret);
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_free(mpi_window* win)
{
  assert_initialized();

  barrier(win->comm());

  queue_->win_free(win);

  return os_->now();
}

timestamp
mpi_api::win_get(mpi_request* &req, int origin_count,
                 mpi_type_id origin_datatype, mpi_id target_rank,
                 MPI_Aint target_disp, int target_count,
                 mpi_type_id target_datatype,
                 mpi_window* win)
{
  SSTMACBacktrace("MPI_Win_get");
  start_api_call();
  assert_initialized();

  timestamp entry = os_->now();
  req = mpi_request::construct(default_key_category);
  event_handler* completion = queue_->progress_done_handler(os_, req);

  mpi_type* target_type_obj = type_from_id(target_datatype);

  mpi_rma_message::op_info op;
  op.disp_ = target_disp;
  op.count_ = target_type_obj->packed_size() * target_count;
  op.type_ = mpi_rma_message::get;
  op.epoch_ = win->current_epoch();
  op.target_ = target_rank;

  queue_->get(req, origin_count, origin_datatype, target_rank, win->comm(),
              win, op, completion);

  pending_rmas_[win][req] = op;
  delete completion;
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_put(mpi_request* &req, const payload::const_ptr& load,
                 int target_count, mpi_type_id target_datatype,
                 mpi_id target_rank, long long target_disp,
                 mpi_window* win)
{
  SSTMACBacktrace("MPI_Win_put");
  start_api_call();
  assert_initialized();

  timestamp entry = os_->now();
  req = mpi_request::construct(default_key_category);
  event_handler* completion = queue_->progress_done_handler(os_, req);

  mpi_type* target_type_obj = type_from_id(target_datatype);

  mpi_rma_message::op_info op;
  op.disp_ = target_disp;
  op.count_ = target_type_obj->packed_size() * target_count;
  op.type_ = mpi_rma_message::put;
  op.epoch_ = win->current_epoch();
  op.target_ = target_rank;

  queue_->put(req, target_count, target_datatype, target_rank, win->comm(),
              win, op, completion, load);

  pending_rmas_[win][req] = op;
  delete completion;
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_accumulate(mpi_request* &req, const payload::const_ptr& load,
                        int target_count, mpi_type_id target_datatype,
                        mpi_id target_rank, long long target_disp, mpi_op* o,
                        mpi_window* win)
{
  SSTMACBacktrace("MPI_Accumulate");
  start_api_call();
  assert_initialized();

  timestamp entry = os_->now();

  req = mpi_request::construct(default_key_category);
  event_handler* completion = queue_->progress_done_handler(os_, req);

  mpi_type* target_type_obj = type_from_id(target_datatype);

  mpi_rma_message::op_info op;
  op.disp_ = target_disp;
  op.count_ = target_type_obj->packed_size() * target_count;
  op.type_ = mpi_rma_message::acc;
  op.epoch_ = win->current_epoch();
  op.target_ = target_rank;
  op.op_ = o;

  queue_->put(req, target_count, target_datatype, target_rank, win->comm(),
              win, op, completion, load);

  pending_rmas_[win][req] = op;
  delete completion;
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_fence(mpi_window* win, bool barr,
                   spkt_unordered_map<mpi_request*, payload::const_ptr> &loads)
{
  SSTMACBacktrace("MPI_Win_fence");
  start_api_call();
  assert_initialized();

  std::queue<mpi_request*> toremove;

  spkt_unordered_map<mpi_request*, mpi_rma_message::op_info>::iterator
  it, end = pending_rmas_[win].end();
  for (it = pending_rmas_[win].begin(); it != end; it++) {
    mpi_request* req = it->first;
    mpi_rma_message::op_info& info = it->second;
    if (info.epoch_ == win->current_epoch()) {
      if (!req->is_complete()) {
        os_->block(req->get_key());
      }
      if (info.type_ == mpi_rma_message::get) {
        loads[req] = req->status().content();
      }
      toremove.push(it->first);
    }
  }

  while (toremove.size() > 0) {
    pending_rmas_[win].erase(toremove.front());
    toremove.pop();
  }

  if (barr) {
    barrier(win->comm());
  }

  win->next_epoch();
  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_lock(LOCK_TYPE t, mpi_id rank, WIN_FLAGS flag,
                  mpi_window* win)
{
  SSTMACBacktrace("MPI_Win_lock");
  start_api_call();
  assert_initialized();

  // if(flag != mpi_win_mode_nocheck_){
  // queue_->win_lock(t, rank, win);
  // }

  end_api_call();
  return os_->now();
}

timestamp
mpi_api::win_unlock(mpi_id rank, mpi_window* win,
                    spkt_unordered_map<mpi_request*, payload::const_ptr> &loads)
{
  SSTMACBacktrace("MPI_Win_unlock");
  start_api_call();
  assert_initialized();

  std::queue<mpi_request*> toremove;

  spkt_unordered_map<mpi_request*, mpi_rma_message::op_info>::iterator
  it, end = pending_rmas_[win].end();
  for (it = pending_rmas_[win].begin(); it != end; it++) {
    mpi_request* req = it->first;
    mpi_rma_message::op_info& info = it->second;
    if (info.epoch_ == win->current_epoch() && info.target_ == rank) {
      if (!req->is_complete()) {
        os_->block(req->get_key());
      }
      if (info.type_ == mpi_rma_message::get) {
        loads[req] = req->status().content();
      }
      toremove.push(req);
    }
  }

  while (toremove.size() > 0) {
    pending_rmas_[win].erase(toremove.front());
    toremove.pop();
  }

  win->next_epoch();
  end_api_call();
  return os_->now();

}

void
mpi_api::copy_err_handler(MPI_Comm dst, MPI_Comm src)
{
  err_handlers_[dst] = err_handlers_[src];
}

void*
mpi_api::find_buffer(MPI_Request req)
{
  buf_map_t::iterator it = buffers_.find(req);
  if (it == buffers_.end()){
    return SPROCKIT_FAKE_PTR;
  } else {
    return it->second;
  }
}

void
mpi_api::add_buffer(MPI_Request req, void *buffer)
{
  buffers_[req] = buffer;
}

void
mpi_api::erase_buffer(MPI_Request req)
{
  buffers_.erase(req);
}

mpi_comm*
mpi_api::get_comm(MPI_Comm comm)
{
  spkt_unordered_map<MPI_Comm, mpi_comm*>::iterator it
    = comm_map_.find(comm);
  if (it == comm_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi communicator %d for rank %d",
        comm, int(rank_));
  }
  return it->second;
}

mpi_info*
mpi_api::get_info(MPI_Info inf)
{
  info_map::iterator it = info_map_.find(inf);
  if (it == info_map_.end()){
    spkt_throw_printf(sprockit::spkt_error,
     "could not find mpi info %d for rank %d",
     inf, int(rank_));
  }
  return it->second;
}

mpi_window*
mpi_api::get_window(MPI_Win win)
{
  win_map::iterator it = win_map_.find(win);
  if (it == win_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi window %d for rank %d",
        win, int(rank_));
  }
  return it->second;
}

MPI_Info
mpi_api::add_info_ptr(mpi_info* ptr)
{
  MPI_Info inf = info_counter_++;
  info_map_[inf] = ptr;
  return inf;
}

MPI_Win
mpi_api::add_win_ptr(mpi_window* ptr, MPI_Win win)
{
  win_map_[win] = ptr;
  return win;
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
mpi_api::add_group_ptr(MPI_Group grp, mpi_group*ptr)
{
  grp_map_[grp] = ptr;
}


MPI_Group
mpi_api::add_group_ptr(mpi_group* ptr)
{
  MPI_Group grp = group_counter_++;
  grp_map_[grp] = ptr;
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
  req_map_.erase(it);
}

void
mpi_api::add_comm_grp(MPI_Comm comm, MPI_Group grp)
{
  comm_grp_map_[comm] = grp;
}

MPI_Group
mpi_api::get_comm_grp(MPI_Comm comm)
{
  comm_grp_map::iterator it = comm_grp_map_.find(comm);
  if (it == comm_grp_map_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "could not find mpi group for comm %d for rank %d",
        comm, int(rank_));
  }
  return it->second;
}

void
mpi_api::erase_err_handler(MPI_Comm comm)
{
  err_handlers_.erase(comm);
}

void
mpi_api::add_err_handler(MPI_Comm comm, MPI_Errhandler err)
{
  err_handlers_[comm] = err;
}

MPI_Errhandler
mpi_api::get_err_handler(MPI_Comm comm)
{
  err_handler_map::iterator it = err_handlers_.find(comm);
  if (it == err_handlers_.end()) {
    spkt_throw_printf(sprockit::value_error,
       "Invalid communicator %d in MPI_Errhandler_get", comm);
  }
  return it->second;
}

void
mpi_api::attach_buffer(void *buffer, int size)
{
  attached_buffer_ = std::make_pair(buffer, size);
}

void
mpi_api::detach_buffer(void **buffer, int *size)
{
  *buffer = attached_buffer_.first;
  *size = attached_buffer_.second;
  attached_buffer_= std::make_pair<void*, int>(NULL, 0);
}

void
mpi_api::check_key(int key)
{
  if (keyvals_.find(key) == keyvals_.end()) {
    spkt_throw_printf(sprockit::spkt_error,
        "mpi_api::check_key: could not find keyval %d in key_map", key);
  }
}


}
} // end of namespace sstmac


