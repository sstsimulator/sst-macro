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

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_comm/keyval.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_data.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

namespace sstmac {
namespace sw {

//
// NULL communicator.
//
mpi_comm* mpi_comm::comm_null;

mpi_comm::mpi_comm() :
  group_(),
  next_collective_tag_(10235),
  id_(-1),
  rank_(-1)
{
  topotype_ = TOPO_NONE;
}

mpi_comm::mpi_comm(
  mpi_comm_id id, //const appid &aid,
  mpi_id rank, mpi_group* peers,
  app_manager* env, app_id aid) :
  env_(env), group_(peers), next_collective_tag_(10235),
  aid_(aid), id_(id), rank_(rank)
{
  if (peers->size() == 0) {
    spkt_throw_printf(sprockit::value_error, "trying to build communicator of size 0");
  }

  topotype_ = TOPO_NONE;

  if (!comm_null) {
    comm_null = new mpi_comm;
  }
}

void
mpi_comm::dup_keyvals(mpi_comm* m)
{
  spkt_unordered_map<int, keyval*>::iterator it, end = m->keyvals_.end();
  for (it = m->keyvals_.begin(); it != end; it++) {
    keyval* c = (it->second)->clone(keyval::get_new_key());
    keyvals_[c->key()] = c;
  }
}

std::string
mpi_comm::to_string() const
{
  return "mpicomm(" + rank().to_string() + ")";
}

/// The size of the communicator.
mpi_id
mpi_comm::size() const
{
  if (id_ == mpi::null_comm) {
    spkt_throw(sprockit::value_error,
              "mpicomm: trying to call size() on a null mpicomm");
  }
  if (!group_) {
    spkt_throw(sprockit::null_error,
              "mpicomm: peers_ is null for some reason in size()");
  }
  return mpi_id(group_->size());
}

void
mpi_comm::set_keyval(keyval* k, void* val)
{
  keyvals_[k->key()] = k;
  k->set_val(val);
}

void
mpi_comm::get_keyval(keyval* k, void* val, int* flag)
{

  if (keyvals_.find(k->key()) == keyvals_.end()) {
    *flag = 1;
  }
  else {
    *flag = 0;
  }

  void** vcast = (void**)val;

  *vcast = k->val();

  //memcpy(val, (k->val()), sizeof(void*));

  //val = (k->val());
}

//
// Get a unique tag for a collective operation.
//
mpi_tag
mpi_comm::next_collective_tag() const
{
  next_collective_tag_++;
  return next_collective_tag_;
}

void
mpi_comm::validate(const char* fxn) const
{
#if SSTMAC_SANITY_CHECK
  if (id_ == mpi::null_comm) {
    spkt_throw_printf(sprockit::null_error,
                     "mpicomm::%s trying to call function on a null mpicomm",
                     fxn);
  }
  if (!group_) {
    spkt_throw_printf(sprockit::null_error,
                     "mpicomm::%s: group_ is null for some reason",
                     fxn);
  }
  if (rank_.id_ < 0|| rank_.id_ >= env_->nproc()) {
    spkt_throw_printf(sprockit::illformed_error,
                     "mpicomm::%s: invalid rank %d",
                     fxn, rank_.id_);
  }
  if (!env_) {
    spkt_throw_printf(sprockit::null_error,
                     "mpicomm::validate: env_ is null for some reason",
                     fxn);
  }
#endif
}

/// The task index of the caller.
task_id
mpi_comm::my_task() const
{
  validate("my_task");
  return group_->at(rank_);
}

/// The task index of the given peer.
task_id
mpi_comm::peer_task(mpi_id rank) const
{
  validate("peer_task");
  return group_->at(rank);
}

node_id
mpi_comm::my_node() const
{
  validate("my_node");
  task_id tid = my_task();
  return env_->node_for_task(tid);
}

/// The list of nodes involved in this communicator.
/// Indexing is done by mpiid::rank().id.
node_id
mpi_comm::node_at(mpi_id rank) const
{
  validate("node_at");
  task_id tid = peer_task(rank);
  return env_->node_for_task(tid);
}

//
// Fairly self-explanatory.
//
std::ostream&
operator<<(std::ostream &os, mpi_comm* comm)
{
  os << "mpicomm(rank=" << comm->rank_ << ", id=" << comm->id_ << ")";
  return os;
}

}
} // end of namespace sstmac

