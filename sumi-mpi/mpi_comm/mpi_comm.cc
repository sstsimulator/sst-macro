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

#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_comm/keyval.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>
#include <sprockit/statics.h>

static sprockit::need_delete_statics<sumi::mpi_comm> cleanup_comm;

namespace sumi {

//
// NULL communicator.
//
mpi_comm* mpi_comm::comm_null = nullptr;

mpi_comm::mpi_comm() :
  group_(nullptr),
  next_collective_tag_(0),
  id_(MPI_COMM_NULL),
  rank_(-1),
  del_grp_(false),
  communicator(-1),
  topotype_(TOPO_NONE)
{
}

mpi_comm::~mpi_comm()
{
  if (del_grp_) delete group_;
}

mpi_comm::mpi_comm(
  MPI_Comm id, //const appid &aid,
  int rank, mpi_group* peers,
  app_id aid,
  bool del_grp,
  topotypes ty) :
  sumi::communicator(rank),
  group_(peers),
  next_collective_tag_(MPI_COMM_WORLD + 100),
  aid_(aid),
  id_(id),
  rank_(rank),
  del_grp_(del_grp),
  topotype_(ty)
{
  if (peers->size() == 0) {
    spkt_throw_printf(sprockit::value_error,
         "trying to build communicator of size 0");
  }

  if (!comm_null) {
    comm_null = new mpi_comm;
  }
}

void
mpi_comm::delete_statics()
{
  if (comm_null) delete comm_null;
}

int
mpi_comm::global_to_comm_rank(int global_rank) const
{
  spkt_throw(sprockit::unimplemented_error,
    "mpi_comm::global_to_comm_rank");
}

void
mpi_comm::dup_keyvals(mpi_comm* m)
{
  spkt_unordered_map<int, keyval*>::iterator it, end = m->keyvals_.end();
  for (it = m->keyvals_.begin(); it != end; it++) {
    spkt_throw(sprockit::unimplemented_error,
      "dup_keyvals");
    //keyval* c = (it->second)->clone(keyval::get_new_key());
    //keyvals_[c->key()] = c;
  }
}

std::string
mpi_comm::to_string() const
{
  return sprockit::printf("mpicomm(id=%d,size=%d,rank=%d)", id_, size(), rank_);
}

/// The size of the communicator.
int
mpi_comm::size() const
{
  if (id_ == MPI_COMM_NULL) {
    spkt_throw(sprockit::value_error,
              "mpicomm: trying to call size() on a null mpicomm");
  }
  if (!group_) {
    spkt_throw(sprockit::null_error,
              "mpicomm: peers_ is null for some reason in size()");
  }
  return group_->size();
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
int
mpi_comm::next_collective_tag()
{
  uint16_t id = id_;
  int next_tag = (id << 16) | next_collective_tag_;
  next_collective_tag_++;
  return next_tag;
}

/// The task index of the caller.
task_id
mpi_comm::my_task() const
{
  return group_->at(rank_);
}

/// The task index of the given peer.
task_id
mpi_comm::peer_task(int rank) const
{
  return group_->at(rank);
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