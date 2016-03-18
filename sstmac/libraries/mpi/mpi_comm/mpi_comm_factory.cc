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

#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_factory_pending.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm.h>
#include <sstmac/libraries/mpi/mpi_comm/mpi_comm_cart.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/libraries/mpi/mpi_payload.h>
#include <sstmac/libraries/mpi/sstmac_mpi_integers.h>
#include <sstmac/libraries/mpi/mpi_types.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/thread_info.h>
#include <sprockit/errors.h>

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include <stdint.h>
#include <iterator>

namespace sstmac {
namespace sw {


//
// Build comm_world using information retrieved from the environment.
//
mpi_comm_factory::mpi_comm_factory(app_id aid, mpi_api* parent) :
  parent_(parent),
  aid_(aid),
  mpirun_np_(0),
  next_id_(1),
  splittype_(0),
  global_grp_(0),
  self_grp_(0)
{
}

//
// Goodbye.
//
mpi_comm_factory::~mpi_comm_factory()
{
  if (worldcomm_) delete worldcomm_;
  if (selfcomm_) delete selfcomm_;
  if (splittype_) delete splittype_;
  if (global_grp_) delete global_grp_;
  if (self_grp_) delete self_grp_;
}

//
// Initialize.
//
void
mpi_comm_factory::init(app_manager* env, mpi_id rank)
{
  next_id_.id_ = 1;

  mpirun_np_ = env->nproc();

  //SSTMAC_DEBUG << "MPI comm factory[" << int(rank)<< "]: app count is "
  //             << mpirun_np_ << "\n";

  const mpi_comm_id cid(0);
  std::pair<app_id, mpi_comm_id> index = std::make_pair(aid_, cid);
  //std::make_pair<app_id, mpi_comm_id>(aid_, cid);

  global_grp_ = new mpi_group(mpirun_np_);

  worldcomm_ = new mpi_comm(mpi_comm_id(MPI_COMM_WORLD), rank, global_grp_, env, aid_);

  std::vector<task_id> selfp;
  selfp.push_back(task_id(rank));

  self_grp_ = new mpi_group(selfp);
  selfcomm_ = new mpi_comm(mpi_comm_id(MPI_COMM_SELF), mpi_id(0), self_grp_, env, aid_);

  splittype_ = new mpi_type;
  splittype_->init_vector("split_type", mpi_type::mpi_int, 3, 1, 1, true,
                  MPI_COMBINER_CONTIGUOUS);

  parent_->allocate_type_id(splittype_);
}

void
mpi_comm_factory::finalize()
{
  if (worldcomm_) delete worldcomm_;
  if (selfcomm_) delete selfcomm_;
  if (splittype_) delete splittype_;
  if (global_grp_) delete global_grp_;
  if (self_grp_) delete self_grp_;
  worldcomm_ = 0;
  selfcomm_ = 0;
  splittype_ = 0;
  global_grp_ = 0;
  self_grp_ = 0;
}

//
// Duplicate a communicator.
//
mpi_comm*
mpi_comm_factory::comm_dup(mpi_comm*caller)
{
  mpi_comm* ret = this->comm_create(caller, caller->group_);

  ret->dup_keyvals(caller);
  return ret;
}

//
// Make the given mpiid refer to a newly created communicator.
//
mpi_comm*
mpi_comm_factory::comm_create(mpi_comm* caller, mpi_group* group)
{
  //first reduce so we know everyone is on board, which is the necessary barrier
  //payload::const_ptr load = valuepayload<int>::construct(next_id_.id_);
  payload::const_ptr load = new mpi_payload(&next_id_.id_,
                            mpi_type::mpi_int, 1, false);
  payload::const_ptr result;
  parent_->allreduce(1, mpi_type::mpi_int->id,
    mpi_op::max, caller, load, result);

  mpi_payload::const_ptr casted = ptr_safe_cast(const mpi_payload, result);
  int returndata;
  casted->extract(&returndata, sizeof(int), mpi_type::mpi_int);

  mpi_comm_id cid = mpi_comm_id(returndata);

  std::pair<app_id, mpi_comm_id> index = std::make_pair(aid_, cid);
  //std::make_pair<app_id, mpi_comm_id>(aid_, cid);

  //now find my rank
  mpi_id newrank = group->rank_of_task(caller->my_task());

  next_id_.id_ = cid.id_ + 1;

  if (newrank.id_ >= 0) {
    return new mpi_comm(cid, newrank, group, worldcomm_->env_, aid_);
  }
  else {
    return mpi_comm::comm_null;
  }

}

typedef std::map<int, std::list<int> > key_to_ranks_map;
#if !SSTMAC_DISTRIBUTED_MEMORY || SSTMAC_MMAP_COLLECTIVES

static thread_lock split_lock;
typedef std::map<int, key_to_ranks_map> color_to_key_map;
//comm id, comm root task id, tag

struct comm_split_entry {
  int* buf;
  int refcount;
  comm_split_entry() : buf(0), refcount(0) {}
};
static std::map<int, std::map<int, std::map<int, comm_split_entry> > > comm_split_entries;
#endif


//
// MPI_Comm_split.
//
mpi_comm*
mpi_comm_factory::comm_split(mpi_comm* caller, int my_color, int my_key)
{
  key_to_ranks_map key_map;
#if SSTMAC_DISTRIBUTED_MEMORY && !SSTMAC_MMAP_COLLECTIVES
  int mydata[3];
  mydata[0] = next_id_.id_;
  mydata[1] = my_color;
  mydata[2] = my_key;

  mpi_payload::const_ptr mine = new mpi_payload(mydata, splittype_, 1, false);
  std::vector<payload::const_ptr> result;
  //BUG the recv count is per process, JJW 07/21/2015
  parent_->run_allgather(1, splittype_->id, 1, //caller->size()
                     splittype_->id, caller,
                     mine, result);
  if (my_color < 0){ //I'm not part of this!
    return mpi_comm::comm_null;
  }

  
  bool extract_vector = result.size() == caller->size();
  mpi_payload::const_ptr single_big_load;
  if (!extract_vector){
    if (result.size() != 1){
      spkt_throw_printf(sprockit::value_error,
        "mpi_comm_split: MPI communicator has size %d, but result vector has size %d",
        int(caller->size()), result.size());
    }
    single_big_load = ptr_safe_cast(const mpi_payload, result[0]);
  }
#else
  split_lock.lock();
  int root = caller->peer_task(mpi_id(0));
  int tag = caller->next_collective_tag();
  comm_split_entry& entry = comm_split_entries[int(caller->id())][root][tag];
  char fname[256];
  size_t len = 3*caller->size()*sizeof(int);
  entry.refcount++;
  if (entry.buf == 0){
#if SSTMAC_MMAP_COLLECTIVES
    sprintf(fname, "%d.%d.%d", int(caller->id()), root, tag);
    int fd = shm_open(fname, O_RDWR | O_CREAT | O_EXCL, S_IRWXU);
    if (fd < 0){ //oops, someone else already created it
      fd = shm_open(fname, O_RDWR, S_IRWXU);
    }
    if (fd < 0){
      spkt_throw_printf(sprockit::value_error,
        "invalid fd %d shm_open on %s: error=%d",
        fd, fname, errno);
    }
    ftruncate(fd, len);
    void* bufptr = mmap(NULL, len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, 0);
    if (bufptr == ((void*)-1)){
      spkt_throw_printf(sprockit::value_error,
        "bad mmap on shm_open %s:%d: error=%d",
        fname, fd, errno);
    }
    entry.buf = (int*)bufptr;
#else
    entry.buf = new int[3*caller->size()];
#endif
  }
  split_lock.unlock();

  int* my_data = entry.buf + 3*caller->rank();
  my_data[0] = next_id_.id_;
  my_data[1] = my_color;
  my_data[2] = my_key;
  std::vector<payload::const_ptr> dummy;
  parent_->run_allgather(1, splittype_->id, 1, //caller->size()
                     splittype_->id, caller, payload::const_ptr(), dummy);
#endif

  if (my_color < 0){ //I'm not part of this!
    return mpi_comm::comm_null;
  }

  int cid = -1;
  int ninput_ranks = caller->size();
  int new_comm_size = 0;
  for (unsigned rank = 0; rank < ninput_ranks; rank++) {
#if SSTMAC_DISTRIBUTED_MEMORY && !SSTMAC_MMAP_COLLECTIVES
    int thisdata[3];
    if (extract_vector){
      mpi_payload::const_ptr thisload = ptr_safe_cast(const mpi_payload, result[rank]);
      thisload->extract(thisdata, splittype_->packed_size(), splittype_);
    } else {
      //extract at offset i
      single_big_load->extract(thisdata, splittype_->packed_size(), splittype_, rank);
    }
#else
    int* thisdata = entry.buf + 3*rank;
#endif

    int comm_id = thisdata[0];
    int color = thisdata[1];
    int key = thisdata[2];

    if (color >= 0 && color == my_color){
      key_map[key].push_back(rank);
      ++new_comm_size;
    }

    if (comm_id > cid) {
      cid = comm_id;
    }
  }


  //the next id I use needs to be greater than this
  next_id_.id_ = cid + 1;

  std::vector<task_id> task_list(new_comm_size);

  key_to_ranks_map::iterator it, end = key_map.end();
  //iterate map in sorted order
  int next_rank = 0;
  int my_new_rank = -1;
  for (it=key_map.begin(); it != end; ++it){
    std::list<int>& ranks = it->second;
    ranks.sort();
    std::list<int>::iterator rit, rend = ranks.end();
    for (rit=ranks.begin(); rit != rend; ++rit, ++next_rank){
      int comm_rank = *rit;
      task_id tid = caller->peer_task(mpi_id(comm_rank));
      task_list[next_rank] = tid;
      if (comm_rank == caller->rank()){
        my_new_rank = next_rank;
      }
    }
  }
#if !SSTMAC_DISTRIBUTED_MEMORY || SSTMAC_MMAP_COLLECTIVES
  entry.refcount--;
  if (entry.refcount == 0){
#if SSTMAC_MMAP_COLLECTIVES
    munmap(entry.buf, len);
    shm_unlink(fname);
#else
    delete[] entry.buf;
#endif
  }
#endif
  return new mpi_comm(mpi_comm_id(cid), mpi_id(my_new_rank),
                      new mpi_group(task_list), worldcomm_->env_, aid_);
}

mpi_comm*
mpi_comm_factory::create_cart(mpi_comm*caller, int ndims,
                              int *dims, int *periods, int reorder)
{
  //first reduce so we know everyone is on board, which is the necessary barrier
  payload::const_ptr load = new mpi_payload(&next_id_.id_,
                            mpi_type::mpi_int, 1, false);
  payload::const_ptr result;
  parent_->allreduce(1, mpi_type::mpi_int->id, mpi_op::max, caller, load, result);

  mpi_payload::const_ptr casted = ptr_safe_cast(const mpi_payload, result);
  int returndata;
  casted->extract(&returndata, sizeof(int), mpi_type::mpi_int);


  mpi_comm_id cid = mpi_comm_id(returndata);

  std::pair<app_id, mpi_comm_id> index = std::make_pair(aid_, cid);
  //std::make_pair<app_id, mpi_comm_id>(aid_, cid);

  //now find my rank
  mpi_id newrank = caller->group_->rank_of_task(caller->my_task());


  next_id_.id_ = cid.id_ + 1;

  if (newrank.id_ >= 0) {
    return new mpi_comm_cart(cid, newrank, caller->group_,
                     worldcomm_->env_, aid_, ndims, dims, periods, reorder);
  }
  else {
    return mpi_comm::comm_null;
  }
}

}
} // end of namespace sstmac

