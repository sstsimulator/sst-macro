#ifndef sumi_api_COLLECTIVE_H
#define sumi_api_COLLECTIVE_H

#include <sumi/timeout.h>
#include <sumi/collective_message_fwd.h>
#include <sumi/transport_fwd.h>
#include <sumi/communicator_fwd.h>
#include <sumi/collective_actor_fwd.h>
#include <sumi/comm_functions.h>
#include <sprockit/factories/factory.h>
#include <sprockit/debug.h>

DeclareDebugSlot(sumi_collective)
DeclareDebugSlot(sumi_vote)
DeclareDebugSlot(sumi_collective_sendrecv)
DeclareDebugSlot(sumi_collective_init)
DeclareDebugSlot(sumi_collective_round)

namespace sumi
{

class collective
{
 public:
  typedef enum {
    alltoall,
    alltoallv,
    allreduce,
    allgather,
    allgatherv,
    bcast,
    barrier,
    gather,
    gatherv,
    reduce,
    reduce_scatter,
    scan,
    scatter,
    scatterv,
    dynamic_tree_vote,
    heartbeat
  } type_t;

  virtual ~collective();

  /**
   * @brief persistent
   * Some collectives are not allowed to "exit" based on the protocol
   * They have to remain active and persistent even after receving a completion ack
   * @return Whether the collective is persistent
   */
  virtual bool
  persistent() const {
    return false;
  }

  int
  context() const {
    return context_;
  }

  static const char*
  tostr(type_t ty);

  virtual void
  recv(int target, const collective_work_message_ptr& msg) = 0;

  void
  recv(const collective_work_message_ptr &msg);

  virtual void
  start() = 0;

  communicator*
  comm() const {
    return comm_;
  }

  bool
  complete() const {
    return complete_;
  }

  void
  set_complete() {
    complete_ = true;
  }

  int
  tag() const {
    return tag_;
  }

  type_t
  type() const {
    return type_;
  }

  void
  actor_done(int comm_rank, bool& generate_cq_msg, bool& delete_event);

  virtual void
  add_actors(collective* coll);

  static const int default_nproc = -1;

  virtual void
  deadlock_check(){}

  void
  init(type_t type, transport* api, communicator* comm, int tag, int context);

  virtual void init_actors(){}

 protected:
  collective(type_t type, transport* api, communicator* comm, int tag, int context);

  collective(){} //to be initialized later

 protected:
  transport* my_api_;
  communicator* comm_;
  int dense_me_;
  int dense_nproc_;
  int context_;
  bool complete_;
  int tag_;

  std::map<int, int> refcounts_;
  collective::type_t type_;

};

class dag_collective :
  public collective,
  public sprockit::factory_type
{
 public:
  void
  recv(int target, const collective_work_message_ptr& msg);

  void
  start();

  void
  init(type_t type,
    transport *my_api, communicator *comm,
    void *dst, void *src,
    int nelems, int type_size,
    int tag,
    bool fault_aware, int context);

  void init_actors();

  virtual dag_collective*
  clone() const = 0;

  virtual void
  init_reduce(reduce_fxn fxn){}

  virtual void
  init_root(int root){}

  virtual void init_recv_counts(int* nelems){}

  virtual void init_send_counts(int* nelems){}

  void deadlock_check();

  virtual ~dag_collective();

  static dag_collective*
  construct(const std::string& name, sprockit::sim_parameters* params, reduce_fxn fxn);

  static dag_collective*
  construct(const std::string& name, sprockit::sim_parameters *params);

 protected:
  virtual dag_collective_actor*
  new_actor() const = 0;

  void add_actors(collective *coll);

 protected:
  typedef std::map<int, dag_collective_actor*> actor_map;
  actor_map my_actors_;

  void* src_buffer_;

  void* dst_buffer_;

  int nelems_;

  int type_size_;

  // The bruck algorithm can actually apply to multiple collectives
  // Mainly barrier and allgather


  bool fault_aware_;

  std::list<collective_work_message_ptr> pending_;
};

class collective_algorithm_selector
{
 public:
  virtual dag_collective* select(int nproc, int nelems) = 0;
  virtual dag_collective* select(int nproc, int* counts) = 0;
};

DeclareFactory(dag_collective);


}

#endif // COLLECTIVE_H
