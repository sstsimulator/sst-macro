#ifndef sstmac_sw_api_simpsg_alltoall_H
#define sstmac_sw_api_simpsg_alltoall_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

DeclareDebugSlot(sumi_alltoall)

namespace sumi {

class bruck_alltoall_actor :
  public bruck_actor
{

 public:
  std::string
  to_string() const {
    return "bruck all-to-all actor";
  }

 protected:
  void finalize();

  void finalize_buffers();
  void init_buffers(void *dst, void *src);
  void init_dag();

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac);

  void start_shuffle(action* ac);

  void shuffle(action *ac, void* tmpBuf, void* mainBuf, bool copyToTemp);

  int midpoint_;
};

class bruck_alltoall_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const {
    return "all-to-all";
  }

  dag_collective_actor*
  new_actor() const {
    return new bruck_alltoall_actor;
  }

  dag_collective*
  clone() const {
    return new bruck_alltoall_collective;
  }

};

}

#endif // ALLGATHER_H
