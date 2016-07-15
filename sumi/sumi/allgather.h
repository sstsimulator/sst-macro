#ifndef sstmac_sw_api_simpsg_ALLGATHER_H
#define sstmac_sw_api_simpsg_ALLGATHER_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>


namespace sumi {

class bruck_allgather_actor :
  public bruck_actor
{

 public:
  std::string
  to_string() const {
    return "bruck allgather actor";
  }

 protected:
  void finalize();

  void finalize_buffers();
  void init_buffers(void *dst, void *src);
  void init_dag();

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac);


};

class bruck_allgather_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const {
    return "bruck allgather";
  }

  dag_collective_actor*
  new_actor() const {
    return new bruck_allgather_actor;
  }

  dag_collective*
  clone() const {
    return new bruck_allgather_collective;
  }

};

}

#endif // ALLGATHER_H
