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
  to_string() const override {
    return "bruck allgather actor";
  }

 protected:
  void finalize() override;

  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;


};

class bruck_allgather_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const override {
    return "bruck allgather";
  }

  dag_collective_actor*
  new_actor() const override {
    return new bruck_allgather_actor;
  }

  dag_collective*
  clone() const override {
    return new bruck_allgather_collective;
  }

};

}

#endif // ALLGATHER_H
