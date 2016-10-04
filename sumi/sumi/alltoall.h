#ifndef alltoall_H
#define alltoall_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class bruck_alltoall_actor :
  public bruck_actor
{

 public:
  std::string
  to_string() const override {
    return "bruck all-to-all actor";
  }

 protected:
  void finalize() override;

  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

  void start_shuffle(action* ac) override;

  void shuffle(action *ac, void* tmpBuf, void* mainBuf, bool copyToTemp);

  int midpoint_;
};

class bruck_alltoall_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const override {
    return "all-to-all";
  }

  dag_collective_actor*
  new_actor() const override {
    return new bruck_alltoall_actor;
  }

  dag_collective*
  clone() const override {
    return new bruck_alltoall_collective;
  }

};

}

#endif // ALLGATHER_H
