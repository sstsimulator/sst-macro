#ifndef sstmac_sw_api_simpsg_ALLGATHERV_H
#define sstmac_sw_api_simpsg_ALLGATHERV_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class bruck_allgatherv_actor :
  public bruck_actor
{

 public:
  std::string
  to_string() const override {
    return "bruck allgatherv actor";
  }

  bruck_allgatherv_actor(int* recv_counts);

 protected:
  void finalize() override;

  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

  int nelems_to_recv(int partner, int partner_gap);

 private:
  int* recv_counts_;
  int total_nelems_;
  int my_offset_;

};

class bruck_allgatherv_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const override {
    return "bruck allgatherv";
  }

  dag_collective_actor*
  new_actor() const override {
    return new bruck_allgatherv_actor(recv_counts_);
  }

  dag_collective*
  clone() const override {
    return new bruck_allgatherv_collective;
  }

  void init_recv_counts(int* counts) override {
    recv_counts_ = counts;
  }

 private:
  int* recv_counts_;

};

}

#endif // ALLGATHER_H
