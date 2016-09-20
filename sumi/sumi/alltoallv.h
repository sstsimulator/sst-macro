#ifndef ALLTOALLV_H
#define ALLTOALLV_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class direct_alltoallv_actor :
  public bruck_actor
{

 public:
  direct_alltoallv_actor(int* send_counts, int* recv_counts) :
    send_counts_(send_counts), recv_counts_(recv_counts) {}

  std::string
  to_string() const override {
    return "bruck all-to-allv actor";
  }

 protected:
  void finalize() override;

  void finalize_buffers() override;

  void init_buffers(void *dst, void *src) override;

  void init_dag() override;

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

 private:
  void add_action(
    const std::vector<action*>& actions,
    int stride_direction,
    int num_initial,
    int stride);

  int midpoint_;
  int* send_counts_;
  int* recv_counts_;
  int total_recv_size_;
  int total_send_size_;
};

class direct_alltoallv_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const override {
    return "all-to-all";
  }

  dag_collective_actor*
  new_actor() const override {
    return new direct_alltoallv_actor(send_counts_, recv_counts_);
  }

  dag_collective*
  clone() const override {
    return new direct_alltoallv_collective;
  }

  void init_send_counts(int* nelems) override {
    send_counts_ = nelems;
  }

  void init_recv_counts(int* nelems) override {
    recv_counts_ = nelems;
  }

 private:
  int* send_counts_;
  int* recv_counts_;

};

}

#endif // ALLGATHER_H
