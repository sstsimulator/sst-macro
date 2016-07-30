#include <sumi/alltoallv.h>
#include <sumi/partner_timeout.h>
#include <sumi/transport.h>
#include <sumi/communicator.h>
#include <sprockit/output.h>
#include <cstring>

#define divide_by_2_round_up(x) \
  ((x/2) + (x%2))

#define divide_by_2_round_down(x) \
  (x/2)

using namespace sprockit::dbg;

#define SEND_SHUFFLE 0
#define RECV_SHUFFLE 1

#define MAX_UNROLL =

namespace sumi
{

SpktRegister("bruck_alltoall", dag_collective, direct_alltoallv_collective);

void
direct_alltoallv_actor::init_buffers(void* dst, void* src)
{
  if (src){
    total_send_size_ = 0;
    total_recv_size_ = 0;
    for (int i=0; i < dense_nproc_; ++i){
      total_send_size_ += send_counts_[i];
      total_recv_size_ += recv_counts_[i];
    }
    result_buffer_ = my_api_->make_public_buffer(dst, total_recv_size_);
    send_buffer_ = my_api_->make_public_buffer(src, total_send_size_);
    recv_buffer_ = result_buffer_;
  }
}

void
direct_alltoallv_actor::finalize_buffers()
{
  if (result_buffer_.ptr){
    my_api_->unmake_public_buffer(result_buffer_, total_recv_size_);
    my_api_->unmake_public_buffer(send_buffer_, total_send_size_);
  }
}

void
direct_alltoallv_actor::add_action(
  const std::vector<action*>& actions,
  int stride_direction,
  int num_initial,
  int stride)
{
  int partner = (dense_me_ + dense_nproc_ + stride*stride_direction) % dense_nproc_;
  action* ac = actions[partner];
  if (stride < num_initial){
    dag_collective_actor::add_action(ac);
  } else {
    int prev_partner = (partner + dense_nproc_ - num_initial*stride_direction) % dense_nproc_;
    action* prev = actions[prev_partner];
    add_dependency(prev, ac);
  }
}

void
direct_alltoallv_actor::init_dag()
{
  std::vector<action*> recvs(dense_nproc_);
  std::vector<action*> sends(dense_nproc_);

  recv_action::buf_type_t recv_ty = slicer_->contiguous() ?
        recv_action::in_place : recv_action::unpack_temp_buf;

  int send_offset = 0;
  int recv_offset = 0;
  int round = 0;
  for (int i=0; i < dense_nproc_; ++i){
    action* recv = new recv_action(round, i, recv_ty);
    action* send = new send_action(round, i, send_action::in_place);
    send->offset = send_offset;
    send->nelems = send_counts_[i];
    recv->offset = recv_offset;
    recv->nelems = recv_counts_[i];

    sends[i] = send;
    recvs[i] = recv;
  }

  int num_initial = 3;
  for (int i=0; i < dense_nproc_; ++i){
    //move down for recvs
    add_action(recvs, -1, num_initial, i);
    //move down for sends
    add_action(sends, 1, num_initial, i);
  }

}

void
direct_alltoallv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
direct_alltoallv_actor::finalize()
{
}


}

