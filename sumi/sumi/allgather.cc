#include <sumi/allgather.h>
#include <sumi/partner_timeout.h>
#include <sumi/transport.h>
#include <sumi/domain.h>
#include <sprockit/output.h>
#include <cstring>

#define divide_by_2_round_up(x) \
  ((x/2) + (x%2))

#define divide_by_2_round_down(x) \
  (x/2)

using namespace sprockit::dbg;

RegisterDebugSlot(sumi_allgather,
  "print all debug output associated with allgather collectives in the sumi framework");

namespace sumi
{

SpktRegister("bruck", dag_collective, bruck_collective);

void
bruck_actor::init_buffers(void* dst, void* src)
{
  if (src){
    //put everything into the dst buffer to begin
    std::memcpy(dst, src, nelems_ * type_size_);
    long buffer_size = nelems_ * type_size_ * dom_->nproc();
    send_buffer_ = my_api_->make_public_buffer(dst, buffer_size);
    recv_buffer_ = send_buffer_;
    result_buffer_ = send_buffer_;
  }
}

void
bruck_actor::finalize_buffers()
{
  long buffer_size = nelems_ * type_size_ * dom_->nproc();
  my_api_->unmake_public_buffer(send_buffer_, buffer_size);
}

void
bruck_actor::init_dag()
{
  int virtual_nproc = dense_nproc_;

  /** let's go bruck algorithm for now */
  int nproc = 1;
  int log2nproc = 0;
  while (nproc < virtual_nproc)
  {
    ++log2nproc;
    nproc *= 2;
  }

  int num_extra_procs = 0;
  if (nproc > virtual_nproc){
    --log2nproc;
    //we will have to do an extra exchange in the last round
    num_extra_procs = virtual_nproc - nproc / 2;
  }

  int num_rounds = log2nproc;
  int nprocs_extra_round = num_extra_procs;

  debug_printf(sumi_collective | sumi_allgather,
    "Bruck %s: configured for %d rounds with an extra round exchanging %d proc segments on tag=%d ",
    rank_str().c_str(), log2nproc, num_extra_procs, tag_);

  //in the last round, we send half of total data to nearest neighbor
  //in the penultimate round, we send 1/4 data to neighbor at distance=2
  //and so on...
  nproc = dense_nproc_;

  int partner_gap = 1;
  int round_nelems = nelems_;
  action *prev_send, *prev_recv;
  for (int i=0; i < num_rounds; ++i){
    int send_partner = (dense_me_ + nproc - partner_gap) % nproc;
    int recv_partner = (dense_me_ + partner_gap) % nproc;
    action* send_ac = new send_action(i, send_partner);
    action* recv_ac = new recv_action(i, recv_partner);
    send_ac->offset = 0;
    recv_ac->offset = round_nelems;
    send_ac->nelems = round_nelems;
    recv_ac->nelems = round_nelems;
    partner_gap *= 2;
    round_nelems *= 2;

    if (i == 0){
      add_initial_action(send_ac);
      add_initial_action(recv_ac);
    } else {
      add_dependency(prev_send, send_ac);
      add_dependency(prev_recv, send_ac);
      add_dependency(prev_send, recv_ac);
      add_dependency(prev_recv, recv_ac);
    }

    prev_send = send_ac;
    prev_recv = recv_ac;
  }

  if (nprocs_extra_round){
    int nelems_extra_round = nprocs_extra_round * nelems_;
    int send_partner = (dense_me_ + nproc - partner_gap) % nproc;
    int recv_partner = (dense_me_ + partner_gap) % nproc;
    action* send_ac = new send_action(num_rounds+1,send_partner);
    action* recv_ac = new recv_action(num_rounds+1,recv_partner);
    send_ac->offset = 0;
    recv_ac->offset = round_nelems;
    send_ac->nelems = nelems_extra_round;
    recv_ac->nelems = nelems_extra_round;

    add_dependency(prev_send, send_ac);
    add_dependency(prev_recv, send_ac);
    add_dependency(prev_send, recv_ac);
    add_dependency(prev_recv, recv_ac);
  }
}

void
bruck_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
bruck_actor::finalize()
{
  // rank 0 need not reorder
  // or no buffers
  if (dense_me_ == 0 || result_buffer_ == 0){
    return;
  }

  //we need to reorder things a bit
  //first, copy everything out
  int nproc = dense_nproc_;
  int total_size = nelems_ * type_size_ * nproc;
  char* tmp = new char[total_size];
  std::memcpy(tmp, result_buffer_, total_size);

  //int* buf = (int*) tmp;
  //for (int i=0; i < nelems_ * nproc; ++i){
  //  printf("Start[%d] = %d\n", i, buf[i]);
  //}

  for (int rank=0; rank < nproc; ++rank){
    //this rank is shifted to the wrong position
    int src_rank_offset = (rank + nproc - dense_me_) % nproc;
    int src_buffer_offset = src_rank_offset * nelems_ * type_size_;
    int dst_buffer_offset = rank * nelems_ * type_size_;
    void* src = tmp + src_buffer_offset;
    void* dst = ((char*)result_buffer_) + dst_buffer_offset;
    std::memcpy(dst, src, nelems_ * type_size_);
  }

  delete[] tmp;
}


}

