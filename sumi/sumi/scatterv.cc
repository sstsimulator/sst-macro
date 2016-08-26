#include <sumi/scatterv.h>
#include <sumi/communicator.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_scatterv_actor::init_tree()
{
  log2nproc_ = 0;
  midpoint_ = 1;
  int nproc = comm_->nproc();
  while (midpoint_ < nproc){
    midpoint_ *= 2;
    log2nproc_++;
  }
  //unrull one - we went too far
  midpoint_ /= 2;
}

void
btree_scatterv_actor::init_buffers(void *dst, void *src)
{
  //check dst - everyone has dst, not everyone has a source
  if (!dst)
    return;

  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int result_size = nelems_ * type_size_;
  int max_recv_buf_size = midpoint_*nelems_*type_size_;
  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    send_buffer_ = my_api_->make_public_buffer(src, buf_size);
    if (root_ != 0){
      recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
      result_buffer_ = my_api_->make_public_buffer(dst, result_size);
    } else {
      ::memcpy(dst, src, result_size);
      recv_buffer_ = result_buffer_; //won't ever actually be used
      result_buffer_.ptr = dst;
    }
  } else {
    recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
    send_buffer_ = recv_buffer_;
    if (me  % 2 == 1){ //I receive into my final buffer
      result_buffer_ = my_api_->make_public_buffer(dst, result_size);
    } else {
      result_buffer_.ptr = dst;
    }
  }
}

void
btree_scatterv_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

}

void
btree_scatterv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_scatterv_actor::init_dag()
{
  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int round = 0;

}


}

