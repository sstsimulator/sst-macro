#include <sumi/gatherv.h>
#include <sumi/communicator.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_gatherv_actor::init_tree()
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
btree_gatherv_actor::init_buffers(void *dst, void *src)
{
  if (!src)
    return;

}

void
btree_gatherv_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

}

void
btree_gatherv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_gatherv_actor::init_dag()
{
  int me = comm_->my_comm_rank();
  int nproc = comm_->nproc();
  int round = 0;

  int maxGap = midpoint_;
  if (root_ != 0){
    //special case to handle the last gather round
    maxGap = midpoint_ / 2;
  }
}


}
