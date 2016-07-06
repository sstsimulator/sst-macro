#include <sumi/gather.h>
#include <sumi/domain.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_gather_actor::init_tree()
{
  log2nproc_ = 0;
  midpoint_ = 1;
  int nproc = dom_->nproc();
  while (midpoint_ < nproc){
    midpoint_ *= 2;
    log2nproc_++;
  }
  //unrull one - we went too far
  midpoint_ /= 2;
}

void
btree_gather_actor::init_buffers(void *dst, void *src)
{
  if (!src)
    return;

  int me = dom_->my_domain_rank();
  int nproc = dom_->nproc();

  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    result_buffer_ = my_api_->make_public_buffer(dst, buf_size);
    recv_buffer_ = result_buffer_;
    send_buffer_ = result_buffer_;
  } else {
    int max_recv_buf_size = midpoint_*nelems_*type_size_;
    recv_buffer_ = my_api_->allocate_public_buffer(max_recv_buf_size);
    send_buffer_ = recv_buffer_;
    result_buffer_ = recv_buffer_;
  }

  ::memcpy(recv_buffer_.ptr, src, nelems_*type_size_);
}

void
btree_gather_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

  int nproc = dom_->nproc();
  int me = dom_->my_domain_rank();
  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    my_api_->unmake_public_buffer(result_buffer_, buf_size);
  } else {
    int max_recv_buf_size = midpoint_*nelems_*type_size_;
    my_api_->free_public_buffer(recv_buffer_,max_recv_buf_size);
  }
}

void
btree_gather_actor::start_shuffle(action *ac)
{
  //only ever arises in weird midpoint scenarios
  int copy_size = ac->nelems * type_size_;
  int copy_offset = ac->offset * type_size_;
  char* dst = ((char*)result_buffer_.ptr) + copy_offset;
  char* src = ((char*)result_buffer_.ptr);
  ::memcpy(dst, src, copy_size);
}

void
btree_gather_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_gather_actor::init_dag()
{
  int me = dom_->my_domain_rank();
  int nproc = dom_->nproc();
  int round = 0;

  int maxGap = midpoint_;
  if (root_ != 0){
    //special case to handle the last gather round
    maxGap = midpoint_ / 2;
  }


  action* prev = 0;

  int partnerGap = 1;
  int stride = 2;
  while (1){
    if (partnerGap > maxGap) break;

    //just keep going until you stop
    if (me % stride == 0){
      //I am a recver
      int partner = me + partnerGap;
      if (partner < nproc){
        action* recv = new recv_action(round, partner);
        int recvChunkStart = me + partnerGap;
        int recvChunkStop = std::min(recvChunkStart+partnerGap, nproc);
        int recvChunkSize = recvChunkStop - recvChunkStart;
        recv->nelems = nelems_ * recvChunkSize;
        recv->offset = partnerGap * nelems_;  //I receive into top half of my buffer
        add_dependency(prev, recv);
        prev = recv;
      }
    } else {
      //I am a sender
      int partner = me - partnerGap;
      action* send = new send_action(round, partner);
      int sendChunkStart = me;
      int sendChunkStop = std::min(sendChunkStart+partnerGap,nproc);
      int sendChunkSize = sendChunkStop - sendChunkStart;
      send->nelems = nelems_*sendChunkSize;
      send->offset = 0; //I send my whole buffer
      add_dependency(prev, send);
      prev = send;

      break; //I am done, yo
    }
    ++round;
    partnerGap *= 2;
    stride *= 2;
  }

  round = log2nproc_;
  if (root_ != 0 && root_ == midpoint_ && me == root_){
    //I have to shuffle my data
    action* shuffle = new shuffle_action(round, me);
    shuffle->offset = midpoint_ * nelems_;
    shuffle->nelems = (nproc - midpoint_) * nelems_;
    if (prev){
      add_dependency(prev, shuffle);
    } else {
      add_initial_action(shuffle);
    }
    prev = shuffle;
  }


  if (root_ != 0){
    //the root must receive from 0 and midpoint
    if (me == root_){
      int size_1st_half = midpoint_;
      int size_2nd_half = nproc - midpoint_;
      //recv 1st half from 0
      action* recv = new recv_action(round, 0);
      recv->offset = 0;
      recv->nelems = nelems_ * size_1st_half;
      add_dependency(prev, recv);
      //recv 2nd half from midpoint - unless I am the midpoint
      if (midpoint_ != root_){
        recv = new recv_action(round, midpoint_);
        recv->offset = midpoint_*nelems_;
        recv->nelems = nelems_ * size_2nd_half;
        add_dependency(prev, recv);
      }
    }
    //0 must send the first half to the root
    if (me == 0){
      action* send = new send_action(round, root_);
      send->offset = 0; //send whole thing
      send->nelems = nelems_*midpoint_;
      add_dependency(prev,send);
    }
    //midpoint must send the second half to the root
    //unless it is the root
    if (me == midpoint_ && midpoint_ != root_){
      action* send = new send_action(round, root_);
      int size = nproc - midpoint_;
      send->offset = 0;
      send->nelems = nelems_ * size;
      add_dependency(prev,send);
    }
  }
}


}
