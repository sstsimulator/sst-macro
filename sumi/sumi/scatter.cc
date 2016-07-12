#include <sumi/scatter.h>
#include <sumi/domain.h>
#include <sumi/transport.h>

namespace sumi {

void
btree_scatter_actor::init_tree()
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
btree_scatter_actor::init_buffers(void *dst, void *src)
{
  //check dst - everyone has dst, not everyone has a source
  if (!dst)
    return;

  int me = dom_->my_domain_rank();
  int nproc = dom_->nproc();
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
btree_scatter_actor::finalize_buffers()
{
  if (!result_buffer_.ptr)
    return;

  int nproc = dom_->nproc();
  int me = dom_->my_domain_rank();
  int result_size = nelems_*type_size_;
  int max_recv_buf_size = midpoint_*nelems_*type_size_;
  if (me == root_){
    int buf_size = nproc * nelems_ * type_size_;
    my_api_->unmake_public_buffer(send_buffer_, buf_size);
    if (root_ != 0){
      my_api_->unmake_public_buffer(result_buffer_,result_size);
      my_api_->free_public_buffer(recv_buffer_,max_recv_buf_size);
    }
  } else {
    if (me % 2 == 0){
      //I sent from a temp buffer, need a memcpy
      ::memcpy(result_buffer_.ptr, recv_buffer_.ptr, result_size);
    } else {
      //I am done
      my_api_->unmake_public_buffer(result_buffer_,result_size);
    }
    my_api_->free_public_buffer(recv_buffer_,max_recv_buf_size);
  }
}

void
btree_scatter_actor::buffer_action(void *dst_buffer, void *msg_buffer, action *ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
btree_scatter_actor::init_dag()
{
  int me = dom_->my_domain_rank();
  int nproc = dom_->nproc();
  int round = 0;

  action* prev = 0;

  if (me == root_){
    //send half my data to midpoint to begin the scatter
    action* ac = new send_action(round, midpoint_);
    ac->offset = nelems_ * midpoint_;
    ac->nelems = nelems_ * std::min(nproc-midpoint_, midpoint_);;
    add_initial_action(ac);
    prev = ac;
  }
  if (me == midpoint_){
    action* ac = new recv_action(round, root_);
    ac->offset = 0;
    ac->nelems = nelems_ * std::min(nproc-midpoint_, midpoint_);
    ac->recv_type_ = action::temp;
    add_initial_action(ac);
    prev = ac;
  }

  if (root_ != 0){
    //uh oh - need an extra send
    if (me == root_){
      action* ac = new send_action(round, 0);
      ac->nelems = nelems_ * midpoint_;
      ac->offset = 0;
      add_initial_action(ac);
    } else if (me == 0){
      action* ac = new recv_action(round, root_);
      ac->nelems = nelems_ * midpoint_;
      ac->offset = 0;
      ac->recv_type_ = action::temp;
      add_initial_action(ac);
      prev = ac;
    }
  }

  ++round;

  int partnerGap = midpoint_ / 2;
  while (partnerGap > 0){
    bool i_am_active = me % partnerGap == 0;
    //only a certain number of tasks are active
    if (i_am_active){
      int myRole = (me / partnerGap) % 2;
      if (myRole == 0){
        //I send up
        int partner = me + partnerGap;
        if (partner < nproc){
          action* ac = new send_action(round, partner);
          ac->offset = partnerGap * nelems_;
          ac->nelems = std::min(nproc-partner,partnerGap) * nelems_;
          add_dependency(prev, ac);
          prev = ac;
        }
      } else {
        //I recv down
        int partner = me - partnerGap;
        action* ac = new recv_action(round, partner);
        ac->offset = 0;
        ac->nelems = std::min(nproc-me,partnerGap) * nelems_;
        if (partnerGap != 1) ac->recv_type_ = action::temp; //not done - receive into temp buffer
        add_dependency(prev, ac);
        prev = ac;
      }
    }
    ++round;
    partnerGap /= 2;
  }

}


}

