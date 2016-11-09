#ifndef REDUCE_H
#define REDUCE_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class wilke_reduce_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const override {
    return "virtual reduce actor";
  }

  void
  buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

  wilke_reduce_actor(int root, reduce_fxn fxn);

 private:
  bool is_lower_partner(int virtual_me, int partner_gap);
  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

 private:
  reduce_fxn fxn_;

  int root_;

  int num_reducing_rounds_;

  int num_total_rounds_;

};

class wilke_halving_reduce :
  public dag_collective
{
 public:
  std::string
  to_string() const override {
    return "sumi allreduce";
  }

  wilke_halving_reduce(int root, reduce_fxn fxn);

  wilke_halving_reduce() : root_(-1) {}

  virtual void
  init_reduce(reduce_fxn fxn) override{
    fxn_ = fxn;
  }

  dag_collective_actor*
  new_actor() const override {
    return new wilke_reduce_actor(root_, fxn_);
  }

  void init_root(int root) override {
    root_ = root;
  }

  dag_collective*
  clone() const override {
    return new wilke_halving_reduce(root_, fxn_);
  }

 private:
  reduce_fxn fxn_;

  int root_;

};


}


#endif // REDUCE_H
