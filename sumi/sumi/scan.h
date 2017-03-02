#ifndef sumi_scan_h
#define sumi_scan_h

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class simultaneous_btree_scan_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const override {
    return "simultaneous btree scan";
  }

  void
  buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

  simultaneous_btree_scan_actor(int root, reduce_fxn fxn);

 private:
  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;
  void start_shuffle(action* ac) override;

 private:
  reduce_fxn fxn_;

  int root_;

  int num_reducing_rounds_;

  int num_total_rounds_;

};

class simultaneous_btree_scan :
  public dag_collective
{
 public:
  std::string
  to_string() const override {
    return "simultaneous btree scan";
  }

  simultaneous_btree_scan(int root, reduce_fxn fxn);

  simultaneous_btree_scan() : root_(-1) {}

  virtual void
  init_reduce(reduce_fxn fxn) override{
    fxn_ = fxn;
  }

  dag_collective_actor*
  new_actor() const override {
    return new simultaneous_btree_scan_actor(root_, fxn_);
  }

  void init_root(int root) override {
    root_ = root;
  }

  dag_collective*
  clone() const override {
    return new simultaneous_btree_scan(root_, fxn_);
  }

 private:
  reduce_fxn fxn_;

  int root_;

};


}


#endif
