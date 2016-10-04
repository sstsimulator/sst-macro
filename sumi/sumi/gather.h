#ifndef GATHER_H
#define GATHER_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class btree_gather_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const override {
    return "btree gather actor";
  }

  btree_gather_actor(int root) : root_(root) {}

 protected:
  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;
  void init_tree() override;
  void start_shuffle(action *ac) override;
  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac) override;

 private:
  int root_;
  int midpoint_;
  int log2nproc_;

};

class btree_gather :
  public dag_collective
{

 public:
  btree_gather(int root) : root_(root){}

  btree_gather() : root_(-1){}

  std::string
  to_string() const override {
    return "btree gather";
  }

  dag_collective_actor*
  new_actor() const override {
    return new btree_gather_actor(root_);
  }

  dag_collective*
  clone() const override {
    return new btree_gather(root_);
  }

  void init_root(int root) override{
    root_ = root;
  }

 private:
  int* recv_counts_;
  int root_;

};

}

#endif // GATHER_H
