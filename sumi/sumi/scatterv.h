#ifndef scatterv_H
#define scatterv_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class btree_scatterv_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const {
    return "btree scatterv actor";
  }

  btree_scatterv_actor(int root, int* send_counts) :
    root_(root), send_counts_(send_counts) {}

 protected:
  void finalize_buffers();
  void init_buffers(void *dst, void *src);
  void init_dag();
  void init_tree();

  void buffer_action(void *dst_buffer, void *msg_buffer, action* ac);

 private:
  int root_;
  int midpoint_;
  int log2nproc_;
  int* send_counts_;

};

class btree_scatterv :
  public dag_collective
{

 public:
  btree_scatterv(int root) :
    root_(root) {}

  btree_scatterv() : root_(-1){}

  std::string
  to_string() const {
    return "btree scatterv";
  }

  dag_collective_actor*
  new_actor() const {
    return new btree_scatterv_actor(root_, send_counts_);
  }

  dag_collective*
  clone() const {
    return new btree_scatterv(root_);
  }

  void init_root(int root){
    root_ = root;
  }

  void init_send_counts(int *nelems){
    send_counts_ = nelems;
  }

 private:
  int root_;
  int* send_counts_;

};

}

#endif // GATHER_H

