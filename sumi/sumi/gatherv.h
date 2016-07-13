#ifndef GATHERV_H
#define GATHERV_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class btree_gatherv_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const {
    return "btree gatherv actor";
  }

  btree_gatherv_actor(int root, int* recv_counts) :
    root_(root), recv_counts_(recv_counts) {}

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
  int* recv_counts_;

};

class btree_gatherv :
  public dag_collective
{

 public:
  btree_gatherv(int root) : root_(root){}

  btree_gatherv() : root_(-1){}

  std::string
  to_string() const {
    return "btree gatherv";
  }

  dag_collective_actor*
  new_actor() const {
    return new btree_gatherv_actor(root_, recv_counts_);
  }

  dag_collective*
  clone() const {
    return new btree_gatherv(root_);
  }

  void init_root(int root){
    root_ = root;
  }

  void init_recv_counts(int* counts){
    recv_counts_ = counts;
  }

 private:
  int* recv_counts_;
  int root_;

};

}

#endif // GATHER_H
