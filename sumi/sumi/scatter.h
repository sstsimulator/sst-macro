#ifndef scatter_H
#define scatter_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class btree_scatter_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const {
    return "btree scatter actor";
  }

  btree_scatter_actor(int root) : root_(root) {}

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

};

class btree_scatter :
  public dag_collective
{

 public:
  btree_scatter(int root) : root_(root){}

  btree_scatter() : root_(-1){}

  std::string
  to_string() const {
    return "btree scatter";
  }

  dag_collective_actor*
  new_actor() const {
    return new btree_scatter_actor(root_);
  }

  dag_collective*
  clone() const {
    return new btree_scatter(root_);
  }

  void init_root(int root){
    root_ = root;
  }

 private:
  int root_;

};

}

#endif // GATHER_H

