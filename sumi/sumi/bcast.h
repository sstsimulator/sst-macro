#ifndef sumi_bcast_included_h
#define sumi_bcast_included_h

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

DeclareDebugSlot(sumi_bcast)

namespace sumi {

class binary_tree_bcast_actor :
  public dag_collective_actor
{
 public:
  std::string
  to_string() const {
    return "bcast actor";
  }

  ~binary_tree_bcast_actor(){}

  binary_tree_bcast_actor(int root) : root_(root) {}

 private:
  void finalize_buffers();
  void init_buffers(void *dst, void *src);
  void init_dag();

  void init_root(int me, int roundNproc, int nproc);
  void init_child(int me, int roundNproc, int nproc);
  void init_internal(int me, int windowSize, int windowStop, action* recv);
  void buffer_action(void *dst_buffer, void *msg_buffer, action *ac);

  int root_;
};

class binary_tree_bcast_collective :
  public dag_collective
{

 public:
  std::string
  to_string() const {
    return "bcast";
  }

  dag_collective_actor*
  new_actor() const {
    return new binary_tree_bcast_actor(root_);
  }

  dag_collective*
  clone() const {
    return new binary_tree_bcast_collective(root_);
  }

  binary_tree_bcast_collective() : root_(-1) {}

  void init_root(int root){
    root_ = root;
  }

 private:
  binary_tree_bcast_collective(int root) : root_(root) {}

  int root_;

};

}

#endif // BCAST_H
