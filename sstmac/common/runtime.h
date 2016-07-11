#ifndef RUNTIME_H
#define RUNTIME_H

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/launch/app_launch_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sprockit/unordered.h>
#include <list>

namespace sstmac {

class deadlock_check {
 public:
  virtual void run() = 0;
};

template <class T, class Fxn> class deadlock_check_impl : public deadlock_check
{
 public:
  deadlock_check_impl(T* t , Fxn f) : t_(t), f_(f) {}

  void run(){
    (t_->*f_)();
  }

 private:
  T* t_;
  Fxn f_;
};

template <class T, class Fxn>
deadlock_check*
new_deadlock_check(T* t, Fxn f){
  return new deadlock_check_impl<T,Fxn>(t,f);
}

class runtime
{
 protected:
  typedef spkt_unordered_map<sw::task_id, node_id> task_to_nodeid_map;
  typedef spkt_unordered_map<sw::app_id, task_to_nodeid_map> app_to_task_map;

 public:
  static node_id
  node_for_task(sw::app_id aid, sw::task_id tid);

  static hw::topology*
  current_topology() {
    return topology_;
  }

  static void
  set_topology(hw::topology* top) {
    topology_ = top;
  }

  static void
  delete_statics();

  static node_id
  current_node();

  static sw::job_launcher*
  launcher() {
    return launcher_;
  }

  static void
  finish();

  static void enter_deadlock_region(){
    do_deadlock_check_ = true;
  }

  static void exit_deadlock_region(){
    do_deadlock_check_ = false;
  }

  static void check_deadlock();

  static void add_deadlock_check(deadlock_check* c){
    deadlock_checks_.push_back(c);
  }

  static void
  set_job_launcher(sw::job_launcher* launcher){
    launcher_ = launcher;
  }

 protected:
  static bool do_deadlock_check_;

  static hw::topology* topology_;

  static sw::job_launcher* launcher_;

  static std::list<deadlock_check*> deadlock_checks_;

};

}

#endif // RUNTIME_H

