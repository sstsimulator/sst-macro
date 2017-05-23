/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef RUNTIME_H
#define RUNTIME_H

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sprockit/unordered.h>
#include <list>

namespace sstmac {

class deadlock_check {
 public:
  virtual void run() = 0;

  virtual ~deadlock_check(){}

};

template <class T, class Fxn> class deadlock_check_impl : public deadlock_check
{
 public:
  deadlock_check_impl(T* t , Fxn f) : t_(t), f_(f) {}

  void run(){
    (t_->*f_)();
  }

  virtual ~deadlock_check_impl(){}

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
  clear_statics();

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