/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifndef sstmac_software_process_JOB_LAUNCHER_H
#define sstmac_software_process_JOB_LAUNCHER_H

#include <sprockit/factories/factory.h>
#include <unordered_map>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/event_scheduler.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/software/launch/node_set.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/launch/launch_request_fwd.h>
#include <sstmac/software/launch/launch_event_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <memory>

namespace sstmac {
namespace sw {

struct job_allocation
{
  timestamp requested; //time job was requested to start
  timestamp start; //time job actually started
  timestamp estimated; //time job is estimated to take
  ordered_node_set nodes;
  int nproc_launched;
  int nproc_completed;
};

class task_mapping {
 public:
  task_mapping(app_id aid) : aid_(aid) {}

  typedef std::shared_ptr<task_mapping> ptr;

  node_id rank_to_node(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::list<int>& node_to_ranks(int node) const {
    return node_to_rank_indexing_[node];
  }

  app_id aid() const {
    return aid_;
  }

  static task_mapping::ptr serialize_order(app_id aid, serializer& ser);

  int num_ranks() const {
    return rank_to_node_indexing_.size();
  }

  int nproc() const {
    return rank_to_node_indexing_.size();
  }

  const std::vector<node_id>& rank_to_node() const {
    return rank_to_node_indexing_;
  }

  std::vector<node_id>& rank_to_node() {
    return rank_to_node_indexing_;
  }

  const std::vector<std::list<int>>& node_to_rank() const {
    return node_to_rank_indexing_;
  }

  std::vector<std::list<int>>& node_to_rank() {
    return node_to_rank_indexing_;
  }

  static const task_mapping::ptr& global_mapping(app_id aid);

  static task_mapping::ptr global_mapping(const std::string& unique_name);

  static void
  add_global_mapping(app_id aid, const std::string& unique_name,
                     const task_mapping::ptr& mapping);

  static void
  remove_global_mapping(app_id aid, const std::string& name);

 private:
  app_id aid_;
  std::vector<node_id> rank_to_node_indexing_;
  std::vector<std::list<int> > node_to_rank_indexing_;
  std::vector<int> core_affinities_;

  static std::vector<int>  local_refcounts_;
  static std::vector<task_mapping::ptr> app_ids_launched_;
  static std::map<std::string, task_mapping::ptr> app_names_launched_;

  static void delete_statics();
};

/**
 * @brief The job_launcher class performs the combined operations a queue scheduler like PBS or MOAB
 * and a job launcher like SLURM (srun) or ALPS (aprun).
 * The job launcher allocates nodes to each requested MPI job (or other application).
 * Once nodes are allocated, the job_launcher has to assign MPI ranks to each node (mapping or indexing).
 * Each application can request a specific allocation or indexing.
 * However, it is ultimately the responsibility of the job launcher to decide on the final
 * allocation/indexing. In most cases, the job_launcher will honor exactly each applications's request
 * unless there is a conflict - in which case the job_launcher must arbitrate conflicting requests.
 */
class job_launcher : public service
{
  DeclareFactory(job_launcher, operating_system*)
 public:
  /**
   * @brief incoming_event Handle an event sent from one of the nodes
   * @param ev Must be a job_stop_event
   */
  void incoming_event(event *ev);

  void incoming_launch_request(app_launch_request* request);

  void schedule_launch_requests();

  uint32_t component_id() const;

  virtual ~job_launcher(){}

 protected:
  job_launcher(sprockit::sim_parameters* params, operating_system* os);

 protected:
  /** A topology object for querying about the details of the system */
  hw::topology* topology_;
  /** The set of available nodes - equivalent to std::set<node_id> */
  ordered_node_set available_;
  std::list<app_launch_request*> initial_requests_;

 private:
  void add_launch_requests(sprockit::sim_parameters* params);

  /**
   * @brief cleanup_app Perform all operations to free up resources associated with a job
   * @param ev
   */
  void cleanup_app(job_stop_event* ev);

  /**
   * @brief satisfy_launch_request Called by subclasses to cause a job to be launched
   *                This sends out launch messages to all the nodes involved, which will
   *                cause an MPI or other application to launc with the correct rank ID
   * @param request An object representing a user request to launch a job as if it were
   *                submitted via qsub or salloc. This transfers ownership of the request
   *                to the job launcher. The request should not be used again after this.
   */
  void satisfy_launch_request(app_launch_request* request, const ordered_node_set& allocation);

  /**
   * @brief handle_new_launch_request As if a new job had been submitted with qsub or salloc.
   * The job_launcher receives a new request to launch an application, at which point
   * it can choose to launch the application immediately if node allocation succeeds.
   * @param request  An object specifying all the details (indexing, allocation, application type)
   *                of the application being launched
   * @param allocation [INOUT] The set of nodes allocated (but not yet indexed) for running
   *                           an application. This must fill out the allocation.
   * @return Whether the allocation succeeded. True means job can launch immediately.
   *         False means job must be delayed until another job finishes.
   */
  virtual bool handle_launch_request(app_launch_request* request,
                                     ordered_node_set& allocation) = 0;

  /**
   * @brief stop_event_received Perform all necessary operations upon completion
   *            of a job. This indicates that new resources are available
   *            for running other jobs that might be queued
   * @param ev  An event describing the job that has finished
   */
  virtual void stop_event_received(job_stop_event* ev) = 0;


};

/**
 * @brief The default_job_launcher
 * Encapsulates a job launcher that ALWAYS tries to launch a job. It performs no queueing
 * or conflict resolution. If insufficient resources are available to launch a job,
 * the job launcher aborts and ends the simulation
 */
class default_job_launcher : public job_launcher
{
  FactoryRegister("default", job_launcher, default_job_launcher)
 public:
  default_job_launcher(sprockit::sim_parameters* params, operating_system* os) :
    job_launcher(params, os)
  {
  }

 private:
  void stop_event_received(job_stop_event* ev);

 protected:
  bool handle_launch_request(app_launch_request* request, ordered_node_set& allocation);

};

/**
 * @brief The exclusive_job_launcher class
 * A job launcher that only allows a single job on the system at one time.
 * Even if two jobs only use part of the system and could run simultaneously,
 * only one job is allowed at a time.
 */
class exclusive_job_launcher : public default_job_launcher
{
  FactoryRegister("exclusive", job_launcher, exclusive_job_launcher)
 public:
  exclusive_job_launcher(sprockit::sim_parameters* params, operating_system* os) :
   default_job_launcher(params, os), active_job_(nullptr)
  {
  }

 private:
  bool handle_launch_request(app_launch_request* request, ordered_node_set& allocation);

  void stop_event_received(job_stop_event* ev);

  std::list<app_launch_request*> pending_requests_;
  app_launch_request* active_job_;

};

struct outcast_iterator {

  outcast_iterator(int my_rank, int nranks) : my_rank_(my_rank), nranks_(nranks){}

  int forward_to(int ranks[]){
    int power2_size = 1;
    while (power2_size < nranks_){
      power2_size *= 2;
    }
    int ret = 0;
    pvt_forward_to(ret, ranks, 0, power2_size);
    return ret;
  }

  int nranks_;
  int my_rank_;

 private:
  void pvt_forward_to(int& num_ranks, int ranks[], int offset, int blocksize){
    if (blocksize == 1) return;

    int half_size = blocksize / 2;
    int role = my_rank_ % blocksize;
    int midpoint = offset + half_size;
    if (role == 0){
      int dst = midpoint;
      if (dst < nranks_) ranks[num_ranks++] = dst;
    }
    if (my_rank_ >= midpoint){
      pvt_forward_to(num_ranks, ranks, midpoint, half_size);
    } else {
      pvt_forward_to(num_ranks, ranks, offset, half_size);
    }
  }

};

struct incast_iterator {

  incast_iterator(int my_rank, int nranks) : my_rank_(my_rank), nranks_(nranks){}

  /**
   * @brief config
   * @param to_send
   * @param to_recv
   * @return
   */
  void config(int& num_to_send, int& num_to_recv, int to_send[], int to_recv[]){
    int power2_size = 1;
    while (power2_size < nranks_){
      power2_size *= 2;
    }
    int ret = 0;
    num_to_send = 0;
    num_to_recv = 0;
    pvt_config(num_to_send, num_to_recv, to_send, to_recv, 0, power2_size);
  }

  int nranks_;
  int my_rank_;

 private:
  void pvt_config(int& num_send, int& num_recv, int to_send[], int to_recv[], int offset, int blocksize){
    if (blocksize == 1) return;

    int half_size = blocksize / 2;
    int role = my_rank_ % blocksize;
    int midpoint = offset + half_size;
    if (role == 0){
      int dst = midpoint;
      if (dst < nranks_) to_recv[num_send++] = dst;
    } else if (role == half_size) {
      to_send[num_send++] = offset;
    }
    if (my_rank_ >= midpoint){
      pvt_config(num_send, num_recv, to_send, to_recv, midpoint, half_size);
    } else {
      pvt_config(num_send, num_recv, to_send, to_recv, offset, half_size);
    }
  }

};



}
}



#endif // JOB_LAUNCHER_H
