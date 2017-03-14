#ifndef JOB_LAUNCHER_H
#define JOB_LAUNCHER_H

#include <sprockit/factories/factory.h>
#include <sprockit/unordered.h>
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

class task_mapping : public sprockit::ptr_type
{
 public:
  task_mapping(app_id aid) : aid_(aid) {}

  typedef sprockit::refcount_ptr<task_mapping> ptr;

  node_id rank_to_node(int rank) const {
    return rank_to_node_indexing_[rank];
  }

  const std::list<int>&
  node_to_ranks(int node) const {
    return node_to_rank_indexing_[node];
  }

  app_id aid() const {
    return aid_;
  }

  int
  num_ranks() const {
    return rank_to_node_indexing_.size();
  }

  int nproc() const {
    return rank_to_node_indexing_.size();
  }

  const std::vector<node_id>&
  rank_to_node() const {
    return rank_to_node_indexing_;
  }

  std::vector<node_id>&
  rank_to_node() {
    return rank_to_node_indexing_;
  }

  const std::vector<std::list<int>>& node_to_rank() const {
    return node_to_rank_indexing_;
  }

  std::vector<std::list<int>>& node_to_rank() {
    return node_to_rank_indexing_;
  }

  static task_mapping::ptr
  global_mapping(app_id aid);

  static task_mapping::ptr
  global_mapping(const std::string& unique_name);

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

  static std::map<app_id,int>  local_refcounts_;
  static std::map<app_id, task_mapping::ptr> app_ids_launched_;
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
 public:
  /**
   * @brief incoming_event Handle an event sent from one of the nodes
   * @param ev Must be a job_stop_event
   */
  void incoming_event(event *ev);

  void incoming_launch_request(app_launch_request* request);

  device_id event_location() const;

  virtual ~job_launcher(){}

 protected:
  job_launcher(sprockit::sim_parameters* params, operating_system* os);

 protected:
  /** A topology object for querying about the details of the system */
  hw::topology* topology_;
  /** The set of available nodes - equivalent to std::set<node_id> */
  ordered_node_set available_;

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
   *                submitted via qsub or salloc
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
  virtual bool
  handle_launch_request(app_launch_request* request, ordered_node_set& allocation) = 0;

  /**
   * @brief stop_event_received Perform all necessary operations upon completion
   *            of a job. This indicates that new resources are available
   *            for running other jobs that might be queued
   * @param ev  An event describing the job that has finished
   */
  virtual void
  stop_event_received(job_stop_event* ev) = 0;


};


DeclareFactory(job_launcher, operating_system*);

/**
 * @brief The default_job_launcher
 * Encapsulates a job launcher that ALWAYS tries to launch a job. It performs no queueing
 * or conflict resolution. If insufficient resources are available to launch a job,
 * the job launcher aborts and ends the simulation
 */
class default_job_launcher : public job_launcher
{
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

}
}



#endif // JOB_LAUNCHER_H

