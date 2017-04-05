#include <sstmac/software/launch/job_launcher.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/launch/launch_event.h>
#include <sstmac/software/launch/job_launch_event.h>
#include <sstmac/software/launch/launch_request.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/thread_lock.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::sw::job_launcher)

namespace sstmac {
namespace sw {

SpktRegister("default", job_launcher, default_job_launcher);
SpktRegister("exclusive", job_launcher, exclusive_job_launcher);

std::map<app_id, task_mapping::ptr> task_mapping::app_ids_launched_;
std::map<std::string, task_mapping::ptr> task_mapping::app_names_launched_;
std::map<int,app_id> task_mapping::local_refcounts_;

job_launcher::job_launcher(sprockit::sim_parameters* params,
                           operating_system* os) :
  service(std::string("job_launcher"), software_id(0,0,0), os)
{
  topology_ = sstmac::hw::topology::static_topology(params);
  int num_nodes = topology_->num_nodes();
  for (int i=0; i < num_nodes; ++i){
    available_.insert(i);
  }
  add_launch_requests(params);
}

void
job_launcher::incoming_event(event *ev)
{
  job_stop_event* stop_ev = safe_cast(job_stop_event, ev);
  cleanup_app(stop_ev);
  stop_event_received(stop_ev);
}

void
job_launcher::incoming_launch_request(app_launch_request *request)
{
  ordered_node_set allocation;
  bool startJob = handle_launch_request(request, allocation);
  if (startJob){
    satisfy_launch_request(request, allocation);
  }
}

void
job_launcher::schedule_launch_requests()
{
  for (app_launch_request* req : initial_requests_){
    os_->schedule(req->time(),
        new_callback(os_->event_location(), this, &job_launcher::incoming_launch_request, req));
  }
}

void
job_launcher::add_launch_requests(sprockit::sim_parameters* params)
{
  bool keep_going = true;
  int aid = 1;
  int last_used_aid = 0;
  while (keep_going || aid < 10){
    std::string name = sprockit::printf("app%d",aid);
    if (params->has_namespace(name)){
      sprockit::sim_parameters* app_params = params->get_namespace(name);
      app_launch_request* mgr = new app_launch_request(app_params, app_id(aid), name);
      initial_requests_.push_back(mgr);
      keep_going = true;
      last_used_aid = aid;
    } else {
      keep_going = false;
    }
    ++aid;
  }

  aid = last_used_aid+1;

  std::vector<std::string> services_to_launch;
  params->get_optional_vector_param("services", services_to_launch);
  for (std::string& str : services_to_launch){
    sprockit::sim_parameters* srv_params = params->get_namespace(str);
    //setup the name for app factory
    srv_params->add_param_override("name", "distributed_service");
    //setup the name for distributed service
    srv_params->add_param_override("libname", str);
    app_launch_request* mgr = new app_launch_request(srv_params, app_id(aid), str);
    node_debug("adding distributed service %s", str.c_str());
    initial_requests_.push_back(mgr);
    ++aid;
  }
}

device_id
job_launcher::event_location() const
{
  return os_->event_location();
}

void
job_launcher::cleanup_app(job_stop_event* ev)
{
  task_mapping::ptr themap = task_mapping::global_mapping(ev->aid());
  task_mapping::remove_global_mapping(ev->aid(), ev->unique_name());
  const std::vector<node_id>& rank_to_node = themap->rank_to_node();
  int num_ranks = rank_to_node.size();
  //put all the nodes back in the available map
  for (int i=0; i < num_ranks; ++i){
    node_id nid = rank_to_node[i];
    available_.insert(nid);
  }
}

void
job_launcher::satisfy_launch_request(app_launch_request* request, const ordered_node_set& allocation)
{
  task_mapping::ptr mapping = new task_mapping(request->aid());
  request->index_allocation(
     topology_, allocation,
     mapping->rank_to_node(),
     mapping->node_to_rank());

  int num_ranks = mapping->num_ranks();
  for (int rank=0; rank < num_ranks; ++rank){
    sw::start_app_event* lev = new start_app_event(request->aid(), request->app_namespace(),
                                    mapping, rank, mapping->rank_to_node(rank),
                                    os_->addr(),//the job launch root
                                    request->app_params());
    os_->execute_kernel(ami::COMM_PMI_SEND, lev);
    //os_->execute_kernel(ami::COMM_PMI_BCAST, lev);
  }
  //job launcher needs to add this - might need it later
  task_mapping::add_global_mapping(request->aid(), request->app_namespace(), mapping);
}

bool
default_job_launcher::handle_launch_request(app_launch_request* request, ordered_node_set& allocation)
{
  request->request_allocation(available_, allocation);

  for (const node_id& nid : allocation){
    if (available_.find(nid) == available_.end()){
      spkt_throw_printf(sprockit::value_error,
                        "allocation requested node %d, but it's not available",
                        int(nid));
    }
    available_.erase(nid);
  }
  return true;
}

void
default_job_launcher::stop_event_received(job_stop_event *ev)
{
}

bool
exclusive_job_launcher::handle_launch_request(app_launch_request *request, ordered_node_set& allocation)
{
  if (active_job_ == nullptr){
    active_job_ = request;
    return default_job_launcher::handle_launch_request(request, allocation);
  } else {
    pending_requests_.push_back(request);
    return false;
  }
}

void
exclusive_job_launcher::stop_event_received(job_stop_event *ev)
{
  delete active_job_;
  active_job_ = nullptr;
  if (!pending_requests_.empty()){
    app_launch_request* next = pending_requests_.front();
    pending_requests_.pop_front(); //remove the running job
    job_launcher::incoming_launch_request(next);
  }
}

task_mapping::ptr
task_mapping::global_mapping(app_id aid)
{
  auto iter = app_ids_launched_.find(aid);
  if (iter == app_ids_launched_.end()){
    spkt_abort_printf("cannot find global task mapping for %d", aid);
  }
  return iter->second;
}

task_mapping::ptr
task_mapping::global_mapping(const std::string& name)
{
  auto iter = app_names_launched_.find(name);
  if (iter == app_names_launched_.end()){
    spkt_abort_printf("cannot find global task mapping for %s", name.c_str());
  }
  return iter->second;
}

void
task_mapping::add_global_mapping(app_id aid, const std::string &unique_name, const task_mapping::ptr &mapping)
{
  app_ids_launched_[aid] = mapping;
  app_names_launched_[unique_name] = mapping;
  local_refcounts_[aid]++;
}

void
task_mapping::remove_global_mapping(app_id aid, const std::string& name)
{
  static thread_lock lock;
  lock.lock();
  auto iter = local_refcounts_.find(aid);
  int& refcount = iter->second;
  refcount--;
  if (refcount == 0){
    local_refcounts_.erase(iter);
    app_ids_launched_.erase(aid);
    app_names_launched_.erase(name);
  }
  lock.unlock();
}

}
}
