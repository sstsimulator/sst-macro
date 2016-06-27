#ifndef APPMANAGER_H
#define APPMANAGER_H

#include <sstmac/software/process/task_id.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/common/node_address.h>
#include <sprockit/unordered.h>

#include <sprockit/factories/factory.h>

#include <sstmac/software/process/app_fwd.h>
#include <sstmac/software/launch/launch_info_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>

#include <vector>

#if SSTMAC_INTEGRATED_SST_CORE
#define STATIC_INIT_APP_MANAGER(params) \
{ \
  sstmac::sw::app_manager* mgr = sstmac::sw::app_manager::static_app_manager(params); \
  set_app_manager(mgr); \
}

#else
#define STATIC_INIT_APP_MANAGER(params)
#endif

namespace sstmac {
namespace sw {

class app_manager :
  public sprockit::factory_type
{

 public:

  virtual ~app_manager();

  virtual int
  nproc() const = 0;

  virtual node_id
  node_assignment(int rank) const = 0;

  virtual const std::list<int>&
  rank_assignment(node_id nid) const = 0;

  virtual void
  init_param1(sw::app_id aid) {
    aid_ = aid;
  }

  virtual void
  init_param2(parallel_runtime* rt){
    rt_ = rt;
  }

  virtual void
  set_interconnect(hw::interconnect* interconn);

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  hw::topology*
  topol() const {
    return top_;
  }

  hw::interconnect*
  interconn() const {
    return interconn_;
  }

  hw::node*
  node_at(node_id nid) const;

  virtual void
  allocate_and_index_jobs() = 0;

  virtual sw::launch_info*
  launch_info() const = 0;

  void
  add_hostname(sw::task_id tid, const std::string& name, node_id addr);

  void
  setenv(const std::string& name, const std::string& value) {
    shell_env_[name] = value;
  }

  std::string
  getenv(const std::string& name) const;

  std::string
  hostname(sw::task_id tid) const;

  node_id
  node_address(const std::string& hostname) const;

  node_id
  node_for_task(sw::task_id tid) const;

  void
  set_node_address(sw::task_id tid, node_id n) {
    nodeids_[tid] = n;
  }

  static app_manager*
  static_app_manager(int aid, sprockit::sim_parameters* params);

 protected:
  typedef spkt_unordered_map<sw::task_id, node_id> task_to_node_map;
  typedef spkt_unordered_map<sw::task_id, std::string> task_to_hostname_map;
  typedef spkt_unordered_map<std::string, std::string> shell_var_map;
  typedef spkt_unordered_map<std::string, node_id> hostname_to_addr_map;

  parallel_runtime* rt_;

  hw::topology* top_;

  hw::interconnect* interconn_;

  sw::app* app_template_;

  sw::app_id aid_;

  std::string appname_;

  std::vector<int> core_affinities_;

  task_to_node_map nodeids_;

  task_to_hostname_map hostnames_;

  hostname_to_addr_map inet_addrs_;

  shell_var_map shell_env_;

  app_manager();

 private:
  static std::map<int, app_manager*> static_app_managers_;
};


DeclareFactory2InitParams(app_manager, sw::app_id, parallel_runtime*);

}
}



#endif // APPMANAGER_H

