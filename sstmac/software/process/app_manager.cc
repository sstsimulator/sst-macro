#include <sstmac/common/runtime.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/app_manager.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::sw::app_manager);
RegisterNamespaces("app_manager");

namespace sstmac {
namespace sw {

std::map<int, app_manager*> app_manager::static_app_managers_;

app_manager::app_manager()
{
}

app_manager::~app_manager()
{
  delete app_template_;
}

hw::node*
app_manager::node_at(node_id nid) const
{
  return interconn_->node_at(nid);
}

void
app_manager::set_interconnect(hw::interconnect* interconn)
{
  interconn_ = interconn;
  top_ = interconn->topol();
}

void
app_manager::init_factory_params(sprockit::sim_parameters* params)
{
  launch_prefix_ = sprockit::printf("launch_app%d", int(aid_));
  appname_ = params->get_param(launch_prefix_);

  if (params->has_param(launch_prefix_ + "_core_affinities")) {
    params->get_vector_param(launch_prefix_ + "_core_affinities",
                             core_affinities_);
  }
  app_template_ = sw::app_factory::get_value(appname_, params);
  app_template_->init_argv(aid_, params);

  STATIC_INIT_INTERCONNECT(params)
}

app_manager*
app_manager::static_app_manager(int aid, sprockit::sim_parameters* params)
{
  if (!static_app_managers_[aid]){
    sprockit::sim_parameters* app_params = params->top_parent()->get_namespace("app_manager");
    std::string param_name = sprockit::printf("launch_app%d_type", aid);
    app_manager* mgr = app_manager_factory::get_optional_param(
      param_name, "skeleton", app_params, app_id(aid), 0/*no parallel runtime*/);
    static_app_managers_[aid] = mgr;
    sstmac_runtime::register_app_manager(app_id(aid), mgr);
    mgr->allocate_and_index_jobs();
  }
  return static_app_managers_[aid];
}

std::string
app_manager::getenv(const std::string &name) const
{
  shell_var_map::const_iterator it = shell_env_.find(name);
  if (it == shell_env_.end()) {
    return "";
  }
  return it->second;
}

std::string
app_manager::hostname(sw::task_id tid) const
{
  task_to_hostname_map::const_iterator it = hostnames_.find(tid);
  if (it == hostnames_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "environment::get_hostname: unable to find hostname for task %d",
                     int(tid));
  }
  return it->second;
}

void
app_manager::add_hostname(sw::task_id tid, const std::string &name,
                          node_id addr)
{
  hostnames_[tid] = name;
  inet_addrs_[name] = addr;
}

node_id
app_manager::node_address(const std::string& name) const
{
  hostname_to_addr_map::const_iterator it = inet_addrs_.find(name);
  if (it == inet_addrs_.end()) {
    spkt_throw_printf(
      sprockit::value_error,
      "unable to find nodaddress for hostname %s",
      name.c_str());
  }
  return it->second;
}

node_id
app_manager::node_for_task(sw::task_id tid) const
{
  task_to_node_map::const_iterator it = nodeids_.find(tid);
  if (it == nodeids_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "sstmac_runtime::get_node: can not find task %d",
                     int(tid));
  }
  return it->second;
}

}
}

