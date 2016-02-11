#ifndef SKELETONAPPMANAGER_H
#define SKELETONAPPMANAGER_H

#include <sstmac/backends/native/indexing_app_manager.h>

namespace sstmac {
namespace native {


class skeleton_app_manager :
  public indexing_app_manager
{

 public:
  void
  init_factory_params(sprockit::sim_parameters* params);

  std::string
  to_string() const {
    return "skeleton appmanager";
  }

  static void
  parse_launch_cmd(const std::string& prefix,
    sprockit::sim_parameters* params,
    int& nproc,
    int& procs_per_node,
    std::vector<int>& affinities);

 protected:
  int procs_per_node_;

  void parse_launch_cmd(sprockit::sim_parameters* params);

  static void
  parse_aprun(const std::string& cmd, int& nproc, int& nproc_per_node,
              std::vector<int>& core_affinities, const std::string& appname);

 private:
  virtual void
  do_allocate_and_index_jobs();

};


}
}

#endif // SKELETONAPPMANAGER_H

