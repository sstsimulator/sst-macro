#ifndef DUMPIAPPMANAGER_H
#define DUMPIAPPMANAGER_H

#include <dumpi/libundumpi/libundumpi.h>
#include <sstmac/backends/native/indexing_app_manager.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace native {

class dumpi_app_manager :
  public indexing_app_manager
{

 public:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  std::string
  to_string() const {
    return "dumpi appmanager";
  }

 protected:
  std::string metafile_;
  sw::dumpi_meta* meta_;

 private:
  void
  do_allocate_and_index_jobs();

};


}
}

#endif // DUMPIAPPMANAGER_H

