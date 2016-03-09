#ifndef sstmac_hardware_memory_NULL_MEMORYMODEL_H
#define sstmac_hardware_memory_NULL_MEMORYMODEL_H

#include <sstmac/hardware/memory/memory_model.h>

namespace sstmac {
namespace hw {

class null_memory_model :
  public memory_model
{
 public:
  null_memory_model();

  virtual ~null_memory_model();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  handle(sst_message* msg);

  virtual void
  access(sst_message* msg);

  double
  max_single_bw() const {
    return 1e15; // Just return a ridiculous number like 1 PB/s
  }

};

}
} /* namespace sstmac */

#endif // NULL_MEMORYMODEL_H

