#ifndef sstmac_hw_common_PARAM_EXPANDER_H
#define sstmac_hw_common_PARAM_EXPANDER_H

#include <sstmac/common/param_expander.h>

namespace sstmac {
namespace hw {

class param_expander : public sstmac::param_expander
{

 protected:
  virtual double
  network_bandwidth_multiplier(sprockit::sim_parameters* params) const;

  virtual double
  switch_bandwidth_multiplier(sprockit::sim_parameters* params) const;

  virtual int
  switch_buffer_multiplier(sprockit::sim_parameters* params) const;

};

}
}

#endif // PARAM_EXPANDER_H
