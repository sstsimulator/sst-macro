#ifndef SIMPLE_INTERCONNECT_H
#define SIMPLE_INTERCONNECT_H

#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace hw {

class simple_interconnect :
  public interconnect
{

 public:
  simple_interconnect(sprockit::sim_parameters* params, event_manager* mgr,
                      partition* part, parallel_runtime* rt);

 private:
  sprockit::sim_parameters*
  override_params(sprockit::sim_parameters* params);

  double inverse_bw_;

  timestamp hop_latency_;

  timestamp inj_latency_;

};

}
}

#endif // SIMPLE_INTERCONNECT_H

