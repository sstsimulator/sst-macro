#ifndef SIMPLE_INTERCONNECT_H
#define SIMPLE_INTERCONNECT_H

#include <sstmac/hardware/interconnect/switch_interconnect.h>

namespace sstmac {
namespace hw {

class simple_interconnect :
  public switch_interconnect
{

 public:
  simple_interconnect(partition* part, parallel_runtime* rt) :
    switch_interconnect(part, rt)
  {
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "simple interconnect";
  }

  event_loc_id
  event_location() const {
    return event_loc_id::null;
  }

 protected:
  double inverse_bw_;

  timestamp hop_latency_;

  timestamp inj_latency_;

};

}
}

#endif // SIMPLE_INTERCONNECT_H

