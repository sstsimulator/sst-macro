
#ifndef SSTMAC_MICRO_INTEGRATED_SST_CORE_H_
#define SSTMAC_MICRO_INTEGRATED_SST_CORE_H_

#include <sst/core/element.h>
#include <sst/core/params.h>

#include <sprockit/unordered.h>
#include <sprockit/sim_parameters.h>

#include <sstmac/hardware/common/connection.h>

#include <Python.h>

namespace sstmac {

inline sprockit::sim_parameters*
make_sim_params_from_params(
    const SST::Params& map
)
{
  sprockit::sim_parameters* rv = new sprockit::sim_parameters;
  for(auto&& pair : map) {
    rv->parse_keyval(
        map.getParamName(pair.first),
        pair.second, false
    );
  }
  return rv;
}

template <typename Mapping>
sprockit::sim_parameters*
make_sim_params_from_mapping(
    Mapping&& map
)
{
  sprockit::sim_parameters* rv = new sprockit::sim_parameters;
  for(auto&& pair : map) {
    rv->parse_keyval(pair.first, pair.second, false);
  }
  return rv;
}

} // end namespace sstmac

#endif /* SSTMAC_MICRO_INTEGRATED_SST_CORE_H_ */

