
#ifndef SSTMAC_MICRO_INTEGRATED_SST_CORE_H_
#define SSTMAC_MICRO_INTEGRATED_SST_CORE_H_

#include <sst_config.h>

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
  std::set<std::string> key_names = map.getKeys();
  for(auto&& key : key_names) {
    rv->parse_keyval(
        key, map.find_string(key), false, true, false);
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
    rv->parse_keyval(pair.first, pair.second, false, true, false);
  }
  return rv;
}

} // end namespace sstmac

#endif /* SSTMAC_MICRO_INTEGRATED_SST_CORE_H_ */

