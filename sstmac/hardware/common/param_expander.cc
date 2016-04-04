#include <sstmac/hardware/common/param_expander.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

double
param_expander::nic_bandwidth_multiplier(sprockit::sim_parameters *params) const
{
  double ret = params->get_optional_int_param("injection_redundant", 1);
  return ret;
}

double
param_expander::network_bandwidth_multiplier(sprockit::sim_parameters *params) const
{
  if (params->has_param("topology_redundant")){
    std::vector<int> red;
    params->get_vector_param("topology_redundant", red);
    int sum = 0;
    for (int i=0; i < red.size(); ++i){
      sum += red[i];
    }
    return ((double)sum) / red.size();
  } else {
    return 1.0;
  }
}

double
param_expander::switch_bandwidth_multiplier(sprockit::sim_parameters *params) const
{
  if (params->has_param("switch_geometry")){
    std::vector<int> geom;
    params->get_vector_param("switch_geometry", geom);
    int prod = 1;
    for (int i=0; i < geom.size(); ++i){
      prod *= geom[i];
    }
    return prod;
  } else {
    return 1.0;
  }
}

int
param_expander::switch_buffer_multiplier(sprockit::sim_parameters *params) const
{
  if (params->has_param("topology_redundant")){
    std::vector<int> red;
    params->get_vector_param("topology_redundant", red);
    int sum = 0;
    for (int i=0; i < red.size(); ++i){
      sum += red[i];
    }
    return sum;
  } else {
    return 1;
  }
}

}
}
