#include <sstmac/hardware/common/param_expander.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

double
param_expander::network_bandwidth_multiplier(sprockit::sim_parameters *params) const
{
  sprockit::sim_parameters* top_params = params->get_optional_namespace("topology");
  if (top_params->has_param("redundant")){
    std::vector<int> red;
    top_params->get_vector_param("redundant", red);
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
  sprockit::sim_parameters* sw_params = params->get_optional_namespace("switch");
  if (sw_params->has_param("geometry")){
    std::vector<int> geom;
    sw_params->get_vector_param("geometry", geom);
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
  sprockit::sim_parameters* top_params = params->get_optional_namespace("topology");
  if (top_params->has_param("redundant")){
    std::vector<int> red;
    top_params->get_vector_param("redundant", red);
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
