#include <sstmac/hardware/topology/cartesian_topology.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
  namespace hw {


void
cartesian_topology::init_factory_params(sprockit::sim_parameters *params)
{
  /**
  sstkeyword { gui=1 1 1;
      docstring=The number of redundant physical links that make up
                each link[edge] in the topology. These vector dimensions
                should exactly match those in topology_geometry.
                The total available bandwidth in each topology link is:ENDL
                BW=Redundant*Link_BW;
   }
  */
  std::string red_param = params->get_optional_param("redundant", "");
  if (red_param.size() > 0) {
    params->get_vector_param("redundant", red_);
    if (red_.size() != ndimensions()) {
      spkt_throw_printf(sprockit::input_error,
                       "topology::init: wrong number of dimensions in topology_redundant=%s, "
                       "should be %d, got %d\n",
                       red_param.c_str(),
                       ndimensions(),
                       red_.size());
    }
  }
  else {
    int ndim = ndimensions();
    red_.resize(ndim);
    for (int i = 0; i < ndim; i++) {
      red_[i] = 1;
    }
  }
  structured_topology::init_factory_params(params);
}

void
cartesian_topology::minimal_routes_to_switch(switch_id current_sw_addr,
                                             switch_id dest_sw_addr,
                                             structured_routable::path& current_path,
                                             structured_routable::path_set& paths) const
{
  coordinates src = switch_coords(current_sw_addr);
  coordinates dst = switch_coords(dest_sw_addr);
  minimal_routes_to_coords(src, dst, current_path, paths);
}

void
cartesian_topology::setup_port_params(int port, int credits,
                                      double bw, timestamp lat,
                                      const std::string& arb,
                                      sprockit::sim_parameters* params)
{
  std::string port_name = sprockit::printf("port%d", port);
  sprockit::sim_parameters* port_params = params->get_optional_namespace(port_name);
  //for max lookahead, no credit latency
  //put all of the credits on sending, none on credits
  (*port_params)["arbitrator"] = arb;
  (*port_params)["bandwidth"].setBandwidth(bw/1e9, "GB/s");
  (*port_params)["credits"].setByteLength(credits, "B");
  //for output links, put all the latency on the send
  //none of the latency on the credits
  (*port_params)["send_latency"].setTime(lat.nsec(), "ns");
  (*port_params)["credit_latency"].setTime(0, "ns");
}

  }
}
