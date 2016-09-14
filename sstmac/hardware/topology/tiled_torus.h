#ifndef TILED_TORUS_H
#define TILED_TORUS_H

#include <sstmac/hardware/topology/hdtorus.h>

namespace sstmac {
namespace hw {


class tiled_torus :
  public hdtorus
{
 public:
  virtual void
  init_factory_params(sprockit::sim_parameters *params) override;

  virtual void
  minimal_routes_to_coords(
    const coordinates &src_coords,
    const coordinates &dest_coords,
    structured_routable::path& current,
    structured_routable::path_set &paths) const override;

  switch_id
  endpoint_to_injection_switch(
      node_id nodeaddr, int ports[], int& num_ports) const override;

  switch_id
  endpoint_to_ejection_switch(
      node_id nodeaddr, int ports[], int& num_ports) const override;

  void
  eject_paths_on_switch(
      node_id dest_addr, switch_id sw_addr,
      structured_routable::path_set &paths) const override;

 protected:
  virtual void
  connect_dim(sprockit::sim_parameters* params,
    int dim,
    connectable *center,
    connectable *plus,
    connectable *minus) override;

 private:
  int port(int replica, int dim, int dir);

 private:
  int ntiles_row_;
  int ntiles_col_;
  std::vector<int> tile_offsets_;
  std::vector<int> rotater_;


};

}
}


#endif // TILED_TORUS_H
