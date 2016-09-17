#ifndef TILED_TORUS_H
#define TILED_TORUS_H

#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/topology/multipath_topology.h>

namespace sstmac {
namespace hw {


class tiled_torus :
  public hdtorus,
  public multipath_topology
{
 public:
  tiled_torus(sprockit::sim_parameters *params);

  void
  get_redundant_paths(routable::path& inPath,
                      routable::path_set& outPaths) const override;

  void
  configure_geometric_paths(std::vector<int>& redundancies) const override;

  switch_id
  endpoint_to_injection_switch(
        node_id nodeaddr, int ports[], int& num_ports) const override;

 protected:
  virtual void
  connect_dim(sprockit::sim_parameters* params,
    int dim,
    connectable *center,
    connectable *plus,
    connectable *minus) override;

 private:
  inline int port(int replica, int dim, int dir) const {
    int offset = tile_offsets_[dim];
    //offset for dim, each replica has +/- for *2
    return offset + red_[dim] * dir + replica;
  }

 private:
  int ntiles_row_;
  int ntiles_col_;
  std::vector<int> tile_offsets_;
  std::vector<int> rotater_;
  int first_simple_torus_eject_port_;

};

}
}


#endif // TILED_TORUS_H
