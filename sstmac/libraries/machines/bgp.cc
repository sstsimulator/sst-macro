#include <sstmac/libraries/machines/bgp.h>
#include <sstmac/common/runtime.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/util.h>

extern "C"
void
Kernel_GetPersonality(_BGP_Personality_t *p, int size)
{
  sstmac::node_id nid = sstmac::runtime::current_node();
  sstmac::hw::topology* top = sstmac::runtime::current_topology();

  sstmac::hw::hdtorus* torus = test_cast(sstmac::hw::hdtorus, top);
  if (!torus || torus->ndimensions() != 3){
    spkt_throw(sprockit::value_error,
        "Kernel_GetPersonality for BGP being called, but topology is not a 3D torus");
  }

  std::vector<int> coords = torus->node_coords(nid);
  std::vector<int> dims = torus->dimensions();
  p->Network_Config.Xcoord = coords[0];
  p->Network_Config.Ycoord = coords[1];
  p->Network_Config.Zcoord = coords[2];
  p->Network_Config.Xnodes = dims[0];
  p->Network_Config.Ynodes = dims[1];
  p->Network_Config.Znodes = dims[2];
}
