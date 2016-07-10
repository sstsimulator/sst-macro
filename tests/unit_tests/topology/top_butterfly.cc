#include "test_topology.h"
#include <sstmac/hardware/topology/butterfly.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void test_butterfly(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 3";

    topology* top = topology_factory::get_value("butterfly", &params);
    structured_topology* bfly = test_cast(structured_topology, top);
    assertTrue(unit, "butterfly cast topology", bool(bfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        long nids[] = {0, 17, 45, 46};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = bfly->switch_coords(nid);
            switch_id test_id = bfly->switch_number(coords);
            coordinates test_coords = bfly->switch_coords(test_id);
            assertEqual(unit, "bfly switch id", test_id, nid);
            assertEqual(unit, "bfly coords", test_coords, coords);
        }
    }

    {
    network_switch* sw = switches[switch_id(7)];
    geometry_routable::path path;
    bfly->minimal_route_to_node(switch_id(7), node_id(4), path);
    //assertEqual(unit, "route butterfly dim", path.dim, (int) butterfly::up_dimension);
    //assertEqual(unit, "route butterfly dir", path.dir, 0);
    //assert_dim_dir(unit, "butterfly dim/dir",
    //               sw, path, 19);

    bfly->minimal_route_to_node(switch_id(7), node_id(42), path);
    //assertEqual(unit, "route butterfly dim", path.dim, (int) butterfly::up_dimension);
    //assertEqual(unit, "route butterfly dir", path.dir, 2);
    //assert_dim_dir(unit, "butterfly dim/dir",
    //               sw, path, 27);

    bfly->minimal_route_to_node(switch_id(7), node_id(15), path);
    //assertEqual(unit, "route butterfly dim", path.dim, (int) butterfly::up_dimension);
    //assertEqual(unit, "route butterfly dir", path.dir, 0);
    //assert_dim_dir(unit, "butterfly dim/dir",
    //               sw, path, 19);
    }


}
