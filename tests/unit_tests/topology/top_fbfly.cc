#include "test_topology.h"
#include <sstmac/hardware/topology/flattened_butterfly.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;


void test_fbfly(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 3";

    topology* top = topology_factory::get_value("fbfly", &params);
    structured_topology* bfly = test_cast(structured_topology, top);
    assertTrue(unit, "fbfly cast topology", bool(bfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        long nids[] = {0, 3, 8, 14};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = bfly->switch_coords(nid);
            switch_id test_id = bfly->switch_number(coords);
            coordinates test_coords = bfly->switch_coords(test_id);
            assertEqual(unit, "fbfly switch id", test_id, nid);
            assertEqual(unit, "fbfly coords", test_coords, coords);
        }
    }

    
    {
        coordinates coords = bfly->switch_coords(switch_id(14));
        assertEqual(unit, "fbfly switch coords", coords, 3, 2);
    }

    {
        coordinates coords = bfly->switch_coords(switch_id(3));
        assertEqual(unit, "fbfly switch coords", coords, 0, 3);
    }

    {
        coordinates coords = bfly->switch_coords(switch_id(11));
        assertEqual(unit, "fbfly switch coords", coords, 2, 3);
    }

    {
        coordinates coords = bfly->switch_coords(switch_id(7));
        assertEqual(unit, "fbfly switch coords", coords, 1, 3);
    }

    {
    network_switch* sw = switches[switch_id(7)];
    geometry_routable::path path;
    bfly->minimal_route_to_node(switch_id(7), node_id(4), path);
    //assertEqual(unit, "route fbfly dim", path.dim, (int)flattened_butterfly::down_dimension);
    //assertEqual(unit, "route fbfly dir", path.dir, 0);
    //assert_dim_dir(unit, "fbfly dim/dir",
    //               sw, path, 3);
    }

    {
    network_switch* sw = switches[switch_id(3)];
    geometry_routable::path path;
    bfly->minimal_route_to_node(switch_id(3), node_id(4), path);
    //assertEqual(unit, "route fbfly dim", path.dim, (int)flattened_butterfly::down_dimension);
    //assertEqual(unit, "route fbfly dir", path.dir, 5);
    //assert_dim_dir(unit, "fbfly dim/dir",
    //               sw, path, 1);
    }
}

