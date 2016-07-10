#include "test_topology.h"
#include <sstmac/hardware/topology/structured_topology.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void test_crossbar(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "10";
    params["concentration"] = "3";
    topology* top = topology_factory::get_value("crossbar", &params);
    structured_topology* xbar = test_cast(structured_topology, top);
    assertTrue(unit, "crossbar cast topology", bool(xbar) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        long nids[] = {0, 3, 8, 14};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = xbar->switch_coords(nid);
            switch_id test_id = xbar->switch_number(coords);
            coordinates test_coords = xbar->switch_coords(test_id);
            assertEqual(unit, "xbar switch id", test_id, nid);
            assertEqual(unit, "xbar coords", test_coords, coords);
        }
    }



    {
    network_switch* sw = switches[switch_id(7)];
    geometry_routable::path path;
    xbar->minimal_route_to_node(switch_id(7), node_id(4), path);
    //assertEqual(unit, "route crossbar", path.dim, 0);
    //assert_dim_dir(unit, "crossbar dim/dir",
    //               sw, path, 1);

    xbar->minimal_route_to_node(switch_id(7), node_id(23), path);
    //assertEqual(unit, "route crossbar", path.dim, (int) topology::eject);

    xbar->minimal_route_to_node(switch_id(7), node_id(15), path);
    //assertEqual(unit, "route crossbar", path.dim, 0);
    //assert_dim_dir(unit, "crossbar dim/dir",
    //               sw, path, 5);
    }

}
