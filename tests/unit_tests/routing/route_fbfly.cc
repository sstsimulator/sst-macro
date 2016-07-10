#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/topology/flattened_butterfly.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void test_fbfly(UnitTest& unit)
{
    sprockit::sim_parameters params;  sstmac::env::params = &params;
    params["geometry"] = "4 3";
    topology* top = topology_factory::get_value("fbfly", &params);
    structured_topology* bfly = test_cast(structured_topology, top);
    assertTrue(unit, "fbfly cast topology", bool(bfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        coordinates coords = get_vector(3,1);
        switch_id swid = bfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = bfly->node_addr(get_vector(3,2,1));

        geometry_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 1);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, router->convert_to_port(flattened_butterfly::up_dimension, 6));
    }

    {
        coordinates coords = get_vector(3,1);
        switch_id swid = bfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = bfly->node_addr(get_vector(2,2,1));

        geometry_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 2);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, router->convert_to_port(flattened_butterfly::down_dimension, 2));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, router->convert_to_port(flattened_butterfly::up_dimension, 6));
    }

    {
        coordinates c1 = get_vector(3,1);
        coordinates c2 = get_vector(0,1);
        int dist = bfly->minimal_distance(c1, c2);
        assertEqual(unit, "min distance", dist, 1);
    }

    {
        coordinates c1 = get_vector(3,1);
        coordinates c2 = get_vector(0,2);
        int dist = bfly->minimal_distance(c1, c2);
        assertEqual(unit, "min distance", dist, 2);
    }
}

