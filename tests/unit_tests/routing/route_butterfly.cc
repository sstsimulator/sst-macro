#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/butterfly.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

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
        coordinates coords = get_vector(3,2,0);
        switch_id swid = bfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = bfly->node_addr(get_vector(1,0,0,1));

        geometry_routable::path_set paths;
        router->productive_paths_to_node(node_id(dst), paths);
        //this should tell me that the productive ports are -X, -Y, -Z
        assertEqual(unit, "num productive ports", paths.size(), 1);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, router->convert_to_port(0, 1));
    }

    {
        coordinates coords = get_vector(3,1,0);
        switch_id swid = bfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = bfly->node_addr(get_vector(3,2,0,1));

        geometry_routable::path_set paths;
        router->productive_paths_to_node(node_id(dst), paths);

        assertEqual(unit, "num productive ports", paths.size(), 1);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, router->convert_to_port(butterfly::up_dimension, 3));
    }

    /** test some distances */
    {
        coordinates c1 = get_vector(3,1,0);
        coordinates c2 = get_vector(3,2,2);
        int dist = bfly->minimal_distance(c1, c2);
        assertEqual(unit, "min distance", dist, 2);
    }

    {
        coordinates c1 = get_vector(3,1,1);
        coordinates c2 = get_vector(0,2,2);
        int dist = bfly->minimal_distance(c1, c2);
        assertEqual(unit, "min distance", dist, 1);
    }

}
