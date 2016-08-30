#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

using namespace sstmac; using namespace sstmac::hw;

void
test_dragonfly_v1(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 4 4";
    params["concentration"] = "2";
    params["optical_link_weight"] = "2.0";
    params["group_connections"] = "3";

    topology* top = topology_factory::get_value("dragonfly", &params);
    structured_topology* dfly = test_cast(structured_topology, top);
    assertTrue(unit, "dragonfly cast topology", bool(dfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(3,2,1,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 1);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(1, 2));
    }

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(0,2,1,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 2);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, 0));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, 2));
    }

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(0,2,0,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 3);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, 0));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, 2));
        if (paths.size() > 2)
            assertEqual(unit, "productive port 2", paths[2].outport, top->convert_to_port(2, 0));
    }

    {
        coordinates c1 = get_vector(0,1,2);
        coordinates c2 = get_vector(1,0,2);
        int dist = dfly->minimal_distance(c1,c2);
        int correct = 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(1,1,2);
        coordinates c2 = get_vector(1,0,2);
        int dist = dfly->minimal_distance(c1,c2);
        int correct = 1;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(0,1,1);
        coordinates c2 = get_vector(1,0,2);
        int dist = dfly->minimal_distance(c1,c2);
        int correct = 3;
        assertEqual(unit, "min distance", dist, correct);
    }

}


void
test_dragonfly_v2(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 4 8";
    params["concentration"] = "2";
    params["optical_link_weight"] = "2.0";
    params["group_connections"] = "3";
    params["seed"] = "14";


    topology* top = topology_factory::get_value("dragonfly", &params);
    structured_topology* dfly = test_cast(structured_topology, top);
    assertTrue(unit, "dragonfly cast topology", bool(dfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(3,2,1,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 1);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(1, 2));
    }

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(0,2,1,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 2);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, 0));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, 2));
    }

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(0,2,0,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 3);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, 0));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, 2));
        if (paths.size() > 2)
            assertEqual(unit, "productive port 2", paths[2].outport, top->convert_to_port(2, 0));
    }

    {
        coordinates coords = get_vector(3,0,1);
        switch_id swid = dfly->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = dfly->node_addr(get_vector(0,2,5,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 3);

        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, 0));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, 2));
        if (paths.size() > 2)
            assertEqual(unit, "productive port 2", paths[2].outport, top->convert_to_port(0, 0));
    }

    {
        coordinates c1 = get_vector(0,1,2);
        coordinates c2 = get_vector(1,0,2);
        int dist = dfly->minimal_distance(c1,c2);
        int correct = 2;
        assertEqual(unit, "min distance", dist, correct);
    }


    {
        coordinates c1 = get_vector(0,1,2);
        coordinates c2 = get_vector(0,1,1);
        int dist = dfly->minimal_distance(c1,c2);
        //group hop plus at least two y hops
        assertTrue(unit, "min distance", dist >= 3);
    }
}
