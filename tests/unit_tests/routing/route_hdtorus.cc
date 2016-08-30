#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void test_torus(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "5 5 5";
    params["redundant"] = "1 1 1";
    params["concentration"] = "2";
    topology* top = topology_factory::get_value("hdtorus", &params);
    structured_topology* torus = test_cast(structured_topology, top);

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    assertTrue(unit, "torus cast topology", bool(torus) );

    {
        coordinates coords = get_vector(3,2,3);
        switch_id swid = torus->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = torus->node_addr(get_vector(1,1,1,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        //this should tell me that the productive ports are -X, -Y, -Z
        assertEqual(unit, "num productive ports", paths.size(), 3);


        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(0, hdtorus::neg));
        if (paths.size() > 1)
            assertEqual(unit, "productive port 1", paths[1].outport, top->convert_to_port(1, hdtorus::neg));
        if (paths.size() > 2)
            assertEqual(unit, "productive port 2", paths[2].outport, top->convert_to_port(2, hdtorus::neg));
    }


    {
        coordinates coords = get_vector(3,2,3);
        switch_id swid = torus->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = torus->node_addr(get_vector(3,2,0,1));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        //this should tell me that the productive ports are -X, -Y, -Z
        assertEqual(unit, "num productive ports", paths.size(), 1);


        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, top->convert_to_port(2, hdtorus::pos));
    }

    {
        coordinates c1 = get_vector(3,2,3);
        coordinates c2 = get_vector(2,4,0);
        int dist = torus->minimal_distance(c1, c2);
        int correct = 1 + 2 + 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(4,2,3);
        coordinates c2 = get_vector(1,2,2);
        int dist = torus->minimal_distance(c1, c2);
        int correct = 2 + 0 + 1;
        assertEqual(unit, "min distance", dist, correct);
    }
}


