#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/fat_tree_router.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;
void
test_fattree4(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "3 4";
    params["radix"] = "4";
    params["num_levels"] = "3";
    params["router"] = "fattree";
    topology* top = topology_factory::get_value("fattree", &params);
    structured_topology* ftree = test_cast(structured_topology, top);
    assertTrue(unit, "fat tree cast topology", bool(ftree) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        coordinates coords = get_vector(2,1);
        switch_id swid = ftree->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = ftree->node_addr(get_vector(0,0,1));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 1);
        assertEqual(unit, "productive port", paths[0].outport,
            top->convert_to_port(fat_tree::down_dimension, 0));
    }

    {
        coordinates coords = get_vector(0,11);
        switch_id swid = ftree->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = ftree->node_addr(get_vector(0,3,1));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 4);
        for (int k=0; k < paths.size(); ++k){
            assertEqual(unit, "productive port", paths[k].outport,
                        top->convert_to_port(fat_tree::up_dimension, k));
        }
    }

    {
        coordinates coords = get_vector(1,7);
        switch_id swid = ftree->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = ftree->node_addr(get_vector(0,6),2);

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 1);
        assertEqual(unit, "productive port", paths[0].outport,
            top->convert_to_port(fat_tree::down_dimension, 2));
    }

    {
        coordinates c1 = get_vector(0,1);
        coordinates c2 = get_vector(0,11);
        int dist = ftree->minimal_distance(c1,c2);
        //I need to go up two steps
        int correct = 2 * 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(0,1);
        coordinates c2 = get_vector(1,3);
        int dist = ftree->minimal_distance(c1,c2);
        //I need to go up one step
        int correct = 1;
        assertEqual(unit, "min distance", dist, correct);
    }
}

void
test_fattree2(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "5 2";
    params["radix"] = "2";
    params["num_levels"] = "5";
    params["concentration"] = "2";
    params["router"] = "fattree";
    topology* top = topology_factory::get_value("fattree", &params);
    structured_topology* ftree = test_cast(structured_topology, top);
    assertTrue(unit, "fat tree cast topology", bool(ftree) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    /**
        Bottom Up
        Level 0: 0-15
        Level 1: 16-31
        Level 2: 32-47
        Level 3: 48-63
        Level 4: 64-71
        OR
        Top Down
        Level 0: 64-71
        Level 1: 48-63
        Level 2: 32-47
        Level 3: 16-31
        Level 4: 0-15
    */

    {
        coordinates coords = get_vector(0,1);
        switch_id swid = ftree->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = ftree->node_addr(get_vector(4,1,0,1,0,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 2);
        assertEqual(unit, "productive port", paths[0].outport, top->convert_to_port(fat_tree::down_dimension, 0));
        assertEqual(unit, "productive port", paths[1].outport, top->convert_to_port(fat_tree::down_dimension, 2));
    }

    {
        coordinates coords = get_vector(4,0,1,0,1,0);
        switch_id swid = ftree->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = ftree->node_addr(get_vector(4,1,0,1,0,0,0));

        structured_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        assertEqual(unit, "num productive ports", paths.size(), 2);
        for (int k=0; k < paths.size(); ++k)
            assertEqual(unit, "productive port", paths[k].outport, top->convert_to_port(fat_tree::up_dimension, k));
    }

    {
        coordinates c1 = get_vector(4,0,1,0,1,0);
        coordinates c2 = get_vector(4,0,1,1,1,0);
        int dist = ftree->minimal_distance(c1,c2);
        //I need to go up two steps
        int correct = 2 * 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(2,0,1,1);
        coordinates c2 = get_vector(4,0,1,1,1,0);
        int dist = ftree->minimal_distance(c1,c2);
        //I only need to go down two
        int correct = 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(4,0,1,1,1,0);
        coordinates c2 = get_vector(2,0,1,1);
        int dist = ftree->minimal_distance(c1,c2);
        //I only need to go up two
        int correct = 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(2,0,0,1);
        coordinates c2 = get_vector(4,0,1,1,1,0);
        int dist = ftree->minimal_distance(c1,c2);
        //I only need to go up one then down three
        int correct = 1 + 1 + 2;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(4,0,1,1,1,0);
        coordinates c2 = get_vector(2,0,0,1);
        int dist = ftree->minimal_distance(c1,c2);
        //I only need to go up three then down one
        int correct = 2 + 1 + 1;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(4,0,1,1,1,0);
        coordinates c2 = get_vector(4,1,1,0,0,1);
        int dist = ftree->minimal_distance(c1,c2);
        //I only need to go all the way up and back down
        int correct = 8;
        assertEqual(unit, "min distance", dist, correct);
    }

    {
        coordinates c1 = get_vector(3,0,1,0,0);
        coordinates c2 = get_vector(3,0,1,0,1);
        int dist = ftree->minimal_distance(c1,c2);
        //I'm on the right branch, but wrong switch
        int correct = 2;
        assertEqual(unit, "min distance", dist, correct);
    }

}
