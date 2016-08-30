#include "test_topology.h"
#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/router/valiant_routing.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void test_torus_traffic(UnitTest& unit)
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

    node_id dst;
    std::vector<node_id> partners;

    coordinates src_coords = get_vector(1, 3, 4, 1);
    node_id nid = torus->node_addr(src_coords);
    coordinates test_coords = torus->node_coords(nid);
    assertEqual(unit, "node id", nid, 233);
    assertEqual(unit, "coord transform", test_coords, 1, 3, 4, 1);
    node_id src = nid;

    partners.clear();
    torus->send_partners(traffic_pattern::tornado, src, partners);
    assertEqual(unit, "num tornado partners", partners.size(), 1);
    dst = torus->node_addr(get_vector(3,0,1,1));
    assertEqual(unit, "tornado partner", partners[0], dst);

    partners.clear();
    torus->send_partners(traffic_pattern::nearest_neighbor, src, partners);
    assertEqual(unit, "num NN partners", partners.size(), 6);
    dst = torus->node_addr(get_vector(2,3,4,1));
    assertEqual(unit, "NN partner", partners[0], dst);
    dst = torus->node_addr(get_vector(0,3,4,1));
    assertEqual(unit, "NN partner", partners[1], dst);
    dst = torus->node_addr(get_vector(1,4,4,1));
    assertEqual(unit, "NN partner", partners[2], dst);
    dst = torus->node_addr(get_vector(1,2,4,1));
    assertEqual(unit, "NN partner", partners[3], dst);
    dst = torus->node_addr(get_vector(1,3,0,1));
    assertEqual(unit, "NN partner", partners[4], dst);
    dst = torus->node_addr(get_vector(1,3,3,1));
    assertEqual(unit, "NN partner", partners[5], dst);

    partners.clear();
    torus->send_partners(traffic_pattern::bit_complement, src, partners);
    assertEqual(unit, "num tornado partners", partners.size(), 1);
    dst = torus->node_addr(get_vector(3,1,0,1));
    assertEqual(unit, "tornado partner", partners[0], dst);
}

void test_torus(UnitTest& unit)
{
    sprockit::sim_parameters params;
    params["geometry"] = "5 5 5";
    params["redundant"] = "1 1 1";
    params["concentration"] = "2";
    topology* top = topology_factory::get_value("hdtorus", &params);
    structured_topology* torus = test_cast(structured_topology, top);

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    assertTrue(unit, "torus cast topology", bool(torus) );

    {
        long nids[] = {0, 13, 48, 97};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = torus->switch_coords(nid);
            switch_id test_id = torus->switch_number(coords);
            coordinates test_coords = torus->switch_coords(test_id);
            assertEqual(unit, "torus switch id", test_id, nid);
            assertEqual(unit, "torus coords", test_coords, coords);
        }
    }

    {
    node_id nid(25);
    coordinates coords = torus->node_coords(nid);
    node_id nid_test = torus->node_addr(coords);
    assertEqual(unit, "torus node coords", coords, 2, 2, 0, 1);
    assertEqual(unit, "torus node id mapping", nid_test, nid);
    }

    {
    switch_id sid(37);
    coordinates coords = torus->switch_coords(sid);
    long sid_test = torus->switch_number(coords);
    assertEqual(unit, "torus switch coords", coords, 2, 2, 1);
    assertEqual(unit, "torus switch id mapping", sid_test, sid);
    }

    {
    coordinates coords = get_vector(1,0,2);
    switch_id sid = torus->switch_number(coords);
    coordinates test_coords = torus->switch_coords(sid);
    assertEqual(unit, "torus switch coords mapping", test_coords, coords);
    assertEqual(unit, "torus switch id", sid, 51);
    }

    {
    coordinates coords = get_vector(1,0,2,1);
    node_id nid = torus->node_addr(coords);
    coordinates test_coords = torus->node_coords(nid);
    assertEqual(unit, "torus node coords mapping", test_coords, coords);
    assertEqual(unit, "torus node id", nid, 103);
    }

    params.add_param_override("id", 21);
    router* router = router_factory::get_value("minimal", &params);
    router->set_topology(torus);
    router->set_switch(switches[switch_id(21)]);


    {
        structured_routable::path path;
        torus->minimal_route_to_coords(
            get_vector(0,0,0),
            get_vector(0,0,4),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::neg);

        torus->minimal_route_to_coords(
            get_vector(0,0,4),
            get_vector(0,0,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);

        torus->minimal_route_to_coords(
            get_vector(0,0,1),
            get_vector(0,0,4),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::neg);

        torus->minimal_route_to_coords(
            get_vector(0,0,2),
            get_vector(0,0,4),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);

        torus->minimal_route_to_coords(
            get_vector(0,0,3),
            get_vector(0,0,4),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);
    }

    {
        structured_routable::path path;
        torus->minimal_route_to_coords(
            get_vector(0,0,0),
            get_vector(0,4,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::neg);

        torus->minimal_route_to_coords(
            get_vector(0,4,0),
            get_vector(0,0,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);

        torus->minimal_route_to_coords(
            get_vector(0,1,0),
            get_vector(0,4,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::neg);

        torus->minimal_route_to_coords(
            get_vector(0,2,0),
            get_vector(0,4,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);

        torus->minimal_route_to_coords(
            get_vector(0,3,0),
            get_vector(0,4,0),
            path);
        //assertEqual(unit, "torus dir", path.dir, (int) hdtorus::pos);
    }

}



