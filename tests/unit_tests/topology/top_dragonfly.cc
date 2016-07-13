#include "test_topology.h"
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

void
test_dragonfly_traffic(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 6 8";
    params["concentration"] = "1";
    params["optical_link_weight"] = "2.0";
    params["group_connections"] = "4";
    params["seed"] = "14";
    topology* top = topology_factory::get_value("dragonfly", &params);
    structured_topology* dfly = test_cast(structured_topology, top);
    assertTrue(unit, "dragonfly cast topology", bool(dfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    node_id dst;
    std::vector<node_id> partners;

    coordinates src_coords = get_vector(1, 2, 3);
    node_id nid = dfly->node_addr(src_coords);
    coordinates test_coords = dfly->node_coords(nid);
    assertEqual(unit, "coord transform", test_coords, 1, 2, 3);
    node_id src = nid;

    partners.clear();
    dfly->send_partners(traffic_pattern::tornado, src, partners);
    assertEqual(unit, "num tornado partners", partners.size(), 1);
    dst = dfly->node_addr(get_vector(2,4,6));
    assertEqual(unit, "tornado partner", partners[0], dst);

    partners.clear();
    dfly->send_partners(traffic_pattern::bit_complement, src, partners);
    assertEqual(unit, "num BC partners", partners.size(), 1);
    dst = dfly->node_addr(get_vector(2,3,4));
    assertEqual(unit, "BC partner", partners[0], dst);

    partners.clear();
    dfly->send_partners(traffic_pattern::nearest_neighbor, src, partners);
    //would be 8, but one of the group connections is a "self" connection
    assertEqual(unit, "num NN partners", partners.size(), 7);

    dst = dfly->node_addr(get_vector(2,2,3));
    if (partners.size() >= 1)
        assertEqual(unit, "NN partner", partners[0], dst);

    dst = dfly->node_addr(get_vector(0,2,3));
    if (partners.size() >= 2)
        assertEqual(unit, "NN partner", partners[1], dst);

    dst = dfly->node_addr(get_vector(1,3,3));
    if (partners.size() >= 3)
        assertEqual(unit, "NN partner", partners[2], dst);

    dst = dfly->node_addr(get_vector(1,1,3));
    if (partners.size() >= 4)
        assertEqual(unit, "NN partner", partners[3], dst);

    dst = dfly->node_addr(get_vector(1,2,1));
    if (partners.size() >= 5)
        assertEqual(unit, "NN partner", partners[4], dst);

    dst = dfly->node_addr(get_vector(1,2,5));
    if (partners.size() >= 6)
        assertEqual(unit, "NN partner", partners[5], dst);

    dst = dfly->node_addr(get_vector(1,2,7));
    if (partners.size() >= 7)
        assertEqual(unit, "NN partner", partners[6], dst);
}

void
test_dragonfly_v1(UnitTest& unit)
{
    sprockit::sim_parameters params;
    sstmac::env::params = &params;
    params["geometry"] = "4 4 4";
    params["concentration"] = "2";
    params["optical_link_weight"] = "2.0";
    params["group_connections"] = "4";
    params["seed"] = "14";
    topology* top = topology_factory::get_value("dragonfly", &params);
    structured_topology* dfly = test_cast(structured_topology, top);
    assertTrue(unit, "dragonfly cast topology", bool(dfly) );

    switch_interconnect::switch_map switches;
    init_switches(switches, params, top);

    {
        long nids[] = {0, 17, 53, 60};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = dfly->switch_coords(nid);
            switch_id test_id = dfly->switch_number(coords);
            coordinates test_coords = dfly->switch_coords(test_id);
            assertEqual(unit, "dfly switch id", test_id, nid);
            assertEqual(unit, "dfly coords", test_coords, coords);
        }
    }

    {
    network_switch* sw = switches[switch_id(7)];
    router* router = sw->rter();
    packet_flow_payload* packet = msg(4);
    router->route(packet);
    //const routable::path& path = packet->rinfo().current_path();
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::x_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 2);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //               sw, path, 6);
    }

    {
    network_switch* sw = switches[switch_id(6)];
    router* router = sw->rter();
    packet_flow_payload* packet = msg(4);
    router->route(packet);
    //const routable::path& path = packet->rinfo().current_path();
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::y_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 0);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //               sw, path, 2);
    }

    {
    network_switch* sw = switches[switch_id(29)];
    router* router = sw->rter();
    packet_flow_payload* packet = msg(4);
    router->route(packet);
    //const routable::path& path = packet->rinfo().current_path();
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::g_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 0);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //              sw, path, 13);
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
        long nids[] = {0, 17, 71, 108};
        for (int i=0; i < 4; ++i){
            switch_id nid(nids[i]);
            //make sure the functions work back and forth
            coordinates coords = dfly->switch_coords(nid);
            switch_id test_id = dfly->switch_number(coords);
            coordinates test_coords = dfly->switch_coords(test_id);
            assertEqual(unit, "dfly switch id", test_id, nid);
            assertEqual(unit, "dfly coords", test_coords, coords);
        }
    }


    {
    network_switch* sw = switches[switch_id(7)];
    long port; geometry_routable::path path;
    dfly->minimal_route_to_node(switch_id(7), node_id(4), path);
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::x_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 2);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //               sw, path, 6);
    }


    {
    network_switch* sw = switches[switch_id(6)];
    geometry_routable::path path;
    dfly->minimal_route_to_node(switch_id(6), node_id(4), path);
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::y_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 0);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //               sw, path, 2);
    }

    {
    network_switch* sw = switches[switch_id(29)];
    geometry_routable::path path;
    dfly->minimal_route_to_node(switch_id(29), node_id(4), path);
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::g_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 0);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //              sw, path, 13);
    }

    {
    network_switch* sw = switches[switch_id(30)];
    geometry_routable::path path;
    dfly->minimal_route_to_node(switch_id(30), node_id(4), path);
    //assertEqual(unit, "route dragonfly dim", path.dim, (int) dragonfly::x_dimension);
    //assertEqual(unit, "route dragonfly dir", path.dir, 3);
    //assert_dim_dir(unit, "dragonfly dim/dir",
    //              sw, path, 31);
    }

}
