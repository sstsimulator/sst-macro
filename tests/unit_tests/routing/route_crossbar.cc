#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
using namespace sstmac; using namespace sstmac::hw;

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
        coordinates coords = get_vector(3);
        switch_id swid = xbar->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = xbar->node_addr(get_vector(2,2));

        geometry_routable::path_set paths;
        router->productive_paths_to_node(dst, paths);
        //this should tell me that the productive ports are -X, -Y, -Z
        assertEqual(unit, "num productive ports", paths.size(), 1);


        if (paths.size() > 0)
            assertEqual(unit, "productive port 0", paths[0].outport, router->convert_to_port(0, 2));

    }

    {
        coordinates coords = get_vector(3);
        switch_id swid = xbar->switch_number(coords);
        network_switch* sw = switches[swid];
        router* router = sw->rter();

        node_id dst = xbar->node_addr(get_vector(3,2));

        geometry_routable::path_set paths;
        bool eject = router->productive_paths_to_node(node_id(dst), paths);
        // should only have the ejection port
        assertEqual(unit, "num productive ports", paths.size(), 1);
        assertTrue(unit, "is eject port", eject);
    }

}
