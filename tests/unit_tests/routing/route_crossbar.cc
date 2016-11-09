#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

extern void test_topology(sprockit::sim_parameters& params);

void test_crossbar(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["geometry"] = "10";
  params["concentration"] = "3";
  params["name"] = "crossbar";
  params["router.name"] = "minimal";
  test_topology(params);
}
