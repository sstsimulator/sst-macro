#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/dragonfly.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

extern void test_topology(sprockit::sim_parameters& params);

void
test_dragonfly_v1(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["geometry"] = "4 4 4";
  params["concentration"] = "2";
  params["group_connections"] = "3";
  params["name"] = "dragonfly";
  params["router.name"] = "minimal";
  test_topology(params);
}


void
test_dragonfly_v2(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["geometry"] = "4 4 8";
  params["concentration"] = "2";
  params["group_connections"] = "3";
  params["seed"] = "14";
  params["name"] = "dragonfly";
  params["router.name"] = "minimal";
  test_topology(params);
}
