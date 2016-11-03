#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/butterfly.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

extern void test_topology(sprockit::sim_parameters& params);

void test_butterfly(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["geometry"] = "4 3";
  params["name"] = "crossbar";
  params["router.name"] = "minimal";
  test_topology(params);
}
