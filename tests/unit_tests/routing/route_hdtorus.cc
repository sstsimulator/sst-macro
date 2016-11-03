#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/hdtorus.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

extern void test_topology(sprockit::sim_parameters& params);

void test_torus(UnitTest& unit)
{
  sprockit::sim_parameters params;
  params["geometry"] = "5 5 5";
  params["redundant"] = "1 1 1";
  params["concentration"] = "2";
  params["name"] = "hdtorus";
  params["router.name"] = "minimal";
  test_topology(params);
}


