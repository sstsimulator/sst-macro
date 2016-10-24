#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>

using namespace sstmac;
using namespace sstmac::hw;

extern void test_topology(sprockit::sim_parameters& params);

void test_fbfly(UnitTest& unit)
{
  sprockit::sim_parameters params;
  params["geometry"] = "4 3";
  params["name"] = "fbfly";
  params["router.name"] = "minimal";
  //test_topology(params);
}

