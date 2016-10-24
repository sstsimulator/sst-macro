#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/topology/fat_tree.h>
#include <sstmac/hardware/router/fat_tree_router.h>
#include <sprockit/util.h>

extern void test_topology(sprockit::sim_parameters& params);

void
test_fattree4(UnitTest& unit)
{
  sprockit::sim_parameters params;
  sstmac::env::params = &params;
  params["geometry"] = "3 4";
  params["radix"] = "4";
  params["num_levels"] = "3";
  params["router.name"] = "fattree";
  params["name"] = "fattree";
  test_topology(params);
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
  params["router.name"] = "fattree";
  params["name"] = "fattree";
  test_topology(params);
}
