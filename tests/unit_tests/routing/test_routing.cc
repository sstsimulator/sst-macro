#include <tests/unit_tests/util/util.h>
#include <sprockit/output.h>

void test_torus(UnitTest& unit);
void test_crossbar(UnitTest& unit);
void test_fattree2(UnitTest& unit);
void test_fattree4(UnitTest& unit);
void test_butterfly(UnitTest& unit);
void test_fbfly(UnitTest& unit);
void test_dragonfly_v1(UnitTest& unit);
void test_dragonfly_v2(UnitTest& unit);

using namespace sstmac;
using namespace sstmac::hw;

void
test_topology(sprockit::sim_parameters& params)
{
  topology* top = topology_factory::get_param("name", &params);
  topology::set_static_topology(top);
  interconnect::switch_map switches;
  init_switches(switches, params, top);
  sstmac::env::params = &params;
}

int main(int argc, char** argv)
{
    sprockit::output::init_out0(&std::cout);
    sprockit::output::init_err0(&std::cerr);
    sprockit::output::init_outn(&std::cout);
    sprockit::output::init_errn(&std::cerr);
    UnitTest unit;
    try{
        std::cout << "Testing torus...\n";
            test_torus(unit);
        std::cout << "Testing fat tree...\n";
            //test_fattree2(unit);
            test_fattree4(unit);
        std::cout << "Testing crossbar...\n";
            test_crossbar(unit);
        std::cout << "Testing butterfly...\n";
            test_butterfly(unit);
        std::cout << "Testing fbfly...\n";
            test_fbfly(unit);
        std::cout << "Testing dragonfly...\n";
            test_dragonfly_v1(unit);
            test_dragonfly_v2(unit);
        unit.validate();
    } catch (std::exception& e) {
        cerrn << e.what() << std::endl;
        return 1;
    }

    return 0;
}
