#include "test_topology.h"
#include <sprockit/output.h>

using namespace sstmac;
using namespace sstmac::hw;

int main(int argc, char** argv)
{
    sprockit::output::init_out0(&std::cout);
    sprockit::output::init_err0(&std::cerr);
    sprockit::output::init_outn(&std::cout);
    sprockit::output::init_errn(&std::cerr);
    UnitTest unit;
    try{
        std::cout << "Testing torus...\n";
           test_torus_traffic(unit);
           test_torus(unit);
        std::cout << "Testing fat tree...\n";
            test_fattree2(unit);
            test_fattree4(unit);
        std::cout << "Testing crossbar...\n";
            test_crossbar(unit);
        std::cout << "Testing butterfly...\n";
            test_butterfly(unit);
        std::cout << "Testing fbfly...\n";
            test_fbfly(unit);
        std::cout << "Testing dragonfly...\n";
            test_dragonfly_traffic(unit);
            test_dragonfly_v1(unit);
            test_dragonfly_v2(unit);
        unit.validate();
    } catch (std::exception& e) {
        cerr0 << e.what() << std::endl;
        return 1;
    }

    return 0;
}
