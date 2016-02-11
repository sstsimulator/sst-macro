#include <sprockit/test/test.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/stats/location_trace.h>
#include <sstmac/common/runtime.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>

#include <iomanip>

namespace sstmac {

using namespace hw;
using namespace sw;

sstmac_register_app(print_location_trace)


int
print_location_trace_main(int argc, char **argv)
{
    std::string filename = sstmac_env::params->get_param("trace_file");
    location_trace* trace = new location_trace;
    trace->activate(filename);
    timestamp created;
    event_loc_id creator;
    timestamp scheduled;
    event_loc_id runner;

    topology* the_top = sstmac_runtime::current_topology();
    structured_topology* top = safe_cast(structured_topology, the_top);

    switch_id src, dst;
    while (trace->read(created, creator, scheduled, runner))
    {
        if (creator.is_null() || runner.is_null())
            continue;

        if (creator.is_node_id()){
            node_id nid = creator.convert_to_node_id();
            src = the_top->node_to_injector_addr(nid);
        }
        else {
            src = creator.convert_to_switch_id();
        }

        if (runner.is_node_id()){
            node_id nid = runner.convert_to_node_id();
            dst = the_top->node_to_injector_addr(nid);
        }
        else {
            dst = runner.convert_to_switch_id();
        }

        coordinates src_coords = top->switch_coords(src);
        coordinates dst_coords = top->switch_coords(dst);
        std::cout << sprockit::printf("%5d %16.8f ms", int(src), created.msec()) << " ";
        std::cout << std::setw(12) << src_coords.to_string();
        std::cout << " -> " << sprockit::printf("%5d %16.8f ms", int(dst), scheduled.msec()) << " ";
        std::cout << std::setw(12) << dst_coords.to_string();
        std::cout << std::endl;
    }
    return 0;
}


}
