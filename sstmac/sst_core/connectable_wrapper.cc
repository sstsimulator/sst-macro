#include <sstmac/sst_core/connectable_wrapper.h>
#include <sprockit/util.h>
#include <sprockit/errors.h>
#include <Python.h>

namespace sstmac {

void
integrated_connectable_wrapper::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  connectable* mod)
{
  spkt_abort_printf("integrated_connectable_wrapper::connect: "
             "should not be called");
}

void
integrated_connectable_wrapper::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  connectable* mod)
{
  spkt_abort_printf("integrated_connectable_wrapper::connect: "
             "should not be called");
}

}

