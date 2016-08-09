#include <sstmac/hardware/memory/null_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

SpktRegister("null",memory_model,null_memory_model,
            "Implements a null memory model that just reads infinitely fast");

null_memory_model::~null_memory_model()
{
}

void
null_memory_model::init_factory_params(sprockit::sim_parameters *params)
{
  memory_model::init_factory_params(params);
}

void
null_memory_model::access(long bytes, double max_bw,
                          callback* cb)
{
  mem_debug("null model: doing access of %ld bytes", bytes);
  parent_node_->schedule_now(cb);
}

}
} /* namespace sstmac */

