#include <sstmac/hardware/memory/null_memory_model.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

SpktRegister("null",memory_model,null_memory_model,
            "Implements a null memory model that just reads infinitely fast");

null_memory_model::null_memory_model()
{
}

null_memory_model::~null_memory_model()
{
}

void
null_memory_model::handle(sst_message* msg)
{
  done_->handle(msg);
}

void
null_memory_model::init_factory_params(sprockit::sim_parameters *params)
{
  memory_model::init_factory_params(params);
}

void
null_memory_model::access(sst_message* msg)
{
  mem_debug("null model: doing access of %ld bytes",
    msg->byte_length());

  send_now_self_message(msg);
}

}
} /* namespace sstmac */

