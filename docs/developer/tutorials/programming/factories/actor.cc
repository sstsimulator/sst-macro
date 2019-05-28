#include "actor.h"
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace tutorial {

Actor::Actor(SST::Params& params)
{
  biggest_fan_ = params.find<std::string>("biggest_fan");
}

}
}
