#include "actor.h"

namespace sstmac {
namespace tutorial {

actor::actor(SST::Params& params)
{
  biggest_fan_ = params->get_param("biggest_fan");
}

}
}
