#include "actor.h"

namespace sstmac {
namespace tutorial {

actor::actor(SST::Params& params)
{
  biggest_fan_ = params.find<std::string>("biggest_fan");
}

}
}
