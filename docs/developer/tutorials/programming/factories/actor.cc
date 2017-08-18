#include "actor.h"

namespace sstmac {
namespace tutorial {

actor::actor(sprockit::sim_parameters* params)
{
  biggest_fan_ = params->get_param("biggest_fan");
}

}
}
