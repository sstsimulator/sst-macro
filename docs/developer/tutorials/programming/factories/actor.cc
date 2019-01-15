#include "actor.h"

namespace sstmac {
namespace tutorial {

actor::actor(sprockit::sim_parameters::ptr& params)
{
  biggest_fan_ = params->get_param("biggest_fan");
}

}
}
