#include "actor.h"

ImplementFactory(sstmac::tutorial::actor);

namespace sstmac {
namespace tutorial {

void
actor::init_factory_params(sprockit::sim_parameters* params)
{
  biggest_fan_ = params->get_param("biggest_fan");
}

void
actor::clone_into(actor* cln) const
{
  cln->biggest_fan_ = biggest_fan_;
}

}
}

