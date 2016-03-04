#include <sstmac/software/process/app.h>
#include <sstmac/common/sstmac_env.h>
#include "actor.h"

namespace sstmac {
namespace tutorial {

sstmac_register_app(rob_reiner);

int
rob_reiner_main(int argc, char **argv)
{
  actor* the_guy = actor_factory::get_param("actor_name", sstmac::env::params);
  the_guy->act();
  return 0;
}

}
}

