#include <sstmac/skeleton.h>
#include "actor.h"

#define sstmac_app_name rob_reiner

int
main(int argc, char **argv)
{
  sstmac::tutorial::actor* the_guy = sstmac::tutorial::actor_factory::get_param("actor_name", get_params());
  the_guy->act();
  return 0;
}


