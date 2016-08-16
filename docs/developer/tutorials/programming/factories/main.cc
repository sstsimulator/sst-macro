#include <sstmac/skeleton.h>
#include "actor.h"

#define sstmac_app_name rob_reiner

int
main(int argc, char **argv)
{
  actor* the_guy = actor_factory::get_param("actor_name", sstmac::sw::app:::get_params());
  the_guy->act();
  return 0;
}


