#include <sstmac/util.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/user_app.h>

extern "C" double
sstmac_now(){
  return sstmac::sw::operating_system::current_os()->now().sec();
}

sprockit::sim_parameters*
get_params(){
  return sstmac::sw::operating_system::current_thread()->parent_app()->params();
}

int
user_skeleton_main_init_fxn(main_fxn fxn)
{
  sstmac::sw::user_skeleton_main = fxn;
  return 42;
}

static empty_main_fxn empty_skeleton_main;

int
user_skeleton_main_empty_wrapper(int argc, char** argv)
{
  (*empty_skeleton_main)();
  return 0;
}

int
user_skeleton_main_init_fxn(empty_main_fxn fxn)
{
  sstmac::sw::user_skeleton_main = &user_skeleton_main_empty_wrapper;
  empty_skeleton_main = fxn;
  return 42;
}
