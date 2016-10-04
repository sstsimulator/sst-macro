#include <sstmac/util.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/skeleton.h>

extern "C" double
sstmac_now(){
  return sstmac::sw::operating_system::current_os()->now().sec();
}

sprockit::sim_parameters*
get_params(){
  return sstmac::sw::operating_system::current_thread()->parent_app()->params();
}

int
user_skeleton_main_init_fxn(const char* name, main_fxn fxn)
{
  sstmac::sw::user_app_cxx_full_main::register_main_fxn(name, fxn);
  return 42;
}

static empty_main_fxn empty_skeleton_main;

int
user_skeleton_main_init_fxn(const char* name, empty_main_fxn fxn)
{
  sstmac::sw::user_app_cxx_empty_main::register_main_fxn(name, fxn);
  return 42;
}
