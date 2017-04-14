#include <sstmac/util.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>

typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

extern "C" double
sstmac_now(){
  return sstmac::sw::operating_system::current_os()->now().sec();
}

sprockit::sim_parameters*
get_params(){
  return sstmac::sw::operating_system::current_thread()->parent_app()->params();
}

bool&
should_skip_operator_new(){
  return sstmac::sw::operating_system::static_os_thread_context().skip_next_op_new;
}

int
user_skeleton_main_init_fxn(const char* name, main_fxn fxn)
{
  std::cout << "Yeah, I got called with " << name << std::endl;
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
