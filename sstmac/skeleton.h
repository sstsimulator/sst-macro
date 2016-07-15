#ifndef sstmac_skeleton_h
#define sstmac_skeleton_h

#ifndef __cplusplus
#error All codes must be compiled as C++ to work - cannot use a C-compiler
#else

#include <sprockit/sim_parameters.h>
#include <sstmac/software/process/global.h>
#include <sstmac/common/sstmac_config.h>

/** Automatically inherit runtime types */
using sprockit::sim_parameters;

typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

#ifndef USER_MAIN
#define USER_MAIN(...) \
 fxn_that_nobody_ever_uses_to_make_magic_happen(); \
 typedef int (*this_file_main_fxn)(__VA_ARGS__); \
 int user_skeleton_main_init_fxn(const char* name, this_file_main_fxn fxn); \
 static int user_skeleton_main(__VA_ARGS__); \
 static int dont_ignore_this = \
  user_skeleton_main_init_fxn(sstmac_app_name, user_skeleton_main); \
 static int user_skeleton_main(__VA_ARGS__)
#endif

extern sprockit::sim_parameters*
get_params();

#endif

#endif
