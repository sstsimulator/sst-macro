#ifndef sstmac_UTIL_H
#define sstmac_UTIL_H

#ifndef __cplusplus
#error All codes must be compiled as C++ to work - cannot use a C-compiler
#else
#define EXTERN_C extern "C"
#include <sprockit/sim_parameters.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/spkt_string.h>
#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sstmac/software/process/global.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/task_id.h>

typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

#define USER_MAIN(...) \
 ignore_this_integer_completely = 42; \
 typedef int (*this_file_main_fxn)(__VA_ARGS__); \
 int user_skeleton_main_init_fxn(this_file_main_fxn fxn); \
 extern "C" int user_skeleton_main(__VA_ARGS__); \
 int dont_ignore_this = user_skeleton_main_init_fxn(user_skeleton_main); \
 extern "C" int user_skeleton_main(__VA_ARGS__)

using sstmac::timestamp;

/** Automatically inherit runtime types */
using sprockit::sim_parameters;

/** Automatically inherit the errors */
using sprockit::illformed_error;
using sprockit::input_error;
using sprockit::invalid_key_error;
using sprockit::io_error;
using sprockit::iterator_error;
using sprockit::library_error;
using sprockit::memory_error;
using sprockit::null_error;
using sprockit::os_error;
using sprockit::range_error;
using sprockit::spkt_error;

extern sprockit::sim_parameters*
get_params();

EXTERN_C double
sstmac_now();

#define FORCE_FORTRAN_LINKAGE EXTERN_C int user_skeleton_main_(int,char**); \
namespace sstmac { namespace sw { \
extern main_fxn user_skeleton_main; \
} } \
static int init_fxn() \
{ \
  sstmac::sw::user_skeleton_main = &user_skeleton_main_; \
  return 42; \
} \
int sstmac_c_linkage_integer = init_fxn();
#endif

#endif

