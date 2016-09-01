#ifndef sstmac_skeleton_h
#define sstmac_skeleton_h

#define SSTPP_QUOTE(name) #name
#define SSTPP_STR(name) SSTPP_QUOTE(name)
#define SST_APP_NAME_QUOTED SSTPP_STR(sstmac_app_name)
#define ELI_NAME(app) app##_eli
typedef int (*main_fxn)(int,char**);
typedef int (*empty_main_fxn)();

#if SSTMAC_INTEGRATED_SST_CORE && defined(SSTMAC_EXTERNAL_SKELETON)
#include <Python.h>
#define sst_eli_block(app) \
  static PyMethodDef sst_macro_null_methods[] = { \
      { NULL, NULL, 0, NULL } \
  }; \
  static inline void* gen_sst_macro_integrated_pymodule(void) \
  { \
    PyObject* module = Py_InitModule("sst." #app, sst_macro_null_methods); \
    return module; \
  } \
  extern "C" { \
  SST::ElementLibraryInfo ELI_NAME(app) = { \
      "macro", \
      "SST Macroscale skeleton app", \
      NULL, \
      NULL, \
      NULL, \
      NULL, \
      NULL, \
      NULL, \
      gen_sst_macro_integrated_pymodule \
  }; \
  }
#else
#define sst_eli_block(app)
#endif

#include <sstmac/common/sstmac_config.h>

#ifdef __cplusplus
#include <sprockit/sim_parameters.h>
#include <sstmac/software/process/global.h>
#include <sstmac/software/api/api_fwd.h>

/** Automatically inherit runtime types */
using sprockit::sim_parameters;

#ifndef USER_MAIN
#define USER_MAIN(...) \
 fxn_that_nobody_ever_uses_to_make_magic_happen(); \
 typedef int (*this_file_main_fxn)(__VA_ARGS__); \
 int user_skeleton_main_init_fxn(const char* name, this_file_main_fxn fxn); \
 static int user_skeleton_main(__VA_ARGS__); \
 static int dont_ignore_this = \
  user_skeleton_main_init_fxn(SST_APP_NAME_QUOTED, user_skeleton_main); \
  sst_eli_block(sstmac_app_name) \
 static int user_skeleton_main(__VA_ARGS__)
#endif

extern sprockit::sim_parameters*
get_params();
#else //not C++, extra work required

#ifndef USER_MAIN
#define USER_MAIN(...) \
 fxn_that_nobody_ever_uses_to_make_magic_happen(); \
 sst_eli_block(sstmac_app_name) \
 int sstmac_app_name(__VA_ARGS__)
#endif

#endif




#endif
