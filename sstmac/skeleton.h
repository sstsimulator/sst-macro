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
    PyObject* module = Py_InitModule("sst." SST_APP_NAME_QUOTED, sst_macro_null_methods); \
    return module; \
  } \
  static const SST::ElementInfoComponent macro_components[] = { \
      {NULL, NULL, NULL, NULL} \
  }; \
  extern "C" { \
  SST::ElementLibraryInfo ELI_NAME(app) = { \
      SST_APP_NAME_QUOTED, \
      "SST Macroscale skeleton app", \
      macro_components, \
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
//redirect all operator news to the nothrow version
//sst will decide whether memory is actually going to be allocated
extern void* sstmac_new(unsigned long size);

#define __builtin_operator_new(size) sstmac_new(size)
#include <new>

extern bool& should_skip_operator_new();

template <class T>
T*
placement_new(void* sstmac_placement_ptr){
  if (sstmac_placement_ptr != nullptr){
    return new (sstmac_placement_ptr) T;
  }
  return reinterpret_cast<T*>(sstmac_placement_ptr);
}

template <class T>
T*
conditional_new(unsigned long size){
  bool& flag = should_skip_operator_new();
  T* ret = nullptr;
  if (!flag){
    ret = new T[size];
  }
  flag = false;
  return ret;
}

template <class T>
T*
conditional_new(){
  bool& flag = should_skip_operator_new();
  T* ret = nullptr;
  if (!flag){
    ret = new T;
  }
  flag = false;
  return ret;
}

#include <sprockit/sim_parameters.h>
#include <sstmac/software/process/global.h>
#include <sstmac/software/api/api_fwd.h>

/** Automatically inherit runtime types */
using sprockit::sim_parameters;

#undef main
#define main USER_MAIN
#define USER_MAIN(...) \
 fxn_that_nobody_ever_uses_to_make_magic_happen(); \
 typedef int (*this_file_main_fxn)(__VA_ARGS__); \
 int user_skeleton_main_init_fxn(const char* name, this_file_main_fxn fxn); \
 static int user_skeleton_main(__VA_ARGS__); \
 static int dont_ignore_this = \
  user_skeleton_main_init_fxn(SST_APP_NAME_QUOTED, user_skeleton_main); \
  sst_eli_block(sstmac_app_name) \
 static int user_skeleton_main(__VA_ARGS__)

extern sprockit::sim_parameters*
get_params();

#else
//not C++, extra work required in sst clang compiler
#endif



#endif
