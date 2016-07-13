#ifndef sstmac_UTIL_H
#define sstmac_UTIL_H

#ifndef __cplusplus
#error All codes must be compiled as C++ to work - cannot use a C-compiler
#else
#include <sprockit/spkt_string.h>
#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sstmac/software/process/task_id.h>

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

extern "C" double sstmac_now();
#endif

#endif

