/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SPROCKIT_COMMON_DEBUG_H_INCLUDED
#define SPROCKIT_COMMON_DEBUG_H_INCLUDED

#include <map>
#include <cstdio>
#include <string>
#include <stdint.h>

#define MAX_DEBUG_SLOT 63 //64 bit integer

namespace sprockit {

class debug_int
{
  public:
    debug_int(int slot){
      init(slot);
    }

    debug_int(){
      fields = 0;
    }

    debug_int&
    operator~(){
      fields = ~fields;
      return *this;
    }

    void
    init(int slot){
      fields = (1ull) << slot;
    }

    debug_int& operator=(const debug_int& rhs){
      fields = rhs.fields;
      return *this;
    }

    operator bool() const {
      return fields;
    }

    std::string
    to_string() const;

    uint64_t fields;
};

inline debug_int
operator|(const debug_int& lhs, const debug_int& rhs)
{
  debug_int ret;
  ret.fields = lhs.fields | rhs.fields;
  return ret;
}

inline debug_int
operator&(const debug_int& lhs, const debug_int& rhs)
{
  debug_int ret;
  ret.fields = lhs.fields & rhs.fields;
  return ret;
}

class debug_indent
{
 public:
#if SPROCKIT_ENABLE_DEBUG
  #define DEBUG_MAX_INDENT 6
  int level;
  const char* indents[DEBUG_MAX_INDENT];

  debug_indent();

  debug_indent& operator++(){
    ++level;
    if (level >= DEBUG_MAX_INDENT){
      spkt_throw_printf(illformed_error,
        "debug indent higher than max %d",
        DEBUG_MAX_INDENT);
    }
    return *this;
  }

  debug_indent& operator--(){
    if (level == 0){
      spkt_throw(illformed_error,
        "debug indent less than zero");
    }
    --level;
    return *this;
  }

  const char*
  c_str() const {
    return indents[level];
  }
#else
  void operator++(){}
  void operator--(){}
  const char*
  c_str() const {
    return "";
  }
#endif
};

class debug_prefix_fxn {
 public:
  virtual std::string str() = 0;
};


class debug
{
 public:
  static std::map<std::string, debug_int*>* debug_ints_;
  static std::map<std::string, std::string>* docstrings_;
  static debug_prefix_fxn* prefix_fxn;

  static void
  delete_statics();

  static debug_int start_bitmask_;
  static debug_int current_bitmask_;
  static int num_bits_assigned;

  static void
  turn_off();

  static void
  turn_on();

  static void
  turn_off(debug_int& di);

  static void
  turn_on(debug_int& di);

  static void
  register_debug_slot(const std::string& str, debug_int* dint_ptr, const std::string& docstring);

  static void
  turn_on(const std::string& str);

  static void
  print_debug_string(const std::string& str);

  static bool
  slot_active(const debug_int& allowed);

  static void
  print_all_debug_slots(std::ostream& os);

 protected:
  static void
  assign_slot(debug_int& dint);

};

class debug_register_slot {
 public:
  debug_register_slot(const std::string& str, debug_int* dint_ptr, const std::string& docstring){
    debug::register_debug_slot(str, dint_ptr, docstring);
  }
};

} // end of namespace sprockit

#define DeclareDebugSlot(name) \
  namespace sprockit { namespace dbg { \
  extern debug_int name; \
  } }


//VA_ARGS here is an optional docstring
#define RegisterDebugSlot(name, ...) \
  namespace sprockit { \
  namespace dbg { \
    debug_int name; \
    debug_register_slot name##_debug_registerer(#name, &name, "" __VA_ARGS__); \
  } }

#define MakeDebugSlot(name, ...) \
  DeclareDebugSlot(name) \
  RegisterDebugSlot(name)

#ifdef SPROCKIT_DISABLE_DEBUG
#define debug_printf(...) //eliminate completely
#else
//VA_ARGS[0] = format_str
#define debug_printf(slot_bitmask, ...) \
  if (::sprockit::debug::slot_active(slot_bitmask)){ \
    ::sprockit::debug::print_debug_string(::sprockit::printf(__VA_ARGS__)); \
  }
#endif



#endif

