/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SPROCKIT_COMMON_DEBUG_H_INCLUDED
#define SPROCKIT_COMMON_DEBUG_H_INCLUDED

#include <map>
#include <cstdio>
#include <string>
#include <stdint.h>
#include <iostream>

#define MAX_DEBUG_SLOT 63 //64 bit integer

namespace sprockit {

/**
 * @brief The debug_int class is an opaque wrapper around an integer
 *  used for checking if debug slots are active.  The debug system
 *  will internally store a debug_int bitmask with 1's corresponding to all
 *  the debug slots activated via command line/param file.
 *  Every debug print request provides a debug_int to check against the
 *  internal bitmask.  If the bitwise AND is zero, no active debug slots match
 *  and nothing is printed.
 */
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

/**
 * @brief The debug_prefix_fxn class
 * Produces a string prefix (dynamically, called every time)
 * that can be registered with the debug system to add
 * extra metadata to each debug print. An example usage
 * would be to print timestamps in front of each debug string.
 */
class debug_prefix_fxn {

public:
  virtual std::string str() = 0;
  virtual ~debug_prefix_fxn() = 0;

};


class debug
{
 public:
  static std::map<std::string, debug_int*>* debug_ints_;
  static std::map<std::string, std::string>* docstrings_;
  static debug_prefix_fxn* prefix_fxn;

  static void
  delete_statics();

  /** The bitmask corresponding to all slots that were active at the beginning */
  static debug_int start_bitmask_;
  /** <= start_bitmask_ - some debug slots may have been selectively deactivated */
  static debug_int current_bitmask_;
  /** The number of debug slots that have been assigned unique indices */
  static int num_bits_assigned;

  /**
   * @brief turn_off Turn off all debug printing for all flags
   */
  static void
  turn_off();

  /**
   * @brief turn_on All debug flags that have been registered or
   * previously turned on are reactivated
   */
  static void
  turn_on();

  /**
   * @brief turn_off Turn off a specific type of debug output
   * @param di The identifier for the debug printing to deactivate
   */
  static void
  turn_off(debug_int& di);

  /**
   * @brief turn_on Turn on a specific type of debug output
   * @param di The identifier for the debug printing to activate
   */
  static void
  turn_on(debug_int& di);

  /**
   * @brief register_debug_slot Register a new debug slot that can be
   *  activated via the command line or input file flags
   * @param str       The unique string identifying the slot
   * @param dint_ptr  A pointer to the debug integer that will be used for
   *                  checking if debug printing is active. After registration,
   *                  the debug integer pointed to will be updated.
   * @param docstring An docstring describing the output produced by the debug
   *                  when the debug slot is active
   */
  static void
  register_debug_slot(const std::string& str,
                      debug_int* dint_ptr,
                      const std::string& docstring);

  /**
   * @brief turn_on  Another option for activating debug slots.
   *                 Instead of activating via debug_int, activate via
   *                 the associated string name
   * @param str      The string name associated with a debug slot
   */
  static void
  turn_on(const std::string& str);


  /**
   * @brief print_debug_string  Extra detail (prefix_fxn) can be added to
   *  each debug print.  This provides a wrapper function that prints
   *  a given string with all associated prefix metadata. This ALWAYS prints,
   *  and is not dependent on certain debug slots being active.
   * @param str The string to print after prefix metadata
   * @param os  The stream to print to
   */
  static void
  print_debug_string(const std::string& str, std::ostream& os = std::cout);

  /**
   * @brief slot_active Determine whether a debug slot is active. This is usually
   *  just a bitwise AND of the input parameter and a debug int
   *  with bits set to 1 for all the active slots
   * @param allowed     A debug integer indicating one or more possible
   *                    debug slots
   * @return            Whether the slots in the input parameter match
   *                    any of the active debug slots
   */
  static bool
  slot_active(const debug_int& allowed);

  /**
   * @brief print_all_debug_slots Print all of the possible debug slots
   *  registered with the debug system and any associated doc strings
   * @param os  The stream to print to
   */
  static void
  print_all_debug_slots(std::ostream& os = std::cout);

 private:
  /**
   * @brief assign_slot If a given debug slot is activated, it must be assigned
   *  a unique integer index to allow bitwise AND checking of slots to see if they
   *  are active. This method should never be called by external code
   * @param dint  A reference to the integer that must be assigned a value
   */
  static void
  assign_slot(debug_int& dint);

};


/**
 * @brief The debug_register_slot class
 */
class debug_register_slot {
 public:
  /**
   * @param str       The unique string identifying the slot
   * @param dint_ptr  A pointer to the debug integer that will be used for
   *                  checking if debug printing is active. After registration,
   *                  the debug integer pointed to will be updated.
   * @param docstring An docstring describing the output produced by the debug
   *                  when the debug slot is active
   */
  debug_register_slot(const std::string& str,
                      debug_int* dint_ptr,
                      const std::string& docstring){
    debug::register_debug_slot(str, dint_ptr, docstring);
  }
};

} // end of namespace sprockit

/**
 * Macro used for declaring a debug slot in a header file.
 * This only declares the symbols used for debug printing
 * without defining any of them. An arbitrary number
 * of declarations are allowed.
 */
#define DeclareDebugSlot(name) \
  namespace sprockit { namespace dbg { \
  extern debug_int name; \
  } }


/**
 * Macro used for defining all the symbols associated with a previously
 * declared debug slot (DeclareDebugSlot). This should be defined once
 * in a single source file. The declaration should always go in the
 * global namespace. VA_ARGS here is an optional docstring
 */
#define RegisterDebugSlot(name, ...) \
  namespace sprockit { \
  namespace dbg { \
    debug_int name; \
    debug_register_slot name##_debug_registerer(#name, &name, "" __VA_ARGS__); \
  } }

/**
 * Declare and define a debug slot in a single macro in a source file.
 * Should always go in global namespace
 */
#define MakeDebugSlot(name, ...) \
  DeclareDebugSlot(name) \
  RegisterDebugSlot(name)

#ifdef SPROCKIT_DISABLE_DEBUG
#define debug_printf(...) //eliminate completely
#else
//VA_ARGS[0] = format_str
//This has to be a macro, not a function otherwise we lose the string literal
//GCC then complains about format string vulnerabilities
#define debug_printf(slot_bitmask, ...) \
  if (::sprockit::debug::slot_active(slot_bitmask)){ \
    ::sprockit::debug::print_debug_string(::sprockit::printf(__VA_ARGS__)); \
  }

#define conditional_debug_printf(slot_bitmask, cond, ...) \
  if (::sprockit::debug::slot_active(slot_bitmask && (cond))){ \
    ::sprockit::debug::print_debug_string(::sprockit::printf(__VA_ARGS__)); \
  }

#endif



#endif