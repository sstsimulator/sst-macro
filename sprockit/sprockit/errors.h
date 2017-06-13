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

#ifndef SPROCKIT_COMMON_ERRORS_H_INCLUDED
#define SPROCKIT_COMMON_ERRORS_H_INCLUDED

#include <exception>
#include <cstdlib>
#include <sprockit/spkt_string.h>
#include <iostream>

namespace sprockit {

//----------------------------------------------------------------------
// ASSERT
//      If condition is false,  print a message and dump core.
//      Useful for documenting assumptions in the code.
//
//      NOTE: needs to be a #define, to be able to print the location
//      where the error occurred.
//----------------------------------------------------------------------
#define SPROCKIT_ASSERT(condition)                                                     \
      if (!(condition)) {                                                       \
          fprintf(stderr, "Assertion failed: line %d, file \"%s\"\n",           \
                  __LINE__, __FILE__);                                          \
          fflush(stderr);                                                       \
          exit(1);                                                              \
      }

#define spkt_throw_printf(exc, template_str, ...) \
    throw exc(::sprockit::printf(#exc ": " template_str "\n%s %d", ##__VA_ARGS__, __FILE__, __LINE__))

#define spkt_abort_printf(template_str, ...) \
  { \
    std::cerr << ::sprockit::printf("error: " template_str "\n%s %d", ##__VA_ARGS__, __FILE__, __LINE__) \
              << std::endl; \
    ::abort(); \
  }


#define spkt_throw(exc, ...) \
    { \
        std::stringstream sstr; \
        ::sprockit::spkt_to_stream(sstr, __VA_ARGS__, "\n", __FILE__, ":", __LINE__); \
        throw exc(sstr.str()); \
    }

/**
 * General errors, or base class for more specific errors.
 */
struct spkt_error : public std::exception {
  spkt_error(const std::string &msg) :
    message(msg) {
  }
  virtual
  ~spkt_error() throw () {
  }
  virtual const char*
  what() const throw () {
    return message.c_str();
  }
  const std::string message;
};

/**
 * Error indicating something was null and shouldn't have been
 */
struct null_error : public spkt_error {
  null_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~null_error() throw () {
  }
};

/**
 * Error indicating some internal value was unexpected.
 */
struct value_error : public spkt_error {
  value_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~value_error() throw () {
  }
};

/**
 * A general error somewhere from a library
 */
struct library_error : public spkt_error {
  library_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~library_error() throw () {
  }
};

/**
 * An error indicating something to do with the advancement of simulation time
 */
struct time_error : public spkt_error {
  time_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~time_error() throw () {
  }
};

/**
 * File open, read, or write error
 */
struct io_error : public spkt_error {
  io_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~io_error() throw () {
  }
};

/**
 * An error indicating some format was not correct
 */
struct illformed_error : public spkt_error {
  illformed_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~illformed_error() throw () {
  }
};

/**
 * An error having to do with an operating system
 */
struct os_error : public spkt_error {
  os_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~os_error() throw () {
  }
};

/**
 * Something happened when allocating, deallocating, or mapping
 * a memory region.
 */
struct memory_error : public spkt_error {
  memory_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~memory_error() throw () {
  }
};

/**
 * Something happened when iterating over a collection
 */
struct iterator_error : public spkt_error {
  iterator_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~iterator_error() throw () {
  }
};

/**
 * A function was intentionally unimplemented because it doesn't make sense,
 * or it is ongoing work
 */
struct unimplemented_error : public spkt_error {
  unimplemented_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~unimplemented_error() throw () {
  }
};

/**
 * Exception indicating that chosen path is not yet ported to new framework.
 * Default to older framework.
*/
struct not_ported_error : public spkt_error {
  not_ported_error(const std::string& msg) :
    spkt_error(msg) {
  }
  virtual ~not_ported_error() throw() {}
};

/**
 * Key to a map was not in the map
 */
struct invalid_key_error : public spkt_error {
  invalid_key_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~invalid_key_error() throw () {
  }
};

/**
 * An index was out of range in a collection.
 */
struct range_error : public spkt_error {
  range_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~range_error() throw () {
  }
};

/**
 * Invalid user input
 */
struct input_error : public spkt_error {
  input_error(const std::string &msg) :
    spkt_error(msg) {
  }
  virtual
  ~input_error() throw () {
  }
};

} // end of namespace sprockit
#endif