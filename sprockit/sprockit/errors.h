/**
Copyright 2009-2024 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2024, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
#include <sprockit/spkt_printf.h>
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
    throw exc(::sprockit::sprintf(#exc ": " template_str "\n%s %d", ##__VA_ARGS__, __FILE__, __LINE__))

inline void abort(const std::string& str){
  std::cerr << str << std::endl;
  ::abort();
}

#define spkt_abort_printf(template_str, ...) \
  sprockit::abort(::sprockit::sprintf("error: " template_str "\n%s %d", \
    ##__VA_ARGS__, __FILE__, __LINE__));

/**
 * General errors, or base class for more specific errors.
 */
struct SpktError : public std::exception {
  SpktError(const std::string &msg) :
    message(msg) {
  }
  ~SpktError() throw () override {}

  const char* what() const throw () override {
    return message.c_str();
  }

  const std::string message;
};

/**
 * Error indicating something was null and shouldn't have been
 */
struct NullError : public SpktError {
  NullError(const std::string &msg) :
    SpktError(msg) {
  }
  ~NullError() throw () override {}
};

/**
 * Error indicating some internal value was unexpected.
 */
struct ValueError : public SpktError {
  ValueError(const std::string &msg) :
    SpktError(msg) {
  }
  ~ValueError() throw () override {}
};

/**
 * A general error somewhere from a library
 */
struct LibraryError : public SpktError {
  LibraryError(const std::string &msg) :
    SpktError(msg) {
  }
  ~LibraryError() throw () override {}
};

/**
 * An error indicating something to do with the advancement of simulation time
 */
struct TimeError : public SpktError {
  TimeError(const std::string &msg) :
    SpktError(msg) {
  }
  ~TimeError() throw () override {}
};

/**
 * File open, read, or write error
 */
struct IOError : public SpktError {
  IOError(const std::string &msg) :
    SpktError(msg) {
  }
  ~IOError() throw () override {}
};

/**
 * An error indicating some format was not correct
 */
struct IllformedError : public SpktError {
  IllformedError(const std::string &msg) :
    SpktError(msg) {
  }
  ~IllformedError() throw () override {}
};

/**
 * An error having to do with an operating system
 */
struct OSError : public SpktError {
  OSError(const std::string &msg) :
    SpktError(msg) {
  }
  ~OSError() throw () override {}
};

/**
 * Something happened when allocating, deallocating, or mapping
 * a memory region.
 */
struct MemoryError : public SpktError {
  MemoryError(const std::string &msg) :
    SpktError(msg) {
  }

  ~MemoryError() throw () override {}
};

/**
 * Something happened when iterating over a collection
 */
struct IteratorError : public SpktError {
  IteratorError(const std::string &msg) :
    SpktError(msg) {
  }

  ~IteratorError() throw () override {}

};

/**
 * A function was intentionally unimplemented because it doesn't make sense,
 * or it is ongoing work
 */
struct UnimplementedError : public SpktError {
  UnimplementedError(const std::string &msg) :
    SpktError(msg) {
  }

  ~UnimplementedError() throw () override {}
};

/**
 * Exception indicating that chosen path is not yet ported to new framework.
 * Default to older framework.
*/
struct NotPortedError : public SpktError {
  NotPortedError(const std::string& msg) :
    SpktError(msg) {
  }
  ~NotPortedError() throw() override {}
};

/**
 * Key to a map was not in the map
 */
struct InvalidKeyError : public SpktError {
  InvalidKeyError(const std::string &msg) :
    SpktError(msg) {
  }
  ~InvalidKeyError() throw () override {}
};

/**
 * An index was out of range in a collection.
 */
struct RangeError : public SpktError {
  RangeError(const std::string &msg) :
    SpktError(msg) {
  }
  ~RangeError() throw () override {}
};

/**
 * Invalid user input
 */
struct InputError : public SpktError {
  InputError(const std::string &msg) :
    SpktError(msg) {
  }
  ~InputError() throw () override {}
};

} // end of namespace sprockit
#endif
