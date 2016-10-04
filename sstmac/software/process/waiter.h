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

#ifndef SSTMAC_SOFTWARE_PROCESS_WAITER_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_WAITER_H_INCLUDED

#include <string>

namespace sstmac {
namespace sw {

/**
 * The main interface for something that can wait in a certain way for an event that is important to it.
 */
class waiter
{

 public:
  virtual std::string
  to_string() const override {
    return "waiter";
  }

  virtual bool
  is_blocking() const {
    return false;
  }

  virtual bool
  is_callback() const {
    return false;
  }

  virtual
  ~waiter() {}

  virtual void
  wait() {}


};

class callback_waiter : public waiter
{

 public:
  bool
  is_callback() const {
    return true;
  }


};
}
} // end of namespace sstmac
#endif

