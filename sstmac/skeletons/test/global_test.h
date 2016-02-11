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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_GLOBAL_TEST_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_GLOBAL_TEST_H_INCLUDED

#include <sstmac/software/process/app.h>

#include <sstmac/software/process/global.h>

namespace sstmac {
namespace sw {

class global_test : public app
{
 public:
  global_test() {}

  virtual
  ~global_test() {

  }

  virtual void
  skeleton_main();

  virtual app*
  clone_type() {
    return new global_test;
  }

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "global_test";
  }

 private:
  static global_int class_global_;

};
}
} //end of namespace sstmac

#endif

