/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/software/process/user_app.h>

namespace sstmac {
namespace sw {

class user_app_cxx :
  public user_app
{

 public:
  user_app_cxx(){}

  /** Destructor - do nothing */
  ~user_app_cxx() throw () {}

  sstmac::sw::app*
  clone_type() {
    return new user_app_cxx;
  }

  std::string
  to_string() const {
    return "user_mpiapp_cxx";
  }

};

} //end namespace sw
} //end namespace sstmac




