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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_ARRIVE_MESSAGE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_LAUNCH_MESSAGES_ARRIVE_MESSAGE_H_INCLUDED

#include <sstmac/software/launch/launch_message.h>

#include <sstmac/software/launch/launch_info.h>

namespace sstmac {
namespace sw {

class arrive_message : public launch_event
{

 public:
  /**
   * Stringifier
   * @return String description
   */
  virtual std::string
  to_string() const override {
    return "arrive_message";
  }

};

}
}

#endif

