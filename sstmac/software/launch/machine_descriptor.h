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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_MACHINE_DESCRIPTOR_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_MACHINE_DESCRIPTOR_H_INCLUDED


namespace sstmac {
namespace sw {

class machine_descriptor  {

 public:
  machine_descriptor(long num, std::string top) :
    num_network_slots_(num), topology_name_(top) {
  }

  virtual
  ~machine_descriptor() {
  }

  virtual std::string
  to_string() const {
    return "machine_descriptor";
  }

  long num_network_slots_;
  std::string topology_name_;


};
}
}

#endif

