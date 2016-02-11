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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUIT_ROUTER_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUIT_ROUTER_H_INCLUDED


namespace sstmac {
namespace hw {

class circuit_router : public virtual sprockit::ptr_type
{

 protected:
  bool is_circuit_switching_;
  int channels_;

  circuit_router() {
    is_circuit_switching_ = false;
    channels_ = 1;
  }
 public:

  virtual void
  path_is_good(node_id goingto, int fromport, int toport) = 0;

  virtual void
  path_teardown(int fromport, int toport) = 0;

  virtual std::string
  to_string() const {
    return "circuit_router";
  }

  void
  set_circ_switching() {
    is_circuit_switching_ = true;
  }

  void
  set_channels(int ch) {
    channels_ = ch;
  }


};

}
}

#endif

