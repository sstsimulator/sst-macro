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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUITMESSAGE_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUITMESSAGE_H_INCLUDED

#include <sstmac/hardware/router/routable_message.h>
#include <sstmac/hardware/packet/packet_switch.h>
#include <sstmac/common/messages/message_chunk.h>
#include <sprockit/debug.h>
#include <stack>

DeclareDebugSlot(circuit_path)

namespace sstmac {
namespace hw {

class circuit_message :
  public message_chunk,
  public routable_message
{

 public:
  typedef sprockit::refcount_ptr<circuit_message> ptr;
  typedef sprockit::refcount_ptr<const circuit_message> const_ptr;

  enum CIRCUIT_TYPE {
    SETUP, TEARDOWN, BLOCKED, TURNING, PATH_ACK, DATA
  };

 public:
  virtual ~circuit_message() {}

  int backoff_;
  std::stack<packet_switch*> stack_;
  timestamp ttl_;
  timestamp arrive_;

  circuit_message(const sst_message::ptr& orig,
                  long num_bytes, long byte_offset, CIRCUIT_TYPE t) :
    message_chunk(orig, num_bytes, byte_offset),
    routable_message(orig->toaddr(), orig->fromaddr()),
    circtype_(t), backoff_(0) {

  }

  virtual CIRCUIT_TYPE
  get_circ_type() const {
    return circtype_;
  }

  virtual void
  set_circ_type(CIRCUIT_TYPE t) {
    circtype_ = t;
  }

  virtual std::string
  to_string() const {
    std::string t;
    switch(circtype_) {
      case PATH_ACK:
        t = "PATH_ACK";
        break;
      case SETUP:
        t = "SETUP";
        break;
      case BLOCKED:
        t = "BLOCKED";
        break;
      case TEARDOWN:
        t = "TEARDOWN";
        break;
      case TURNING:
        t = "TURNING";
        break;
      case DATA:
        t = "DATA";
        break;
    }
    return "circuitmessage("+t+")";
  }

  void
  serialize_order(sprockit::serializer& ser){
    spkt_throw(sprockit::unimplemented_error,
     "circuit_message::serialize_order");
  }

 protected:
  CIRCUIT_TYPE circtype_;


};
}
}

#endif

