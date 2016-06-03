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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUITSWITCH_H_
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_CIRCUIT_CIRCUITSWITCH_H_

#include <sstmac/hardware/packet/cycle_accurate/cycle_accurate_switch.h>
#include <sstmac/hardware/circuit/circuit_router.h>
#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

class circuit_switch : public cycle_accurate_switch
{
 public:
  typedef spkt_unordered_map<int, int> maptype;

  circuit_switch();

  virtual
  ~circuit_switch() {
  }

  virtual std::string
  to_string() const {
    return sprockit::printf("circuitswitch(%ld)", long(my_addr_));
  }

 protected:
  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual int
  check_inport(int inport, bool &clock);

  void
  do_route(cycle_accurate_payload::ptr msg, int inport, int &outport,
           bool &kill);

  bool
  check_circuit(cycle_accurate_payload::ptr msg, int in, int out, bool &kill);

  void
  modify_circuits(cycle_accurate_payload::ptr msg, int in, int out);

  //virtual injector*
  //get_new_injector(sprockit::sim_parameters* params);

  maptype circuitsin_; //points from in to out
  maptype circuitsout_; //point from out to in

  bool blocked_protocol_;

  circuit_router* circrouter_;

#if 0
  class circuitpacketinjector : public packet_injector
  {

   protected:
    circuitpacketinjector(sprockit::sim_parameters* params,
                          circuit_switch* par) :
      packet_injector(params, par) {
      circpar_ = par;
    }

    circuit_switch* circpar_;
   public:

    virtual std::string
    to_string() const {
      return "circuitpacketinjector";
    }

    virtual
    ~circuitpacketinjector() {
    }

    static ptr
    construct(sprockit::sim_parameters* params, circuit_switch* par) {
      return ptr(new circuitpacketinjector(params, par));
    }

    void send(long nbytes, long byte_offset, sst_message* msg);

  };
#endif

};

}
}

#endif /* CIRCUITSWITCH_H_ */

