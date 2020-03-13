/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

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

#ifndef SSTMAC_HARDWARE_NETWORK_SWITCHES_NETWORKSWITCH_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_SWITCHES_NETWORKSWITCH_H_INCLUDED



#include <sprockit/factory.h>
#include <sprockit/debug.h>

#include <sstmac/hardware/common/connection.h>
#include <sstmac/hardware/router/router_fwd.h>
#include <sstmac/hardware/topology/topology_fwd.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sstmac/sst_core/integrated_component.h>
#endif

#include <vector>

DeclareDebugSlot(network_switch)
#define switch_debug(...) \
  debug_printf(sprockit::dbg::network_switch, "Switch %d: %s", \
    int(addr()), sprockit::sprintf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

/**
 * @brief The NetworkSwitch class
 * A class encapsulating a network switch that packets must traverse on the network.
 * The network switch performs both routing computations and congestion modeling.
 */
class NetworkSwitch :
  public ConnectableComponent
{
 public:
  SST_ELI_DECLARE_BASE(NetworkSwitch)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(uint32_t,SST::Params&)

  virtual void init(unsigned int phase);

  virtual ~NetworkSwitch();

  SwitchId addr() const {
    return my_addr_;
  }

  virtual void deadlockCheck(){}

  /**
   * @brief queue_length
   * Compute the number of packets waiting on the switch. The queue length
   * is a multiple of the ``system'' packet size, which can be different from the
   * packet size used by SST/macro congestion models. For example,
   * the packet size of the system beings simulated might be 100B, but SST/macro
   * might be doing congestion computations on units of 1024B.
   * @param port The port to check the queue length of
   * @return The queue length as an integer number of packets waiting
   */
  virtual int queueLength(int port, int vc) const = 0;

 protected:
  NetworkSwitch(uint32_t id, SST::Params& params);

  SwitchId my_addr_;
  Topology* top_;

};


}
}

#endif
