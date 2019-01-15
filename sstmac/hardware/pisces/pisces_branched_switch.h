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

#ifndef pisces_BRANCHED_SWITCH_H
#define pisces_BRANCHED_SWITCH_H

#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sstmac/hardware/pisces/pisces_buffer.h>
#include <sstmac/hardware/pisces/pisces_crossbar.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
#include <sstmac/hardware/pisces/pisces_stats_fwd.h>

namespace sstmac {
namespace hw {

/**
 @class PiscesBranchedSwitch
 A branched/hierarchical switch in the network that arbitrates/routes
 packets to the next link in the network
 */
class PiscesBranchedSwitch :
  public PiscesAbstractSwitch
{
  RegisterComponent("pisces_branched", NetworkSwitch, PiscesBranchedSwitch,
         "macro", COMPONENT_CATEGORY_NETWORK,
         "A branched/hierarchical network switch implementing the packet flow congestion model")
 public:
  PiscesBranchedSwitch(sprockit::sim_parameters::ptr& params, uint32_t id);

  int queueLength(int port) const override;

  virtual void connectOutput(sprockit::sim_parameters::ptr& params,
                 int src_outport, int dst_inport,
                 EventLink* link) override;

  virtual void connectInput(sprockit::sim_parameters::ptr& params,
                int src_outport, int dst_inport,
                EventLink* link) override;

  LinkHandler* creditHandler(int port) override;

  LinkHandler* payloadHandler(int port) override;

  Timestamp sendLatency(sprockit::sim_parameters::ptr& params) const override;

  Timestamp creditLatency(sprockit::sim_parameters::ptr& params) const override;

  virtual std::string toString() const override;

  virtual ~PiscesBranchedSwitch();

 private:
  int n_local_xbars_;
  int n_local_ports_;

  PiscesCrossbar* xbar_;

  struct input_port {
    PiscesBranchedSwitch* parent;
    PiscesMuxer* mux;

    int componentId() const {
      return parent->addr();
    }

    void handle(Event* ev);

    std::string toString() const {
      return parent->toString();
    }
  };

  std::vector<input_port> input_muxers_;
  std::vector<PiscesDemuxer*> output_demuxers_;

 private:
  void resizeBuffers();

  void initComponents(sprockit::sim_parameters::ptr& params);
};

}
}

#endif // pisces_BRANCHED_SWITCH_H
