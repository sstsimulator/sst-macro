/*
//@HEADER
// ************************************************************************
//
//                          integrated_component.h
//                         sst-macro
//              Copyright (C) 2015 Sandia Corporation
//
// Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
// the U.S. Government retains certain rights in this software.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
// 1. Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright
// notice, this list of conditions and the following disclaimer in the
// documentation and/or other materials provided with the distribution.
//
// 3. Neither the name of the Corporation nor the names of the
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Questions? Contact David S. Hollman (dshollm@sandia.gov)
//
// ************************************************************************
//@HEADER
*/

#ifndef SSTMAC_MICRO_INTEGRATED_COMPONENT_H_
#define SSTMAC_MICRO_INTEGRATED_COMPONENT_H_

#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters_fwd.h>

#include <sst/core/link.h>
#include <sst/core/linkMap.h>
#include <sst/core/params.h>
#include <sst/core/element.h>
#include <sstmac/sst_core/message_event_wrapper.h>


namespace sstmac {

class SSTSelfEventWrapper : public SST::Event
{
 public:
  virtual void
  run() = 0;
};

class SSTEventEvent : public SSTSelfEventWrapper
{
 public:
  SSTEventEvent(event* ev) : ev_(ev) {}

  void
  run() {
    ev_->execute();
  }

 private:
  event* ev_;
};

class SSTHandlerEvent : public SSTSelfEventWrapper
{
 public:
  SSTHandlerEvent(event_handler* handler, sst_message*msg) :
    msg_(msg),
    handler_(handler)
  {
  }

  void
  run() {
    handler_->handle(msg_);
  }

 private:
  sst_message* msg_;
  event_handler* handler_;
};

// lightweight layer in between integrated components and SST core, useful for common helper functions, etc
class SSTIntegratedComponent
  : public SST::Component
{
 public:
  void handle_event(SST::Event* ev){
    SSTMessageEvent* mev = static_cast<SSTMessageEvent*>(ev);
    handle(mev->message());
  }

  virtual void
  handle(sst_message* msg) = 0;

  virtual void
  init(unsigned int phase);

  void
  handle_self_link(SST::Event* ev);

 protected:
  SSTIntegratedComponent(
    SST::ComponentId_t id,
    SST::Params& params);

  void configure_self_link();

  virtual void
  init_factory_params(sprockit::sim_parameters* params) = 0;

  SST::SimTime_t
  extra_delay(timestamp t) const;

  SST::LinkMap* link_map_;
  SST::Link* self_link_;
  sprockit::sim_parameters* params_;
  static SST::TimeConverter* time_converter_;

};

} /* end namespace sstmac */

#endif /* SSTMAC_MICRO_INTEGRATED_COMPONENT_H_ */

