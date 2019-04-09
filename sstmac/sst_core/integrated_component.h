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

#ifndef SSTMAC_MICRO_INTEGRATED_COMPONENT_H_
#define SSTMAC_MICRO_INTEGRATED_COMPONENT_H_

#include <sstmac/common/sstmac_config.h>
#include <sprockit/sim_parameters_fwd.h>
#include <sprockit/factory.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sstmac/hardware/common/connection_fwd.h>

#define SSTMAC_VALID_PORTS \
   {"input %(out)d %(in)d",  "Will receive new payloads here",      {}}, \
   {"output %(out)d %(in)d", "Will receive new acks(credits) here", {}}, \
   {"in-out %(out)d %(in)d", "Will send/recv payloads here",        {}}, \
   {"rtr",                   "Special link to Merlin router",       {}}

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/link.h>
#include <sst/core/linkMap.h>
#include <sst/core/params.h>
#include <sst/core/element.h>
#include <sst/core/elementinfo.h>
#include <sst/core/component.h>
#include <sst/core/subcomponent.h>

#define SSTMAC_VALID_PORTS \
   {"input %(out)d %(in)d",  "Will receive new payloads here",      {}}, \
   {"output %(out)d %(in)d", "Will receive new acks(credits) here", {}}, \
   {"in-out %(out)d %(in)d", "Will send/recv payloads here",        {}}, \
   {"rtr",                   "Special link to Merlin router",       {}}

#define RegisterSSTComponent(name,parent,cls,lib,cat,desc) \
  FactoryRegister(name,parent,cls,desc) \
  SST_ELI_DOCUMENT_SUBCOMPONENT_SLOTS() \
  SST_ELI_REGISTER_COMPONENT(cls,lib,#cls,SST_ELI_ELEMENT_VERSION(8,0,0),desc,cat) \
  SST_ELI_DOCUMENT_PARAMS() \
  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS) \
  cls(SST::ComponentId_t id, SST::Params& params) : \
    cls(make_spkt_params_from_sst_params(params), id, nullptr){}

#define RegisterComponent(name,parent,cls,lib,cat,desc) \
  RegisterSSTComponent(name,parent,cls,lib,cat,desc) \
  SST_ELI_DOCUMENT_STATISTICS()

#define RegisterSubcomponent(name,parent,cls,lib,interfaceStr,desc) \
  FactoryRegister(name,parent,cls,desc) \
  SST_ELI_REGISTER_SUBCOMPONENT(cls,lib,#cls,desc,interfaceStr) \
  protected: \
  cls(SST::ComponentId_t id, SST::Params& params) : \
    cls(make_spkt_params_from_sst_params(params), id, nullptr){}

sprockit::sim_parameters* make_spkt_params_from_sst_params(SST::Params& map);

namespace sstmac {

/**
 * @brief The SSTIntegratedComponent class  Provides common functionality
 * for converting an sst/macro standalone Component into a
 * a SST::Component compatible with integration
 */
class SSTIntegratedComponent
  : public SST::Component
{
 public:
  /**
   * @brief connectInput All of these classes should implement the
   *        Connectable interface
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connectInput(int src_outport, int dst_inport, EventLink::ptr&& link) = 0;

  /**
   * @brief connectOutput  All of these classes should implement
   *                        the Connectable interface
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connectOutput(int src_outport, int dst_inport, EventLink::ptr&& link) = 0;

  /**
   * @brief payloadHandler
   * @param port
   * @return The handler that will receive payloads from an SST link
   */
  virtual SST::Event::HandlerBase* payloadHandler(int port) = 0;

  /**
   * @brief creditHandler
   * @param port
   * @return The handler that will receive credits from an SST link
   */
  virtual SST::Event::HandlerBase* creditHandler(int port) = 0;

  void initLinks(SST::Params& params);

 protected:
  SSTIntegratedComponent(SST::Params& params, uint32_t id);

  SST::LinkMap* link_map_;

};

} /* end namespace sstmac */

#else

#define SST_ELI_DECLARE_BASE(x) \
  SPKT_DECLARE_BASE(x)

#define SST_ELI_DECLARE_CTOR(...) \
  SPKT_DECLARE_CTOR(SPKT_FORWARD_AS_ONE(__VA_ARGS__))

#define SST_ELI_DECLARE_DEFAULT_CTOR() \
  SPKT_DECLARE_DEFAULT_CTOR()

#define SST_ELI_DECLARE_DEFAULT_INFO()

#define SST_ELI_DOCUMENT_STATISTICS(...)

#define SST_ELI_ELEMENT_VERSION(...)

#define SST_ELI_DOCUMENT_PORTS(...)

//if this macro is used, then this is ONLY registered for core
#define SST_ELI_REGISTER_COMPONENT(cls,lib,name,version,desc,cat)

#define SST_ELI_REGISTER_DERIVED_COMPONENT(base,cls,lib,name,version,desc,cat) \
  SPKT_REGISTER_DERIVED(base,cls,lib,name,desc)

#define SST_ELI_REGISTER_DERIVED(base,cls,lib,name,version,desc) \
  SPKT_REGISTER_DERIVED(base,cls,lib,name,desc)

#define COMPONENT_CATEGORY_UNCATEGORIZED  0x00
#define COMPONENT_CATEGORY_PROCESSOR      0x01
#define COMPONENT_CATEGORY_MEMORY         0x02
#define COMPONENT_CATEGORY_NETWORK        0x04
#define COMPONENT_CATEGORY_SYSTEM         0x08
#endif

#endif /* SSTMAC_MICRO_INTEGRATED_COMPONENT_H_ */
