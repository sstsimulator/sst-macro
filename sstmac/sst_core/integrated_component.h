/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
#include <sprockit/factories/factory.h>
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sstmac/hardware/common/connection_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/link.h>
#include <sst/core/linkMap.h>
#include <sst/core/params.h>
#include <sst/core/element.h>
#include <sst/core/elementinfo.h>
#include <sst/core/component.h>

using SST::ComponentId_t;
using SST::Params;
using SST::ComponentDoc;
using SST::ElementInfoParam;
using SST::ElementInfoPort2;
using SST::ElementInfoStatistic;
using SST::SST_ELI_getMajorNumberFromVersion;
using SST::SST_ELI_getMinorNumberFromVersion;
using SST::SST_ELI_getTertiaryNumberFromVersion;

#define SSTMAC_VALID_PORTS \
   {"input %(out)d %(in)d",  "Will receive new payloads here",      {}}, \
   {"output %(out)d %(in)d", "Will receive new acks(credits) here", {}}, \
   {"in-out %(out)d %(in)d", "Will send/recv payloads here",        {}}, \
   {"rtr",                   "Special link to Merlin router",       {}}

#define RegisterComponent(name,parent,cls,lib,cat,desc) \
  FactoryRegister(name,parent,cls,desc) \
  SST_ELI_DOCUMENT_SUBCOMPONENT_SLOTS() \
  SST_ELI_REGISTER_COMPONENT(cls,lib,#cls,SST_ELI_ELEMENT_VERSION(7,1,0),desc,cat) \
  SST_ELI_DOCUMENT_PARAMS() \
  SST_ELI_DOCUMENT_PORTS(SSTMAC_VALID_PORTS) \
  SST_ELI_DOCUMENT_STATISTICS() \
  cls(SST::ComponentId_t id, SST::Params& params) : \
    cls(make_spkt_params_from_sst_params(params), id, nullptr){}

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
 * for converting an sst/macro standalone event_component into a
 * a SST::Component compatible with integration
 */
class SSTIntegratedComponent
  : public SST::Component
{
 public:
  /**
   * @brief connect_input All of these classes should implement the
   *        connectable interface
   * @param params
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) = 0;

  /**
   * @brief connect_output  All of these classes should implement
   *                        the connectable interface
   * @param params
   * @param src_outport
   * @param dst_inport
   * @param mod
   */
  virtual void connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) = 0;

  /**
   * @brief payload_handler
   * @param port
   * @return The handler that will receive payloads from an SST link
   */
  virtual SST::Event::HandlerBase* payload_handler(int port) const = 0;

  /**
   * @brief credit_handler
   * @param port
   * @return The handler that will receive credits from an SST link
   */
  virtual SST::Event::HandlerBase* credit_handler(int port) const = 0;

  void init_links(sprockit::sim_parameters* params);

  protected:
  SSTIntegratedComponent(sprockit::sim_parameters* params, uint64_t id);

  SST::LinkMap* link_map_;

};

} /* end namespace sstmac */

#else
#define RegisterComponent(name,parent,cls,lib,cat,desc) \
  FactoryRegister(name,parent,cls,desc)

#define RegisterSubcomponent(name,parent,cls,lib,interfaceStr,desc) \
  FactoryRegister(name,parent,cls,desc)

#define COMPONENT_CATEGORY_UNCATEGORIZED  0x00
#define COMPONENT_CATEGORY_PROCESSOR      0x01
#define COMPONENT_CATEGORY_MEMORY         0x02
#define COMPONENT_CATEGORY_NETWORK        0x04
#define COMPONENT_CATEGORY_SYSTEM         0x08
#endif

#endif /* SSTMAC_MICRO_INTEGRATED_COMPONENT_H_ */
