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
#include <sstmac/common/sst_event_fwd.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_handler_fwd.h>
#include <sstmac/hardware/common/connection_fwd.h>

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
  virtual void
  connect_input(
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
  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    event_handler* mod) = 0;

  /**
   * @brief payload_handler
   * @param port
   * @return The handler that will receive payloads from an SST link
   */
  virtual SST::Event::HandlerBase*
  payload_handler(int port) const = 0;

  /**
   * @brief credit_handler
   * @param port
   * @return The handler that will receive credits from an SST link
   */
  virtual SST::Event::HandlerBase*
  credit_handler(int port) const = 0;

  void
  init_links(sprockit::sim_parameters* params);

 protected:
  SSTIntegratedComponent(sprockit::sim_parameters* params, uint64_t id);

  SST::LinkMap* link_map_;


};

} /* end namespace sstmac */

#endif /* SSTMAC_MICRO_INTEGRATED_COMPONENT_H_ */

