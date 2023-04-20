/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

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

#ifndef MEMORYMODEL_H_
#define MEMORYMODEL_H_

#include <sstmac/hardware/common/connection.h>
#include <sprockit/factory.h>
#include <sprockit/debug.h>

#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/memory/memory_id.h>
#include <sstmac/sst_core/integrated_component.h>

DeclareDebugSlot(memory)
#define mem_debug(...) \
    debug_printf(sprockit::dbg::memory, "Memory on Node %d: %s", int(nodeid_), sprockit::sprintf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

class MemoryModel : public SubComponent
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_API(sstmac::hw::MemoryModel, hw::Node*)
#else
  SST_ELI_DECLARE_BASE(MemoryModel)
  SST_ELI_DECLARE_DEFAULT_INFO()
  SST_ELI_DECLARE_CTOR(uint32_t,SST::Params&,hw::Node*)
#endif

  struct Request {
    uint32_t bytes;
    uintptr_t addr;
    uint32_t rspId;
  };

  struct RequestHandlerBase {
    virtual void handle(Request* req) = 0;
  };

  template <class T, class Fxn>
  class RequestHandler : public RequestHandlerBase {
   public:
    RequestHandler(T* t, Fxn f) :
      t_(t), f_(f)
    {
    }

    void handle(Request* req) override {
      (t_->*f_)(req);
    }
   private:
    T* t_;
    Fxn f_;
  };

  template <class T, class Fxn>
  static RequestHandlerBase* makeHandler(T* t, Fxn f){
    return new RequestHandler<T,Fxn>(t,f);
  }

  MemoryModel(uint32_t id, SST::Params& params, hw::Node* node);

  static void deleteStatics();

  virtual ~MemoryModel();

  /**
   * @brief access Call for entire flows, with a rate-limiting byte delay
   * @param bytes
   * @param byte_delay How long it takes for a memory read/write instruction to be issued
   * @param cb
   */
  virtual void accessFlow(uint64_t bytes, TimeDelta byte_request_delay, Callback* cb) = 0;

  /**
   * @brief access Call for individal requests
   * @param linkId
   * @param req
   */
  virtual void accessRequest(int linkId, Request* req) = 0;

  int initialize(RequestHandlerBase* handler);

  NodeId addr() const;

 protected:
  NodeId nodeid_;
  Node* parent_node_;
  std::vector<RequestHandlerBase*> rsp_handlers_;

};

class NullMemoryModel : public MemoryModel
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT_DERIVED(
    NullMemoryModel,
    "macro",
    "null_memory",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a memory model that models nothing",
    sstmac::hw::MemoryModel)
#else
  SST_ELI_REGISTER_DERIVED(
    MemoryModel,
    NullMemoryModel,
    "macro",
    "null",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "implements a memory model that models nothing")
#endif

  NullMemoryModel(uint32_t id, SST::Params& params, hw::Node* node) :
    MemoryModel(id, params, node)
  {
  }

  std::string toString() const override { return "null memory"; }

  void accessFlow(uint64_t, TimeDelta, Callback *) override {}

  void accessRequest(int linkId, Request* req) override;

};

}
} /* namespace sstmac */
#endif /* MEMORYMODEL_H_ */
