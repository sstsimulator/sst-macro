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

#ifndef SIMPLE_MEMORYMODEL_H_
#define SIMPLE_MEMORYMODEL_H_

#include <sstmac/hardware/memory/memory_model.h>

namespace sstmac {
namespace hw {

/**
 * @brief The LogPMemoryModel class implements memory operations using
 *        a very basic LogGP model for simulating delays.
 */
class LogPMemoryModel : public MemoryModel
{
 public:
#if SSTMAC_INTEGRATED_SST_CORE
  SST_ELI_REGISTER_SUBCOMPONENT(
    LogPMemoryModel,
    "macro",
    "logp_memory",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Implements a simple memory model that is just a single link",
    sstmac::hw::MemoryModel)
#else
  SST_ELI_REGISTER_DERIVED(
    MemoryModel,
    LogPMemoryModel,
    "macro",
    "logp",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Implements a simple memory model that is just a single link")
#endif

  LogPMemoryModel(uint32_t id, SST::Params& params, hw::Node* node);

  ~LogPMemoryModel() override;

  std::string toString() const override {
    return "logGP memory model";
  }

  void accessFlow(uint64_t bytes, TimeDelta byte_delay, Callback* cb) override;

  void accessRequest(int linkId, Request* req) override;


 protected:
  class Link  {
   public:
    Link(TimeDelta byte_delay, TimeDelta lat) :
      byte_delay_(byte_delay), lat_(lat), last_access_() {
    }

    ~Link() { }

    /**
     * @brief newAccess
     * @param now
     * @param size
     * @param max_bw
     * @return The deltaT from now the access will finish
     */
    TimeDelta newAccess(Timestamp now, uint64_t size, TimeDelta min_byte_delay);

   protected:
    TimeDelta byte_delay_;
    TimeDelta lat_;
    Timestamp last_access_;

  };

 protected:
  Link* link_;

  TimeDelta min_byte_delay_;

  TimeDelta lat_;

};

}
} /* namespace sstmac */
#endif /* SIMPLE_MEMORYMODEL_H_ */
