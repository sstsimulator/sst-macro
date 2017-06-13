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

#ifndef SIMPLE_MEMORYMODEL_H_
#define SIMPLE_MEMORYMODEL_H_

#include <sstmac/hardware/memory/memory_model.h>

namespace sstmac {
namespace hw {

/**
 * @brief The logp_memory_model class implements memory operations using
 *        a very basic LogGP model for simulating delays.
 */
class logp_memory_model : public memory_model
{
  FactoryRegister("logP | simple | LogP | logp", memory_model,logp_memory_model,
              "Implements a simple memory model that is just a single link")
 public:
  logp_memory_model(sprockit::sim_parameters* params, node* nd);

  virtual ~logp_memory_model();

  std::string to_string() const override {
    return "logGP memory model";
  }

  void access(long bytes, double max_bw, callback* cb) override;

  double max_single_bw() const override {
    return bw_;
  }

 protected:
  class link  {
   public:
    link(double bw, timestamp lat) :
      bw_(bw), lat_(lat), last_access_(0) {
    }

    ~link() { }

    timestamp
    new_access(timestamp now, long size, double max_bw);

   protected:
    double bw_;
    timestamp lat_;
    timestamp last_access_;

  };

 protected:
  link* link_;

  double bw_;

  timestamp lat_;

};

}
} /* namespace sstmac */
#endif /* SIMPLE_MEMORYMODEL_H_ */