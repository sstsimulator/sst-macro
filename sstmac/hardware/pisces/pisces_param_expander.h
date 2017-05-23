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

#ifndef pisces_PARAM_EXPANDER_H
#define pisces_PARAM_EXPANDER_H

#include <sstmac/hardware/common/param_expander.h>

namespace sstmac {
namespace hw {

class pisces_param_expander :
  public param_expander
{
   FactoryRegister("pisces | pisces", sstmac::param_expander, pisces_param_expander)
  public:
    virtual void
    expand(sprockit::sim_parameters* params) override;

  private:
    void expand_amm1_nic(sprockit::sim_parameters* params,
                         sprockit::sim_parameters* nic_params);

    void expand_amm1_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* switch_params,
                             bool set_xbar);

    void expand_amm1_memory(sprockit::sim_parameters* params,
                            sprockit::sim_parameters* mem_params);

    void expand_amm2_memory(sprockit::sim_parameters* params,
                            sprockit::sim_parameters* mem_params);

    void expand_amm3_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* switch_params);

    void expand_amm4_nic(sprockit::sim_parameters* params,
                         sprockit::sim_parameters* top_params,
                         sprockit::sim_parameters* nic_params);

    void expand_amm4_network(sprockit::sim_parameters* params,
                             sprockit::sim_parameters* top_params,
                             sprockit::sim_parameters* nic_params);

  private:
    double switch_bandwidth_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::switch_bandwidth_multiplier(params);
      }
    }

    int switch_buffer_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1;
      } else {
        return param_expander::switch_buffer_multiplier(params);
      }
    }

    double network_bandwidth_multiplier(sprockit::sim_parameters *params) const override {
      if (tiled_switch_){
        return 1.0;
      } else {
        return param_expander::network_bandwidth_multiplier(params);
      }
    }

    int buffer_depth_;

    bool tiled_switch_;

};

}
}

#endif // pisces_PARAM_EXPANDER_H