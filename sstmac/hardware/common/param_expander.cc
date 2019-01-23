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

#include <sstmac/hardware/common/param_expander.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace hw {

double
ParamExpander::networkBandwidthMultiplier(SST::Params& params) const
{
  SST::Params top_params = params.find_prefix_params("topology");
  if (top_params->has_param("redundant")){
    std::vector<int> red;
    top_params.find_array("redundant", red);
    int sum = 0;
    for (int i=0; i < red.size(); ++i){
      sum += red[i];
    }
    return ((double)sum) / red.size();
  } else {
    return 1.0;
  }
}

double
ParamExpander::switchBandwidthMultiplier(SST::Params& params) const
{
  SST::Params sw_params = params.find_prefix_params("switch");
  if (sw_params->has_param("geometry")){
    std::vector<int> geom;
    sw_params.find_array("geometry", geom);
    int prod = 1;
    for (int i=0; i < geom.size(); ++i){
      prod *= geom[i];
    }
    return prod;
  } else {
    return 1.0;
  }
}

int
ParamExpander::switchBufferMultiplier(SST::Params& params) const
{
  SST::Params top_params = params.find_prefix_params("topology");
  if (top_params->has_param("redundant")){
    std::vector<int> red;
    top_params.find_array("redundant", red);
    int sum = 0;
    for (int i=0; i < red.size(); ++i){
      sum += red[i];
    }
    return sum;
  } else {
    return 1;
  }
}

}
}
