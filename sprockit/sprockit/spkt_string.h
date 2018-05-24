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

#ifndef SPKT_STRING_H
#define SPKT_STRING_H

#include <sstream>
#include <sprockit/spkt_config.h>
#include <sprockit/spkt_printf.h>
#include <cctype>
#include <algorithm>

namespace sprockit {

template<typename WritableRangeT>
void to_upper(WritableRangeT& input)
{
  for(char& ch : input) {
    ch = (char)std::toupper(ch);
  }
}

template<typename WritableRangeT>
void to_lower(WritableRangeT& input)
{
  for(char& ch : input) {
    ch = (char)std::tolower(ch);
  }
}

template<typename SequenceT>
void trim(SequenceT& input)
{
  auto check_isspace = [](decltype(*input.begin())& ch) { return (bool)std::isspace(ch); };
  const auto& first_non_space = std::find_if_not(input.begin(), input.end(), check_isspace);
  if(first_non_space == input.end()) {
    input = SequenceT(input.begin(), input.begin()+1);
  }
  else {
    const auto& last_non_space = std::find_if_not(input.rbegin(), input.rend(), check_isspace);
    if(last_non_space == input.rbegin() + 1) {
      input = SequenceT(first_non_space, input.end());
    }
    else {
      input = SequenceT(first_non_space, last_non_space.base() + 1);
    }
  }
}

}


#endif // SPKT_STRING_H
