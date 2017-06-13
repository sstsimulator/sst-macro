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

#ifndef SSTMAC_SOFTWARE_AMI_AMI_H_INCLUDED
#define SSTMAC_SOFTWARE_AMI_AMI_H_INCLUDED

#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/messages/sst_message_fwd.h>

#include <sprockit/spkt_config.h>

namespace sstmac {
namespace ami {

  enum CAT {
    COMPUTE = 0,
    COMM = 1,
    DISP = 2,
    QUERY = 3,
    HW = 4
  };

  enum COMM_FUNC {
    COMM_SEND = 10101, //pretty much have to implement this one
    COMM_PMI_SEND, //process-manager send
  };

  /** Functions that block and must complete before returning */
  enum COMP_FUNC {
    COMP_TIME = 67, //the basic compute-for-some-time
    COMP_INSTR,
    COMP_EIGER
  };

  /** Functions that do not block and should return asynchronously */
  enum SERVICE_FUNC {
    COMP_DISKACCESS
  };

  const char*
  tostr(COMP_FUNC func);

  const char*
  tostr(SERVICE_FUNC func);

  const char*
  tostr(COMM_FUNC func);

  const char*
  tostr(CAT cat);

}
} //end of namespace sstmac

#if SPKT_HAVE_CPP11
namespace std {
template <>
struct hash<sstmac::ami::CAT> : public hash<int> { };
}
#endif

#endif