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

#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

key_traits::category lib_compute_memmove::key_category("Memory");

lib_compute_memmove::lib_compute_memmove(sprockit::sim_parameters* params,
                                         software_id id, operating_system* os) :
  lib_compute_memmove(params, "libmemmove", id, os)
{
  key_cat_ = key_category;
}

lib_compute_memmove::lib_compute_memmove(sprockit::sim_parameters* params,
                                         const char* prefix, software_id sid,
                                         operating_system* os) :
  lib_compute_inst(params, prefix, sid, os)
{
  key_cat_ = key_category;

  access_width_bytes_ = params->get_optional_int_param("lib_compute_access_width", 64) / 8;
}

void
lib_compute_memmove::do_access(long bytes)
{
  //if (bytes == 0){
  //  return;
  //}
  long num_loops = bytes / access_width_bytes_;
  int nflops = 0;
  int nintops = 1; //memmove instruction
  compute_loop(num_loops, nflops, nintops, access_width_bytes_);
}

void
lib_compute_memmove::read(long bytes)
{
  SSTMACBacktrace("memread");
  do_access(bytes);
}

void
lib_compute_memmove::write(long bytes)
{
  SSTMACBacktrace("memwrite");
  do_access(bytes);
}

void
lib_compute_memmove::copy(long bytes)
{
  SSTMACBacktrace("memcopy");
  do_access(bytes);
}


}
} //end of namespace sstmac