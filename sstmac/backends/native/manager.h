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

#ifndef SSTMAC_BACKENDS_NATIVE_MANAGER_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_MANAGER_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_manager_fwd.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/software/launch/job_launcher_fwd.h>
#include <sprockit/factory.h>

DeclareDebugSlot(timestamp);

namespace sstmac {
namespace native {

/**
 * The owner of a discrete-event network simulation.
 *
 * All time progression is handled by this object
 * and messages between nodes are managed here as well.
 */
class Manager {

 public:
  Manager(SST::Params& params, ParallelRuntime* rt);

  static int computeMaxNproc(sprockit::SimParametersPtr& param);

  static int computeMaxNprocForApp(sprockit::SimParametersPtr& app_params);

#if !SSTMAC_INTEGRATED_SST_CORE
  ~Manager() throw ();

  Timestamp run(Timestamp until);

  void stop();

  void finish();

  sstmac::hw::Interconnect* interconnect() const {
    return interconnect_;
  }

 private:
  void start();

  EventManager* EventManager_;

  bool running_;

  sstmac::hw::Interconnect* interconnect_;
  ParallelRuntime* rt_;
#endif
};

}
} // end of namespace sstmac.

#endif
