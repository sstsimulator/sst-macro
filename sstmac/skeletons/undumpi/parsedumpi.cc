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

#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/runtime.h>
#include <sstmac/skeletons/undumpi/parsedumpi.h>
#include <sstmac/skeletons/undumpi/parsedumpi_callbacks.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/dumpi_util/dumpi_util.h>
#include <sumi-mpi/mpi_api.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <stdio.h>
#include <errno.h>
#include <cstring>
#include <fstream>
#include <algorithm>

RegisterKeywords(
"parsedumpi_timescale",
"parsedumpi_terminate_percent",
"parsedumpi_print_progress",
"launch_dumpi_metaname",
"dumpi_metaname",
);

namespace sumi{

using namespace sstmac::hw;


parsedumpi::parsedumpi(sprockit::sim_parameters* params, software_id sid,
                       sstmac::sw::operating_system* os) :
  app(params, sid, os),
  mpi_(nullptr)
{
  fileroot_ = params->reread_param("dumpi_metaname");

  timescaling_ = params->get_optional_double_param("parsedumpi_timescale", 1);

  print_progress_ = params->get_optional_bool_param("parsedumpi_print_progress", true);

  percent_terminate_ = params->get_optional_double_param("parsedumpi_terminate_percent", -1);

  exact_mpi_times_ = params->get_optional_bool_param("parsedumpi_exact_times", false);
}


//
// Wait!  That's not good news at all!
//
parsedumpi::~parsedumpi() throw()
{
}

//
// Parse the file please (this needs an update, there is no reason
// for it to return a TRACEREC (which should not be uppercase).
// Need to figure out how to give the caller access to header/footer recs.
//
void parsedumpi::skeleton_main()
{
  int rank = this->tid();
  mpi_ = get_api<mpi_api>();
  sstmac::sw::dumpi_meta* meta = new   sstmac::sw::dumpi_meta(fileroot_);
  parsedumpi_callbacks cbacks(this);
  std::string fname = sstmac::sw::dumpi_file_name(rank, meta->dirplusfileprefix_);
  // Ready to go.

  //only rank 0 should print progress
  bool print_my_progress = rank == 0 && print_progress_;
  //only rank 0 should cause termination
  double my_percent_terminate = rank == 0 ? percent_terminate_ : -1;

  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(mpi(), &sumi::transport::deadlock_check));
  sstmac::runtime::enter_deadlock_region();

  cbacks.parse_stream(fname.c_str(), print_my_progress, my_percent_terminate);

  if (rank == 0) {
    std::cout << "Parsedumpi finalized on rank 0 - trace "
      << fileroot_ << " successful!" << std::endl;
  }

  sstmac::runtime::exit_deadlock_region();

#if !SSTMAC_INTEGRATED_SST_CORE
  // TODO make this work with @integrated_core
  if (percent_terminate_ >= 0){
    //we must stop the event manager now
    mpi()->os()->event_mgr()->stop();
  }
#endif

}


}