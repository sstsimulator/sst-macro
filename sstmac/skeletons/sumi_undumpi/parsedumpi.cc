/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */


#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/runtime.h>
#include <sstmac/skeletons/sumi_undumpi/parsedumpi.h>
#include <sstmac/skeletons/sumi_undumpi/parsedumpi_callbacks.h>
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

namespace sumi {

SpktRegister("parsedumpi", sstmac::sw::app, parsedumpi,
            "application for parsing and simulating dumpi traces");

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
}

mpi_api* 
parsedumpi::mpi() 
{
  if (mpi_) return mpi_;

  mpi_ = get_api<mpi_api>();
  return mpi_;
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


