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

#ifndef SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_PARSEDUMPI_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_PARSEDUMPI_H_INCLUDED

#include <sstmac/software/process/app.h>
#include <sumi-mpi/mpi_api_fwd.h>
#include <sumi-mpi/mpi_comm/mpi_comm_fwd.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sstmac/hardware/topology/topology.h>

namespace sumi {

/**
 * A refactored dumpi parser to read the newer binary-format dumpi
 * trace files.  Discards support for older-style ascii-based
 * files since nobody was using those except us anyway.
 *
 * TODO:  Fix this to read the metafile rather than the trace files directly
 * (would avoid hack needed where MPI world size is smaller than the total
 * cores allocated to the job).
 */
class parsedumpi : public sstmac::sw::app
{
  friend class parsedumpi_unit_test;
  friend class parsedumpi_callbacks;

  FactoryRegister("parsedumpi | dumpi", sstmac::sw::app, parsedumpi,
              "application for parsing and simulating dumpi traces")
 public:
  parsedumpi(sprockit::sim_parameters* params, sstmac::sw::software_id sid,
             sstmac::sw::operating_system* os);

  /// Wait!  That's not good news at all!
  virtual ~parsedumpi() throw ();

  mpi_api* mpi() {
    return mpi_;
  }

  bool exact_mpi_times() const {
    return exact_mpi_times_;
  }

  virtual void skeleton_main();

 private:
  /// The fileroot we plan to parse.
  std::string fileroot_;

  /// The time scaling factor.
  double timescaling_;

  mpi_api* mpi_;

  bool print_progress_;

  bool exact_mpi_times_;

  double percent_terminate_;

  std::string metafilename_;


};

}

#endif // ! SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_PARSEDUMPI_H_INCLUDED