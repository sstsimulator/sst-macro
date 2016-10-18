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

 public:
  parsedumpi(sprockit::sim_parameters* params, sstmac::sw::software_id sid,
             sstmac::sw::operating_system* os);

  /// Wait!  That's not good news at all!
  virtual
  ~parsedumpi() throw ();

  mpi_api* mpi();

  /// Parse the tracefile.
  virtual void
  skeleton_main();

 private:
  /// The fileroot we plan to parse.
  std::string fileroot_;

  /// The time scaling factor.
  double timescaling_;

  mpi_api* mpi_;

  bool print_progress_;

  double percent_terminate_;

  std::string metafilename_;


};

}

#endif // ! SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_PARSEDUMPI_H_INCLUDED

