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

#ifndef SSTMAC_SOFTWARE_SKELETONS_TEST_MPIPINGPONG_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_TEST_MPIPINGPONG_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>

#include <sprockit/debug.h>

DeclareDebugSlot(mpi_ping_pong)

#define ping_pong_debug(...) \
  debug_printf(sprockit::dbg::mpi_ping_pong, __VA_ARGS__)

namespace sstmac {
namespace sw {

/**
 * Basic MPI ping-pong.
 */
class mpi_ping_pong : public mpi_app
{

 public:
  mpi_ping_pong(){}

  /// Goodbye.
  virtual
  ~mpi_ping_pong() throw ();

  /// Get a copy.
  virtual app*
  clone_type(){
    return new mpi_ping_pong;
  }

  /// Go.
  void
  skeleton_main();

  virtual void
  consume_params(sprockit::sim_parameters* params);

  virtual std::string
  to_string() const {
    return "mpipingpong";
  }

 protected:
  /// How many iterations we do.
  int iterations_, count_;

};

}
} // end of namespace sstmac

#endif

