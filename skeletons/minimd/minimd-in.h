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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_IN_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_IN_H_INCLUDED

#include <minimd.h>

namespace mini {

  // Ignored types.
  class lib_compute_minimd;

  /**
   * Take over the responsibilities of struct In in miniMD.
   */
  class minimd::in : public sprockit::ptr_type
  {
  public:
    typedef sprockit::refcount_ptr<minimd::in> ptr;

    virtual std::string 
    to_string() const{
        return "minimd:in";
    }

    /// Problem size.
    int nx, ny, nz;
    /// Target temperature.
    double t_request;
    /// Desired density.
    double rho;

    void init(lib_compute_minimd*, mpi_api*) {
      // all in:: operations are rolled into runtime overhead for other calls.
    }
  };

} 

#endif
