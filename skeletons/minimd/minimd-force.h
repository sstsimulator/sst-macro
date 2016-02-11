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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_FORCE_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_FORCE_H_INCLUDED

#include <minimd.h>
#include <lib_compute_minimd.h>
#include <sstmac/libraries/mpi/mpi_api.h>

namespace mini {

  /**
   * Take over the responsibilities of Force in miniMD.
   */
  class minimd::force : public sprockit::ptr_type {
  private:
    lib_compute_minimd* execution_;
    mpi_api* mpi_;

  public:
    typedef sprockit::refcount_ptr<minimd::force> ptr;

    virtual std::string 
    to_string() const{
        return "force";
    }

    /// As in miniMD, all key variables are public.
    double cutforce;

    void init(lib_compute_minimd* exe,
              mpi_api* mpi)
    {
      execution_ = exe;
      mpi_ = mpi;
    }

    void compute(const sprockit::refcount_ptr<atom>& a, const sprockit::refcount_ptr<neighbor>& n);
  };

} 

#endif
