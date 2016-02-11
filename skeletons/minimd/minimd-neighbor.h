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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_NEIGHBOR_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_NEIGHBOR_H_INCLUDED

#include <minimd.h>
#include <lib_compute_minimd.h>
#include <sstmac/libraries/mpi/mpi_api.h>

namespace mini {

  /**
   * Take over the neighbor list management from miniMD.
   */
  class minimd::neighbor :
   public sprockit::ptr_type
  {
  private:
    lib_compute_minimd* execution_;
    mpi_api* mpi_;

  public:
    typedef sprockit::refcount_ptr<minimd::neighbor> ptr;

    virtual std::string 
    to_string() const{
        return "neighbor";
    }
    // As in miniMD, all key variables are public.
    int every; /// reneighbor every n steps.
    int nbinx, nbiny, nbinz;  // number of global bins.
    double cutneigh; // neighbor cutoff.

    
    void init(lib_compute_minimd* exe,
              mpi_api* mpi)
    {
      execution_ = exe;
      mpi_ = mpi;
    }

    /// This method is not in the real miniMD.
    void set_bins(int nx, int ny, int nz) {
      nbinx = nx; nbiny = ny; nbinz = nz;
    }

    /// This is a somewhat extensive function in the original.
    void setup() {
      execution_->compute("Neighbor::setup", NULL, NULL);
    }

    /// Build a neighbor list.
    void build(const sprockit::refcount_ptr<atom>& at) {
      execution_->compute("Neighbor::build", NULL, NULL);
    }
  };

} 

#endif
