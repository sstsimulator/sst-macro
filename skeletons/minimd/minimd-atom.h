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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_ATOM_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_ATOM_H_INCLUDED

#include <minimd.h>

namespace mini
{

  // Ignored types.
  class lib_compute_minimd;

  /**
   * Take over the responsibilities of class Atom in miniMD.
   */
  class minimd::atom  :
    public sprockit::ptr_type
  {

  public:
    typedef sprockit::refcount_ptr<minimd::atom> ptr;

    virtual std::string
    to_string() const
    {
      return "atom";
    }
    struct Box
    {
      double xprd, yprd, zprd;
      double xlo, xhi;
      double ylo, yhi;
      double zlo, zhi;
      void
      set_prd(double x, double y, double z)
      {
        xprd = x;
        yprd = y;
        zprd = z;
      }
    };

    int natoms;
    std::vector<int> neighatoms; // Added to keep track of everybodies nlocal.
    int nlocal;
    int nghost;
    int nmax;
    Box box;

    void
    init(lib_compute_minimd*, mpi_api*)
    {
      // all atom:: operations are rolled into runtime overhead for other calls.
    }
  };

}

#endif
