/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_LU_LUSWEEP_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_LU_LUSWEEP_H_INCLUDED

#include <sstmac/sstmacro.h>

namespace lusweep
{
    class lumain : public sstmac::sw::mpi_app
    {
    protected:
      int s_nx, s_ny, s_nz, s_itr, s_inorm;
      double s_wg_jacu, s_wg_jacld, s_wg_buts, s_wg_blts;
      double s_wg_rhs1, s_wg_rhs2, s_wg_rhs3, s_wg_rhs4;

    public:
      lumain(){}

      virtual ~lumain() throw (){ }

      app*
      clone_type()
      {
        return new lumain;
      }

      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "lusweep";
      }

      /** Overrides thread::run virtual function.
       * The task thread context is started by a call into this function. */
      void
      sweep(int pex, int pey, int size, int rank, int nx, int ny, int nz);
      void
      lower(int north, int south, int east, int west, int tilex, int tiley,
          int nz);
      void
      upper(int north, int south, int east, int west, int tilex, int tiley,
          int nz);
      void
      rhs(int north, int east, int south, int west, int tilex, int tiley,
          int nz);
      void
      exchange(int recvfrom, int sentto, int recvlen, int sendlen);

      int
      get_east(int rank, int pex, int pey);
      int
      get_west(int rank, int pex, int pey);
      int
      get_south(int rank, int pex, int pey);
      int
      get_north(int rank, int pex, int pey);

      //place holder routine for communication simulation
      void 
      bcast_inputs();
      void 
      error();
      void
      pintgr();
      void
      ssor(int niter);
      void
      l2norm();
    };

} // end of namespace lusweep

#endif
