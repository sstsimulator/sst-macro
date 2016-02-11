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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_SYSTOLICMXM_SYSTOLICMXM_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_SYSTOLICMXM_SYSTOLICMXM_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sstmac/software/libraries/compute/lib_compute_matrix.h>
#include <math.h>
#include <stdlib.h>

#include <sstmac/sstmacro.h>

namespace mini
{

    /**
     * Torus-wrap Matrix Multiply.
     */
    class systolicmxm : public mpi_app
    {
    public:
      long nrowblocks;
      long nlnkblocks;
      long ncolblocks;
      long blockrowsize;
      long blocklnksize;
      long blockcolsize;
      long nrownodes;
      long ncolnodes;
      long nrowcorespernode;
      long ncolcorespernode;
      bool systolicsetup;
      long threads;
      
      sstmac::sw::lib_compute_matrix* compute_matrix_;

      double rowfactor_;
      double colfactor_;

    private:
      /// Convert grid coordinates to a rank.
      /// Out of range blocks are wrapped a single time.
      void
      grid_to_rank(long row, long col, long &rank);
      /// Convert a rank to grid coordinates.
      /// Out of range blocks are wrapped a single time.
      void
      rank_to_grid(long rank, long &row, long &col);

    public:
      systolicmxm();

      ~systolicmxm() throw ();

      app*
      clone_type(){
        return new systolicmxm;
      }

      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "systolicmxm";
      }

    };

} //end namespace mini

#endif

