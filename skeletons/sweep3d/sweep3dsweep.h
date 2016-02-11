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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_SWEEP3D_SWEEP3DMAIN_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_SWEEP3D_SWEEP3DMAIN_H_INCLUDED

#include <sstmac/sstmacro.h>

/// A hello world example mpi skeleton application.
namespace sweep3d
{

    class sweep3dmain : public sstmac::sw::mpi_app
    {

    protected:
      int s_nx, s_ny, s_nz, s_omega, s_itr;
      double s_wg;
      int s_mk;

    public:
      sweep3dmain() {}

      virtual
      ~sweep3dmain() throw ()
      {
      }

      app*
      clone_type()
      {
        return new sweep3dmain;
      }

      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "sweep3D";
      }

      void
      sweep(int pex, int pey, int size, int rank, int nx, int ny, int nz,
          int omega, int mk);

      int
      get_east(int rank, int pex, int pey);
      int
      get_west(int rank, int pex, int pey);
      int
      get_south(int rank, int pex, int pey);
      int
      get_north(int rank, int pex, int pey);

    };

} // end of namespace sweep3d

#endif
