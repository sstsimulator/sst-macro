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

#include "sweep3dsweep.h"
#include <stdio.h>
#include <math.h>

using namespace sweep3d;
using namespace sstmac;
using namespace sstmac::sw;

static sprockit::debug dbg_;

namespace sweep3d {

    SpktRegisterApp("sweep3d", sweep3dmain);

    void
    sweep3dmain::consume_params(sprockit::sim_parameters* params)
    {
      s_nx = params->get_optional_int_param("sweep3d_nx", 240);
      s_ny = params->get_optional_int_param("sweep3d_ny", 240);
      s_nz = params->get_optional_int_param("sweep3d_nz", 240);
      s_itr = params->get_optional_int_param("sweep3d_itr", 100);
      s_omega = params->get_optional_int_param("sweep3d_omega", 10);
      s_wg = params->get_optional_double_param("sweep3d_wg", 1e-6);
      s_mk = params->get_optional_int_param("sweep3d_mk", 1);
    }

    void
    sweep3dmain::skeleton_main()
    {

      // initialize mpi
      if (mpi() == 0)
        throw sprockit::null_error("sweep3dmain:  mpiapi pointer is null.");

      timestamp start = mpi()->init();

      const mpi_id size = mpi()->comm_size(mpi()->comm_world());

      int pex = (int) sqrt(size);
      int pey;

      for (; pex * ((int) (size / pex)) != size; pex--)
        ;
      pey = size / pex;

      mpi_comm* world(mpi()->comm_world());
      mpi_id rank(world->rank());

      if (rank == 0)
        {
          std::cout <<
                "------------------------------------------------------------\n"
               << "Performing 2D decomposition:\n"
               << "     PE-X:         " << pex << "\n"
               << "     PE-Y:         " << pey << "\n"
               << "     Nx:           " << s_nx << "\n"
               << "     Ny:           " << s_ny << "\n"
               << "     Nz:           " << s_nz << "\n"
               << "     Omega:        " << s_omega << "\n"
               << "     Wg:           " << s_wg << "\n"
               << "     mk:           " << s_mk << "\n"
               << "\n";
        }

      // simulate broadcasting 1 double from rank 0 with no real payload
      //const int count=1024;
      //const mpiid root(0);
      //mpi()->bcast(count, mpitype::mpi_double->id, root, world);

      timestamp start_sweep = mpi()->wtime();
      sweep(pex, pey, size, rank, s_nx, s_ny, s_nz, s_omega, s_mk);
      timestamp end_sweep = mpi()->wtime();

      //mpi()->barrier(mpi()->comm_world());
      // finalize
      timestamp done = mpi()->finalize();

      if (rank == 0)
        {
          std::cout <<
                "------------------------------------------------------------\n"
               << "Simulation Finalized\n"
               << "\n"
               << "End Time:          " << done.sec() << " / " << (done.sec() * s_itr) << "\n"
               << "Sweep Time:        " << (end_sweep.sec() - start_sweep.sec()) << " / "
               << (end_sweep.sec() - start_sweep.sec()) * s_itr << "\n"
               << "------------------------------------------------------------\n";
        }
    }

    void
    sweep3dmain::sweep(int pex, int pey, int size, int rank, int nx, int ny,
        int nz, int omega, int mk)
    {
      int tilex = ceil(nx / pex);
      int tiley = ceil(ny / pey);

      int sweepnumber;
      int zcounter;

      int north = get_north(rank, pex, pey);
      int south = get_south(rank, pex, pey);
      int east = get_east(rank, pex, pey);
      int west = get_west(rank, pex, pey);

      const mpi_id northrank(north);
      const mpi_id southrank(south);
      const mpi_id eastrank(east);
      const mpi_id westrank(west);

      mpi_comm* commworld(mpi()->comm_world());
      mpi_tag tag(0);
      mpi_status* status = new mpi_status;

      timestamp w((tilex * tiley) * s_wg * mk);

      if (rank == 0)
        {
          std::cout << "  -> Nx/PE-X:      " << tilex << "\n"
             << "  -> Ny/PE-Y:      " << tiley << "\n"
             << "  -> W:            " << ((tilex * tiley) * s_wg) << "\n"
             << "\n";
        }

      for (sweepnumber = 0; sweepnumber < 8; sweepnumber++)
        {
          for (zcounter = 0; zcounter < nz; zcounter += mk)
            {
              if ((sweepnumber == 0) || (sweepnumber == 1) || (sweepnumber == 2)
                  || (sweepnumber == 3))
                {
                  if (north > -1)
                    {
                      mpi()->recv(mk * tilex * omega, mpi_type::mpi_double->id,
                          northrank, tag, commworld, status);
                    }
                }

              if ((sweepnumber == 4) || (sweepnumber == 5) || (sweepnumber == 6)
                  || (sweepnumber == 7))
                {
                  if (south > -1)
                    {
                      mpi()->recv(mk * tilex * omega, mpi_type::mpi_double->id,
                          southrank, tag, commworld, status);
                    }
                }

              if ((sweepnumber == 0) || (sweepnumber == 1) || (sweepnumber == 6)
                  || (sweepnumber == 7))
                {
                  if (west > -1)
                    {
                      mpi()->recv(mk * tiley * omega, mpi_type::mpi_double->id,
                          westrank, tag, commworld, status);
                    }
                }

              if ((sweepnumber == 2) || (sweepnumber == 3) || (sweepnumber == 4)
                  || (sweepnumber == 5))
                {
                  if (east > -1)
                    {
                      mpi()->recv(mk * tiley * omega, mpi_type::mpi_double->id,
                          eastrank, tag, commworld, status);
                    }
                }

              // some compute goes here
              this->compute(w);

              if ((sweepnumber == 0) || (sweepnumber == 1) || (sweepnumber == 2)
                  || (sweepnumber == 3))
                {
                  if (south > -1)
                    {
                      mpi()->send(mk * tilex * omega, mpi_type::mpi_double->id,
                          southrank, tag, commworld);
                    }
                }

              if ((sweepnumber == 4) || (sweepnumber == 5) || (sweepnumber == 6)
                  || (sweepnumber == 7))
                {
                  if (north > -1)
                    {
                      mpi()->send(mk * tilex * omega, mpi_type::mpi_double->id,
                          northrank, tag, commworld);
                    }
                }

              if ((sweepnumber == 0) || (sweepnumber == 1) || (sweepnumber == 6)
                  || (sweepnumber == 7))
                {
                  if (east > -1)
                    {
                      mpi()->send(mk * tiley * omega, mpi_type::mpi_double->id,
                          eastrank, tag, commworld);
                    }
                }

              if ((sweepnumber == 2) || (sweepnumber == 3) || (sweepnumber == 4)
                  || (sweepnumber == 5))
                {
                  if (west > -1)
                    {
                      mpi()->send(mk * tiley * omega, mpi_type::mpi_double->id,
                          westrank, tag, commworld);
                    }
                }
            }
        }
    }

    int
    sweep3dmain::get_north(int rank, int pex, int pey)
    {
      if (rank < pex)
        {
          return -1;
        }
      else
        {
          return rank - pex;
        }
    }

    int
    sweep3dmain::get_south(int rank, int pex, int pey)
    {
      if (rank >= (pex * (pey - 1)))
        {
          return -1;
        }
      else
        {
          return rank + pex;
        }
    }

    int
    sweep3dmain::get_east(int rank, int pex, int pey)
    {
      if (((rank + 1) % pex) == 0)
        {
          return -1;
        }
      else
        {
          return rank + 1;
        }
    }

    int
    sweep3dmain::get_west(int rank, int pex, int pey)
    {
      if ((rank % pex) == 0)
        {
          return -1;
        }
      else
        {
          return rank - 1;
        }
    }

} //end namespace

