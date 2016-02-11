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

#include "lusweep.h"
#include <stdio.h>
#include <math.h>

using namespace sstmac;
using namespace sstmac::sw;

namespace lusweep {

    SpktRegisterApp("lusweep", lumain);

 void
lumain::consume_params(sprockit::sim_parameters* params)
{
  s_nx = params->get_optional_int_param("lusweep_nx", 64);
  s_ny = params->get_optional_int_param("lusweep_ny", 64);
  s_nz = params->get_optional_int_param("lusweep_nz", 64);
  s_itr = params->get_optional_int_param("lusweep_itr", 250);
  s_inorm = params->get_optional_int_param("lusweep_inorm", 250);

  s_wg_rhs1 = params->get_optional_double_param("lusweep_rhs1", 1e-6);
  s_wg_rhs2 = params->get_optional_double_param("lusweep_rhs2", 1e-6);
  s_wg_rhs3 = params->get_optional_double_param("lusweep_rhs3", 1e-6);
  s_wg_rhs4 = params->get_optional_double_param("lusweep_rhs4", 1e-6);

  s_wg_jacld = params->get_optional_double_param("lusweep_jacld", 1e-6);
  s_wg_jacu = params->get_optional_double_param("lusweep_jacu", 1e-6);
  s_wg_blts = params->get_optional_double_param("lusweep_blts", 1e-6);
  s_wg_buts = params->get_optional_double_param("lusweep_buts", 1e-6);
}


// Overrides thread::run virtual function
// The task thread context is started by a call into this function
void
lumain::skeleton_main()
{

  // initialize mpi
  if (mpi() == 0)
    throw sprockit::null_error("lumain:  mpiapi pointer is null.");

  timestamp start = mpi()->init();

  mpi_comm* world = mpi()->comm_world();

  const int size = world->size();


  int pex = (int) sqrt(size);
  int pey;

  for (; pex * ((int) (size / pex)) != size; pex--)
    ;
  pey = size / pex;

  int rank = world->rank();


  if (rank == 0)
    {
      std::cout
          << "------------------------------------------------------------\n"
        << "Performing 2D decomposition:\n"
        << "     PE-X:         " << pex << "\n"
        << "     PE-Y:         " << pey << "\n"
        << "     Nx:           " << s_nx << "\n"
        << "     Ny:           " << s_ny << "\n"
        << "     Nz:           " << s_nz << "\n"
        << "\n";
    }

  //place holder routine mimicing communication
  bcast_inputs();
  ssor(0);
  ssor(s_itr - 1);
  error();
  pintgr();

  // simulate broadcasting 1 double from rank 0 with no real payload
  //const int count=1024;
  //const mpiid root(0);
  //mpi()->bcast(count, mpitype::mpi_double, root, world);

  timestamp start_sweep = mpi()->wtime();
  sweep(pex, pey, size, rank, s_nx, s_ny, s_nz);
  timestamp end_sweep = mpi()->wtime();

  //mpi()->barrier(mpi()->comm_world());
  // finalize
  timestamp done = mpi()->finalize();

  if (rank == 0)
    {
      std::cout
          << "------------------------------------------------------------\n"
         << "Simulation Finalized\n"
         << "\n"
         << "End Time:          " << done.sec() << " / "
         << (done.sec() * s_itr) << "\n"
         << "Sweep Time:        "
         << (end_sweep.sec() - start_sweep.sec()) << " / "
         << (end_sweep.sec() - start_sweep.sec()) * s_itr << "\n"
         << "------------------------------------------------------------\n";
    }
}


void
lumain::sweep(int pex, int pey, int size, int rank, int nx, int ny, int nz)
{
  int tilex = ceil(nx / pex);
  int tiley = ceil(ny / pey);

  int north = get_north(rank, pex, pey);
  int south = get_south(rank, pex, pey);
  int east = get_east(rank, pex, pey);
  int west = get_west(rank, pex, pey);

  if (rank == 0)
    {
      std::cout << "  -> Nx/PE-X:      " << tilex << "\n"
         << "  -> Ny/PE-Y:      " << tiley << "\n"
         << "\n";
    }

  for (int itr_count = 0; itr_count < s_itr; itr_count++)
    {
      if (rank == 0)
        {
          std::cout << "Iteration " << itr_count << "...\n";
        }

      lower(north, south, east, west, tilex, tiley, s_nz);
      upper(north, south, east, west, tilex, tiley, s_nz);

      rhs(north, south, east, west, tilex, tiley, s_nz);
    }
}

void
lumain::lower(int north, int south, int east, int west, int tilex,
    int tiley, int nz)
{
  mpi_comm* commworld(mpi()->comm_world());
  mpi_tag tag(0);
  mpi_status* status = new mpi_status;
  const mpi_id northrank(north);
  const mpi_id southrank(south);
  const mpi_id eastrank(east);
  const mpi_id westrank(west);

  timestamp w_jacld(s_wg_jacld * (tilex * tiley));
  timestamp w_blts(s_wg_blts * (tilex * tiley));

  for (int zcounter = 0; zcounter < nz; zcounter++)
    {
      this->compute(w_jacld);

      if (north > -1)
        {
          mpi()->recv(tiley * 5, mpi_type::mpi_double->id, northrank, tag,
              commworld, status);
        }

      if (east > -1)
        {
          mpi()->recv(tilex * 5, mpi_type::mpi_double->id, eastrank, tag,
              commworld, status);
        }

      this->compute(w_blts);

      if (south > -1)
        {
          mpi()->send(tiley, mpi_type::mpi_double->id, southrank, tag,
              commworld);
        }

      if (west > -1)
        {
          mpi()->send(tilex, mpi_type::mpi_double->id, westrank, tag, commworld);
        }
    }
}

void
lumain::upper(int north, int south, int east, int west, int tilex,
    int tiley, int nz)
{
  mpi_comm* commworld(mpi()->comm_world());
  mpi_tag tag(0);
  mpi_status* status = new mpi_status;

  const mpi_id northrank(north);
  const mpi_id southrank(south);
  const mpi_id eastrank(east);
  const mpi_id westrank(west);

  const timestamp w_jacu(s_wg_jacu * (tilex * tiley));
  const timestamp w_buts(s_wg_buts * (tilex * tiley));

  for (int zcounter = 0; zcounter < nz; zcounter++)
    {
      this->compute(w_jacu);

      if (north > -1)
        {
          mpi()->recv(tiley * 5, mpi_type::mpi_double->id, northrank, tag,
              commworld, status);
        }

      if (east > -1)
        {
          mpi()->recv(tilex * 5, mpi_type::mpi_double->id, eastrank, tag,
              commworld, status);
        }

      this->compute(w_buts);

      if (south > -1)
        {
          mpi()->send(tiley, mpi_type::mpi_double->id, southrank, tag,
              commworld);
        }

      if (west > -1)
        {
          mpi()->send(tilex, mpi_type::mpi_double->id, westrank, tag, commworld);
        }
    }
}

void
lumain::rhs(int north, int south, int east, int west, int tilex, int tiley,
    int nz)
{
  const timestamp w_rhs1(s_wg_rhs1 * (tilex * tiley * nz));
  const timestamp w_rhs2(s_wg_rhs2 * (tilex * tiley * nz));
  const timestamp w_rhs3(s_wg_rhs3 * (tilex * tiley * nz));
  const timestamp w_rhs4(s_wg_rhs4 * (tilex * tiley * nz));

  this->compute(w_rhs1);

  exchange(north, south, 10 * tilex * nz, 10 * tilex * nz);
  exchange(south, north, 10 * tilex * nz, 10 * tilex * nz);

  this->compute(w_rhs2);

  exchange(east, west, 10 * tiley * nz, 10 * tiley * nz);
  exchange(west, east, 10 * tiley * nz, 10 * tiley * nz);

  this->compute(w_rhs3);
  this->compute(w_rhs4);
}

void
lumain::exchange(int recvfrom, int sendto, int recvlen, int sendlen)
{
  mpi_comm* commworld(mpi()->comm_world());
  mpi_tag tag(99);
  mpi_status* status = new mpi_status;

  mpi_request* req;

  const mpi_id recvfromrank(recvfrom);
  const mpi_id sendtorank(sendto);

  int rank = mpi()->comm_rank(commworld);

  if (recvfrom > -1)
    {
      mpi()->irecv(recvlen, mpi_type::mpi_double->id, recvfromrank, tag, commworld, req);
      mpi()->wait(&req, status);
      //mpi()->recv(recvlen, mpitype::mpi_double, recvfromrank, tag, commworld, status);
    }

  if (sendto > -1)
    {
      mpi()->send(sendlen, mpi_type::mpi_double->id, sendtorank, tag, commworld);
    }
}

int
lumain::get_north(int rank, int pex, int pey)
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
lumain::get_south(int rank, int pex, int pey)
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
lumain::get_east(int rank, int pex, int pey)
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
lumain::get_west(int rank, int pex, int pey)
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

void
lumain::bcast_inputs()
{
  //simulating bcast input params
  const mpi_id root(0);
  mpi_comm* world = mpi()->comm_world();

  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_double->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_double->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_double->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
  mpi()->bcast(1, mpi_type::mpi_int->id, root, world);
}

void 
lumain::error()
{
    mpi()->allreduce(5, mpi_type::mpi_double->id, mpi_op::sum, mpi()->comm_world());
}

void
lumain::pintgr()
{
    mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi()->comm_world());
    mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi()->comm_world());
    mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::sum, mpi()->comm_world());
}

void
lumain::ssor(int iter)
{
    //compute the L2 norms of newton iteration residuals
    l2norm();
    mpi()->barrier(mpi()->comm_world());

    //compute the max-norms of newton iteration corrections
    if((iter + 1) % s_inorm == 0)
    {
        l2norm();
    }

    //compute the max-norms of newton iteration resituals
    if((iter + 1) % s_inorm == 0 || 
        iter == s_itr)
    {
        l2norm();
    }

    mpi()->allreduce(1, mpi_type::mpi_double->id, mpi_op::max, mpi()->comm_world());
}

void
lumain::l2norm()
{
    mpi()->allreduce(5, mpi_type::mpi_double->id, mpi_op::sum, mpi()->comm_world());
}

}
