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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_COMM_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_COMM_H_INCLUDED

#include <minimd.h>
#include <minimd-atom.h>
#include <lib_compute_minimd.h>
#include <sstmac/libraries/mpi/mpi_api.h>

namespace mini
{

  /**
   * Mimic the communication routines in miniMD.
   */
  class minimd::comm : public sprockit::ptr_type  {

  private:
    lib_compute_minimd* execution_;
    mpi_api* mpi_;

    template<typename T>
      T
      min(T a, T b)
      {
        return (a < b ? a : b);
      }
    template<typename T>
      T
      max(T a, T b)
      {
        return (a > b ? a : b);
      }

  public:
    typedef sprockit::refcount_ptr<minimd::comm> ptr;

    virtual std::string
    to_string() const {
      return "comm";
    }

    std::vector<mpi_id> sendproc; // of dimension nswap
    std::vector<mpi_id> recvproc; // of dimension nswap
    std::vector<int> comm_send_size, comm_recv_size;
    std::vector<int> comm_reverse_send_size, comm_reverse_recv_size;
    int nswap; // the number of swaps to perform
    mpi_id procneigh[3][2]; // the 6 nearest neighbors.
    int procgrid[3]; // the processor grid.
    int need[3]; // how many processors we need in each dimension

    void
    init(lib_compute_minimd* exe, mpi_api* mpi)
    {
      nswap = -1;
      for (int r = 0; r < 3; ++r)
        {
          procgrid[r] = -100;
          need[r] = -100;
          for (int c = 0; c < 2; ++c)
            procneigh[r][c] = mpi_id(-100);
        }
      execution_ = exe;
      mpi_ = mpi;
    }

    /// Setup spatial decomposition.
    void
    setup(double cutneigh, atom::ptr atm)
    {
      sstmac::sw::mpi_comm* world = mpi_->comm_world();
      int nprocs = mpi_->comm_size(world);
      int rank = mpi_->comm_rank(world);
      const double prd[] =
        { atm->box.xprd, atm->box.yprd, atm->box.zprd };
      double area[] =
        { prd[0] * prd[1], prd[0] * prd[2], prd[1] * prd[2] };
      double bestsurf = 2 * area[0] * area[1] * area[2];
      // Figure out the best factorization for the grid.
      for (int ipx = 1; ipx <= nprocs; ++ipx)
        {
          if (nprocs % ipx == 0)
            {
              int nremain = nprocs / ipx;
              for (int ipy = 1; ipy <= nprocs; ++ipy)
                {
                  if (nremain % ipy == 0)
                    {
                      int ipz = nremain / ipy;
                      double surf = area[0] / ipx / ipy + area[1] / ipx / ipz
                          + area[2] / ipy / ipz;
                      if (surf < bestsurf)
                        {
                          bestsurf = surf;
                          procgrid[0] = ipx;
                          procgrid[1] = ipy;
                          procgrid[2] = ipz;
                        }
                    }
                }
            }
        }

#if 0
      // Use mpi_cart will result a different procneigh result than mpich2,
      // besides, the mpi_cart_create will produce additional call record to other mpi calls
      int reorder = 0;
      int periods[3] =
        { 1, 1, 1};
      int myloc[3] =
        { 0, 0, 0};
      sstmac::sw::mpi_comm* cartesian;

      mpi_->mpi_cart_create(world,3,procgrid, periods, reorder, cartesian);
      mpi_->mpi_cart_get(cartesian,3,procgrid, periods, myloc);
      mpi_->mpi_cart_shift(cartesian,0,1,&procneigh[0][0].id,&procneigh[0][1].id);
      mpi_->mpi_cart_shift(cartesian,1,1,&procneigh[1][0].id,&procneigh[1][1].id);
      mpi_->mpi_cart_shift(cartesian,2,1,&procneigh[2][0].id,&procneigh[2][1].id);
      mpi_->comm_free(cartesian);
#else
      // I am not using the cart comm trick from miniMD because I still haven't
      // implemented the needed mpi functionality in the simulator.
      // Instead, we simply stick the nodes on the grid in sequential order.
      // Let
      //    stride = [procgrid[1]*procgrid[2], procgrid[2], 1]
      // A node with rank r has the coordinates:
      //    xpos = rank / stride[0]
      //    ypos = (rank - xpos*stride[0]) / stride[1]
      //    zpos = rank - xpos*stride[0] - ypos*stride[1]
      // Similarly, the grid location [xpos, ypos, zpos] has the rank
      //    xpos*stride[0] + ypos*stride[1] + zpos*stride[2]
      // Functionally, this has the same effect as a "vanilla" MPI_Cart_shift
      // with periodic boundaries under mpich2.
      int stride[] =
        { procgrid[1] * procgrid[2], procgrid[2], 1 };
      int myloc[] =
        { rank / stride[0], (rank - myloc[0] * stride[0]) / stride[1], rank
            - myloc[0] * stride[0] - myloc[1] * stride[1] };
      int nb[3] =
        { myloc[0], myloc[1], myloc[2] };

      atm->box.xlo = myloc[0] * atm->box.xprd / procgrid[0];
      atm->box.xhi = (myloc[0] + 1) * prd[0] / procgrid[0];
      atm->box.ylo = myloc[1] * atm->box.yprd / procgrid[1];
      atm->box.yhi = (myloc[1] + 1) * atm->box.yprd / procgrid[1];
      atm->box.zlo = myloc[2] * atm->box.zprd / procgrid[2];
      atm->box.zhi = (myloc[2] + 1) * atm->box.zprd / procgrid[2];

      // shift.  Make "source" the downward direction and "dest" the upward.
      for (int axis = 0; axis < 3; ++axis)
        {
          nb[axis] = (myloc[axis] > 0 ? myloc[axis] - 1 : procgrid[axis] - 1);
          procneigh[axis][0] = mpi_id(nb[0] * stride[0] + nb[1] * stride[1] + nb[2] * stride[2]);
          nb[axis] = (myloc[axis] < procgrid[axis] - 1 ? myloc[axis] + 1 : 0);
          procneigh[axis][1] = mpi_id(nb[0] * stride[0] + nb[1] * stride[1] + nb[2] * stride[2]);
          nb[axis] = myloc[axis];
        }
#endif // if 0
#if 0
      // Debug dump.
      std::cerr << rank << " dumping procneigh:\n";
      for(int axis = 0; axis < 3; ++axis)
        {
          std::cerr << "    procneigh[" << axis << "] = [" << procneigh[axis][0]
          << ", " << procneigh[axis][1] << "]\n";
        }
#endif // !0
      // The number of boxes from which I need information in each direction.
      for (int axis = 0; axis < 3; ++axis)
        {
          need[axis] = int(cutneigh * procgrid[axis] / prd[axis] + 1);
        }
      // Set up parameters for each exchange
      nswap = 0;
      for (int idim = 0; idim < 3; ++idim)
        {
          for (int ineed = 0; ineed < 2 * need[idim]; ++ineed)
            {
              if (sendproc.size() <= size_t(nswap))
                sendproc.resize(nswap + 1);
              if (recvproc.size() <= size_t(nswap))
                recvproc.resize(nswap + 1);
              if (ineed % 2 == 0)
                {
                  sendproc.at(nswap) = procneigh[idim][0];
                  recvproc.at(nswap) = procneigh[idim][1];
                }
              else
                {
                  sendproc.at(nswap) = procneigh[idim][1];
                  recvproc.at(nswap) = procneigh[idim][0];
                }
              nswap++;
            }
        }
      // finally, account for all the computation and allocations.
      //mpi_->compute(execution_->get("Comm::setup"));
      //unsigned int seedp = rank;
      //double randnoise = (double(rand_r(&seedp)) / RAND_MAX - 0.5) * 0.25;
      timestamp t(1.024e-05);
      execution_->compute(t);
    }

    /// Forward communicate (at every timestep)
    void
    communicate(atom::ptr)
    {
      static const mpi_tag tag(0);
      mpi_comm* world = mpi_->comm_world();
      int rank = world->rank();
      mpi_request* request;
      mpi_status* status;
      for (int iswap = 0; iswap < nswap; iswap++)
        {
          if (sendproc[iswap] != rank)
            {
              mpi_->irecv(comm_recv_size.at(iswap), mpi_type::mpi_double->id,
                  recvproc.at(iswap), tag, world, request);
              mpi_->send(comm_send_size.at(iswap), mpi_type::mpi_double->id,
                  sendproc.at(iswap), tag, world);
              mpi_->wait(&request, status);
            }
        }
    }

    /// Reverse communicate (at every timestep).
    void
    reverse_communicate(atom::ptr)
    {
      mpi_comm* world = mpi_->comm_world();
      int me = world->rank();
      mpi_request* request;
      mpi_status* status;
      for (int iswap = 0; iswap < nswap; iswap++)
        {
          if (sendproc[iswap] != me)
            {
              mpi_->irecv(comm_reverse_recv_size.at(iswap),
                  mpi_type::mpi_double->id, sendproc[iswap], mpi_tag(0), world,
                  request);
              mpi_->send(comm_reverse_send_size.at(iswap), mpi_type::mpi_double->id,
                  recvproc.at(iswap), mpi_tag(0), world);
              mpi_->wait(&request, status);
            }
        }
    }

    /// Exchange atoms that have left their boxes
    /// This will be guessestimated statistically.
    void
    exchange(atom::ptr atm)
    {
      //#define SSTMAC_DB(command) std::cerr << rank << " " << #command << "\n"; command
#define SSTMAC_DB(command) command
      static const mpi_tag tag(0);
      mpi_comm* world = mpi_->comm_world();
      mpi_id rank = world->rank();
      SSTMAC_DB( mpi_request* request );
      SSTMAC_DB( mpi_status* status );
      for (int idim = 0; idim < 3; ++idim)
        {
          // Only exchange if there's more than one processor in this dimension
          if (procgrid[idim] <= 1)
            continue;
          // Filling buffers etc.
          //SSTMAC_DB( mpi_->compute(execution_->get("Comm::exchange",  0,  0)) );
          // This is fast enough that we don't even worry about it (~ 1 us).
          // Get the estimated paramters.
          /// nsend, nrecv, and nrecv2 are the lengths of packed arrays of
          /// coordinate and velocity values.  It looks like the sends
          /// are almost always empty (based on trace files), which is
          /// very strange.
          int nsend = (6 * atm->nlocal) / 50;
          int nrecv1 = (6 * atm->neighatoms.at(procneigh[idim][1])) / 50;
          int nrecv2 = (6 * atm->neighatoms.at(procneigh[idim][0])) / 50;
          // The actual exchange.  First send nsend and receive nrecv1
          mpi_->send(1, mpi_type::mpi_int->id, procneigh[idim][0], tag, world);
          mpi_->recv(1, mpi_type::mpi_int->id, procneigh[idim][1], tag, world,
              status);
          if (procgrid[idim] > 2)
            {
              // send nsend and receive nrecv2
              mpi_->send(1, mpi_type::mpi_int->id, procneigh[idim][1], tag, world);
              mpi_->recv(1, mpi_type::mpi_int->id, procneigh[idim][0], tag, world,
                  status);
            }
          // Exchange packed data arrays
          SSTMAC_DB( mpi_->irecv(nrecv1, mpi_type::mpi_double->id, procneigh[idim][1], tag,
                  world, request) );
          SSTMAC_DB( mpi_->send(nsend, mpi_type::mpi_double->id, procneigh[idim][0], tag, world) );
          SSTMAC_DB( mpi_->wait(&request, status) );

          if (procgrid[idim] > 2)
            {
              // Exchange packed data arrays in the other direction
              SSTMAC_DB( mpi_->irecv(nrecv2, mpi_type::mpi_double->id, procneigh[idim][0], tag,
                      world, request) );
              SSTMAC_DB( mpi_->send(nsend, mpi_type::mpi_double->id, procneigh[idim][1], tag, world) );
              SSTMAC_DB( mpi_->wait(&request, status) );
            }
        }
#undef SSTMAC_DB
    }

    /// Make a list of border atoms to send to neighboring procs.
    void
    borders(atom::ptr at)
    {
      static const mpi_tag tag(0);
      at->nghost = 0;

      mpi_comm* world = mpi_->comm_world();
      mpi_id rank = world->rank();
      mpi_request* request;
      mpi_status* status;
      //#define SSTMAC_DB(command) std::cerr << rank << " " << #command << "\n"; command
#define SSTMAC_DB(command) command
      // Do swaps over 3 dimensions.
      int iswap = 0;
      for (int idim = 0; idim < 3; ++idim)
        {
          for (int ineed = 0; ineed < 2 * need[idim]; ++ineed)
            {
              // Get the estimated paramters.
              // TODO:  We definitely need to improve these parameters
              //        because the transmission load heavily depends on them
              SSTMAC_DB( int nsend = execution_->get_int("Comm::borders::nsend", iswap) );
              SSTMAC_DB( int nrecv = execution_->get_int("Comm::borders::nrecv", iswap) );
              if (comm_send_size.size() <= size_t(iswap))
                {
                  SSTMAC_DB( comm_send_size.resize(iswap+1) );
                }
              if (comm_recv_size.size() <= size_t(iswap))
                {
                  SSTMAC_DB( comm_recv_size.resize(iswap+1) );
                }
              if (comm_reverse_send_size.size() <= size_t(iswap))
                {
                  SSTMAC_DB( comm_reverse_send_size.resize(iswap+1) );
                }
              if (comm_reverse_recv_size.size() <= size_t(iswap))
                {
                  SSTMAC_DB( comm_reverse_recv_size.resize(iswap+1) );
                }
              SSTMAC_DB( comm_send_size.at(iswap) = nsend * 3 ); // Atom::comm_size is 3
              SSTMAC_DB( comm_recv_size.at(iswap) = nrecv * 3 ); // Same as above.
              SSTMAC_DB( comm_reverse_send_size.at(iswap) = nrecv * 3 ); // Same as above.
              SSTMAC_DB( comm_reverse_recv_size.at(iswap) = nsend * 3 ); // Same as above.
              SSTMAC_DB( mpi_->send(1, mpi_type::mpi_int->id, sendproc.at(iswap), tag, world) );
              SSTMAC_DB( mpi_->recv(1, mpi_type::mpi_int->id, recvproc.at(iswap), tag, world, status) );
              SSTMAC_DB( mpi_->irecv(nrecv*3, mpi_type::mpi_double->id,
                      recvproc.at(iswap), tag, world, request) );
              SSTMAC_DB( mpi_->send(nsend*3, mpi_type::mpi_double->id,
                      sendproc.at(iswap), tag, world) );
              SSTMAC_DB( mpi_->wait(&request, status) );
              SSTMAC_DB( ++iswap );
              at->nghost += nrecv;
            }
        }
#undef SSTMAC_DB
    }
  };

}

#endif
