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


#include <mpi3d.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_type.h>
#include <sstmac/libraries/mpi/mpi_types/mpi_tag.h>
#include <sprockit/debug.h>
#include <stdio.h>

using namespace sstmac;
using namespace sstmac::sw;

namespace mini {

template<class T, class S>
inline T
check_cast(const S &s)
{
  if (s > std::numeric_limits<S>::max()) {
    throw sprockit::range_error("overflow");
  }
  if (s < std::numeric_limits<S>::min()) {
    throw sprockit::range_error("underflow");
  }
  return static_cast<T>(s);
}

  SpktRegisterApp("mpi3d", mpi3d);

  int mpi3d::active_workers_ = 0;

  std::ostream &
  operator <<(std::ostream&o, mpi3d::coor&c)
  {
    o << '{' << c[0] << ',' << c[1] << ',' << c[2] << '}';
    return o;
  }

  // Pointer types.
  

  void
  mpi3d::consume_params(sprockit::sim_parameters* params)
  {
    l = 100;
    g = 1;
    nfield = 1;
    nbyte = 8;

    nx = params->get_int_param("mpi3d_nx");

    ny = params->get_int_param("mpi3d_ny");

    nz = params->get_int_param("mpi3d_nz");
    gamma = 100e-9;
    verbose = false;
    nstep = 10;
    waitall = true;
  }

  //
  // Hi.
  //
  mpi3d::mpi3d() :
   compute_time(0), noiselevel(0.1), rngseed(0xF00D)
  {
  }

  //
  // Goodbye.
  //
  mpi3d::~mpi3d() throw ()
  {
  }

  //
  // Set compute time targets.
  //
  void
  mpi3d::set_compute(const timestamp &ts, double noise, unsigned int seed)
  {
    compute_time = ts;
    noiselevel = noise;
    rngseed = seed;
  }

  //
  // Go.
  //
  void
  mpi3d::skeleton_main()
  {
    timestamp start = mpi()->init();
    mpi_comm* world = mpi()->comm_world();
    comm_ = world;
    mpi_id rank = world->rank();
    mpi_id size = world->size();
    mpi_tag tag(0);

    if(nx < 0)
    	nx = sqrt(size);

    if(ny < 0)
    	ny = sqrt(size);

    if(nz < 0)
    	nz = 1;

    if (int(rank)>= nx * ny * nz)
      {
        mpi()->finalize();
        return;
      }

    if (compute_time != timestamp(0))
      {
        rng.seed(rngseed + rank);
      }

    init();

    std::vector<mpi_request*> irecv_reqs;
    std::vector<mpi_request*> isend_reqs;

    if (int(rank)== 0)
      {

          //SSTMAC_DEBUG << "mpi3d::run():  " << face_neighbors_.size()
          //  << " face neighbors, " << edge_neighbors_.size()
          //  << " edge neighbors, " << corner_neighbors_.size()
          //  << " corner neighbors\n";
      }
    for (long i = 0; i < nstep; i++)
      {

        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank)<< "} :  Iteration " << i << " of "
        //  << nstep << "\n";
        irecv_reqs.clear();
        isend_reqs.clear();


        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank)<< "} :  running irecv commands\n";
        run_irecv(face_nbyte_, face_neighbors_, irecv_reqs);
        //run_irecv(edge_nbyte_, edge_neighbors_, irecv_reqs);
        //run_irecv(corner_nbyte_, corner_neighbors_, irecv_reqs);


        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank)<< "} :  running isend commands\n";
        run_isend(face_nbyte_, face_neighbors_, isend_reqs);
        //run_isend(edge_nbyte_, edge_neighbors_, isend_reqs);
        //run_isend(corner_nbyte_, corner_neighbors_, isend_reqs);
        if (verbose)
          {
            std::cout << "waitall 1" << std::endl;
          }

        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank) << "} :  waiting for irecv commands\n";
        run_waitall(irecv_reqs);

        run_compute();

        if (verbose)
          {
            std::cout << "waitall 2" << std::endl;
          }

        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank) << "} :  waiting for isend commands\n";
        run_waitall(isend_reqs);

        //SSTMAC_DEBUG << "mpi3d::run {" << int(rank)<< "} :  Bottom of iterative loop\n";
      }


    //SSTMAC_DEBUG << "mpi3d::run {" << int(rank)<< "} :  finalizing mpi\n";
    timestamp done = mpi()->finalize();

    /*double runtime = (done - start).sec();
    if (int(rank)== 0)
      {
        static const size_t bsize = 1023;
        char *buffer = new char[bsize + 1];snprintf
        (buffer, bsize, "mpi3d run started at %f and finished at %f "
            "with a total runtime of %f seconds\n", start.sec(), done.sec(),
            runtime);
        buffer[bsize] = '\0'; // just to be sure.
        if (!fwrite(buffer, sizeof(char), strlen(buffer), stderr))
          {
            // Failed to write the buffer -- but there isn't much we can do about it
          }
        delete[] buffer;
        //     if(debugger::active()) debug() << "Peer " << rank << " started at " << start
        //               << " and finished at " << done << ", with a total runtime of "
        //               << runtime << " seconds\n";
      }*/
  }

  mpi3d::coor
  mpi3d::rank_to_coor(mpi_id rank)
  {
    coor ret;
    ret[0] = int(rank)/ (ny * nz);
    ret[1] = (int(rank)- ret[0] * ny * nz) / nz;
    ret[2] = int(rank)- ret[0] * ny * nz - ret[1] * nz;
    return ret;
  }

  mpi_id
  mpi3d::coor_to_rank(const coor &c)
  {
    return mpi_id(c[0] * ny * nz + c[1] * nz + c[2]);
  }

  mpi_id
  mpi3d::offset_to_rank(long dx, long dy, long dz)
  {
    return coor_to_rank(
        coor(my_coor_[0] + dx, my_coor_[1] + dy, my_coor_[2] + dz));
  }

  void
  mpi3d::run_irecv(int nbyte, const std::vector<mpi_id> &neighbors,
      std::vector<mpi_request*> &reqs)
  {
    for (unsigned long i = 0; i < neighbors.size(); i++)
      {
        mpi_request* req;
        mpi()->irecv(nbyte, mpi_type::mpi_double->id, neighbors[i], tag_, comm_, req);
        reqs.push_back(req);
        // Make the irecv call "not completely free"
        // This may later be replaced with a more reasonable approach
        //mpi()->compute(timestamp::exact_nsec(1));
      }
  }

  void
  mpi3d::run_isend(int nbyte, const std::vector<mpi_id> &neighbors,
      std::vector<mpi_request*> &reqs)
  {
    for (unsigned long i = 0; i < neighbors.size(); i++)
      {
        mpi_request* req;
        mpi()->isend(nbyte, mpi_type::mpi_double->id, neighbors[i], tag_, comm_, req);
        reqs.push_back(req);
        // Make the isend call "not completely free"
        // This may later be replaced with a more reasonable approach
        //mpi()->compute(timestamp::exact_nsec(1));
      }
  }

  void
  mpi3d::run_waitall(std::vector<mpi_request*> &reqs)
  {
    if (waitall)
      {
        std::vector<mpi_status> stats(reqs.size());
        if (reqs.size() > 0)
          {
            mpi()->waitall(reqs, stats);
          }
      }
    else
      {
        mpi_status stat;
        for (size_t i = 0; i < reqs.size(); i++)
          {

              //SSTMAC_DEBUG << "mpi3d[" << "..." << "]::run_waitall:  Waiting for request "
              //<< reqs[i] << " (" << i << " of " << reqs.size() - 1 << "\n";
            mpi()->wait(&reqs[i], &stat);

              //SSTMAC_DEBUG << "mpi3d[" << "..." << "]::run_waitall:  Request " << i << " of "
              //<< reqs.size() - 1 << " complete\n";
          }
      }
  }

  void
  mpi3d::run_compute()
  {
    /*if (compute_time != timestamp(0))
 {
 if (noiselevel != 0)
 {
 timestamp delay = compute_time + compute_time * noiselevel * rng();
 compute(delay);
 }
 else
 {
 compute(compute_time);
 }
 }*/

    lib_disp_->change_fill(GREEN);
    active_workers_++;

    //SSTMAC_DEBUG << "mpi3d: computing on " << nx << ", " << ny << ", " << nz << "\n";

    compute(compute_time);

    active_workers_--;
    lib_disp_->change_fill(BLACK);
  }

  void
  mpi3d::init_faces()
  {
    std::vector<long> ngrid(3);
    ngrid[0] = nx;
    ngrid[1] = ny;
    ngrid[2] = nz;
    face_nbyte_ = check_cast<int>(g * l * l * nfield * nbyte);
    face_neighbors_.clear();
    face_neighbors_.reserve(6);
    for (long i = -1; i <= 1; i += 2)
      {
        for (long j = 0; j < 3; j++)
          {
            coor neighbor(my_coor_);
            neighbor[j] = (neighbor[j] + ngrid[j] + i) % ngrid[j];
            mpi_id neighbor_rank = coor_to_rank(neighbor);
            if (neighbor_rank != my_rank_)
              {

                  {
                    //SSTMAC_DEBUG << "face: " << int(my_rank_) << " -> " << int(neighbor_rank) << " "
                    //    << my_coor_.to_string() << " -> " << neighbor.to_string() << " \n";
                  }
                face_neighbors_.push_back(neighbor_rank);
              }
          }
      }
  }

  void
  mpi3d::init_edges()
  {
    std::vector<long> ngrid(3);
    ngrid[0] = nx;
    ngrid[1] = ny;
    ngrid[2] = nz;
    edge_nbyte_ = check_cast<int>(g * g * l * nfield * nbyte);
    edge_neighbors_.clear();
    edge_neighbors_.reserve(12);
    for (long i = -1; i <= 1; i += 2)
      {
        for (long j = -1; j <= 1; j += 2)
          {
            long cases[3][2] =
                {
                    { 0, 1 },
                    { 0, 2 },
                    { 1, 2 } };
            for (long k = 0; k < 3; k++)
              {
                long a = cases[k][0];
                long b = cases[k][1];
                coor neighbor(my_coor_);
                neighbor[a] = (neighbor[a] + ngrid[a] + i) % ngrid[a];
                neighbor[b] = (neighbor[b] + ngrid[b] + j) % ngrid[b];
                mpi_id neighbor_rank = coor_to_rank(neighbor);
                if (neighbor_rank != my_rank_)
                  {

                      {
                        //SSTMAC_DEBUG << "edge: " << int(my_rank_) << " -> " << int(neighbor_rank)<< " "
                        //    << my_coor_.to_string() << " -> " << neighbor.to_string() << " \n";
                      }
                    edge_neighbors_.push_back(neighbor_rank);
                  }
              }
          }
      }
  }

  void
  mpi3d::init_corners()
  {
    std::vector<long> ngrid(3);
    ngrid[0] = nx;
    ngrid[1] = ny;
    ngrid[2] = nz;
    corner_nbyte_ = check_cast<int>(g * g * g * nfield * nbyte);
    corner_neighbors_.clear();
    corner_neighbors_.reserve(8);
    for (long i = -1; i <= 1; i += 2)
      {
        for (long j = -1; j <= 1; j += 2)
          {
            for (long k = -1; k <= 1; k += 2)
              {
                coor neighbor(my_coor_);
                neighbor[0] = (neighbor[0] + ngrid[0] + i) % ngrid[0];
                neighbor[1] = (neighbor[1] + ngrid[1] + j) % ngrid[1];
                neighbor[2] = (neighbor[2] + ngrid[2] + k) % ngrid[2];
                mpi_id neighbor_rank = coor_to_rank(neighbor);
                if (neighbor_rank != my_rank_)
                  {

                      {
                        //SSTMAC_DEBUG << "corner: " << int(my_rank_) << " -> " << int(neighbor_rank)<< " "
                        //    << my_coor_.to_string() << " -> " << neighbor.to_string() << " \n";
                      }
                    corner_neighbors_.push_back(neighbor_rank);
                  }
              }
          }
      }
  }

  void
  mpi3d::init()
  {
    my_rank_ = comm_->rank();
    mpi_id nproc = comm_->size();

    long nxyz = nx * ny * nz;
    if (int(nproc) < nxyz)
      {
        std::cout << "mpi3d: given " << nproc << " processes but need " << nxyz
            << std::endl;
        abort();
      }

    tag_.id_ = 0;

    if (my_rank_.id_ < nxyz)
      {
        my_coor_ = rank_to_coor(my_rank_);
        // set up info need for face transfers
        init_faces();
        // set up info needed for edge transfers
        init_edges();
        // set up info needed for edge transfers
        init_corners();
      }
  }

} //end namespace mini
