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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MPI3D_MPI3D_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MPI3D_MPI3D_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>
#include <sstmac/libraries/mpi/mpi_api.h>

#include <math.h>
#include <stdlib.h>

#include <sstmac/software/libraries/compute/lib_compute_time.h>
#include <sstmac/software/libraries/util/lib_display.h>

#include <sstmac/sstmacro.h>

namespace mini
{

    /**
     * Basic MPI ping-pong.
     */
    class mpi3d : public mpi_app
    {
    public:

      // User adjustable fields.
      bool verbose;
      long l;
      long g;
      long nfield;
      long nbyte;
      double gamma;
      long nx;
      long ny;
      long nz;
      long nstep;
      bool waitall;

      // Helgi hacks.
      timestamp compute_time;
      double noiselevel;
      unsigned int rngseed;

    private:
      class gaussnoise
      {
        int valid;
        double val[2];
        unsigned int seed_;
        // Can you believe that boost::mt19937 is 5011 bytes in size!
      public:
        gaussnoise() :
            valid(0)
        {
        }
        void
        seed(unsigned int s)
        {
          seed_ = s;
        }
        double
        operator()()
        {
          if (!valid)
            {
              get_values();
            }
          return val[--valid];
        }
      private:
        void
        get_values()
        {
          // from http://www.taygeta.com/random/gaussian.html
          // Algorithm by Dr. Everett (Skip) Carter, Jr.
          double x1, x2, w;
          double max = RAND_MAX;
          do
            {
              x1 = 2.0 * (rand_r(&seed_) / max) - 1.0;
              x2 = 2.0 * (rand_r(&seed_) / max) - 1.0;
              w = x1 * x1 + x2 * x2;
            }
          while (w >= 1.0);
          w = sqrt((-2.0 * log(w)) / w);
          val[0] = x1 * w;
          val[1] = x2 * w;
          valid = 2;
        }
      };
      gaussnoise rng;

    public:
      // must be public for operator <<
      /// This class represents a coordinate in the 3D grid.
      class coor : public std::vector<long>
      {
      public:
        coor() :
            std::vector<long>(3)
        {
        }
        coor(long x, long y, long z) :
            std::vector<long>(3)
        {
          operator[](0) = x;
          operator[](1) = y;
          operator[](2) = z;
        }

        std::string
        to_string()
        {
          std::stringstream ss;
          ss << "(" << at(0) << ", " << at(1) << ", " << at(2) << ")";
          return ss.str();
        }
      };

    private:
      // These fields keep track of fixed information about the run.
      std::map<coor, long> coor_to_rank_;
      mpi_id my_rank_;
      coor my_coor_;
      std::vector<mpi_id> face_neighbors_;
      std::vector<mpi_id> edge_neighbors_;
      std::vector<mpi_id> corner_neighbors_;
      int face_nbyte_;
      int edge_nbyte_;
      int corner_nbyte_;
      mpi_tag tag_;
      mpi_comm* comm_;
      double time_;

      // utility functions
      coor
      rank_to_coor(mpi_id rank);

      mpi_id
      coor_to_rank(const coor &c);

      mpi_id
      offset_to_rank(long dx, long dy, long dz);

      void
      run_irecv(int nbyte, const std::vector<mpi_id> &neighbors,
          std::vector<mpi_request*> &reqs);

      void
      run_isend(int nbyte, const std::vector<mpi_id> &neighbors,
          std::vector<mpi_request*> &reqs);

      void
      run_waitall(std::vector<mpi_request*> &reqs);

      void
      run_compute();

      void
      init_faces();

      void
      init_edges();

      void
      init_corners();

      void
      init();

    protected:
      sstmac::sw::lib_display* lib_disp_;
      sstmac::sw::lib_compute_time* compute_lib_;

      static int active_workers_;

    public:
      mpi3d();

      /// Goodbye.
      virtual
      ~mpi3d() throw ();

      /// Get a copy.
      virtual app*
      clone_type(){
        return new mpi3d;
      }

      void
      set_compute(const timestamp &ts, double noise, unsigned int seed =
          0xBADF00D);

      /// Go.
      virtual void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const {
        return "mpi3d";
      }
    };

    std::ostream &
    operator <<(std::ostream&o, mpi3d::coor&c);

} // end of namespace mini

#endif
