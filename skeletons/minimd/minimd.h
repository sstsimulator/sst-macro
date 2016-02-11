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

#ifndef SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_MINI_MINIMD_MINIMD_H_INCLUDED

#include <sstmac/libraries/mpi/mpi_app.h>
#include <lib_compute_minimd.h>
#include <sstmac/sstmacro.h>

namespace mini
{

    /**
     * Early pass at turning miniMD into a skeleton application.
     */
    class minimd : public sstmac::sw::mpi_app
    {
      /// Nested types mirroring the layout of minimd.
      class in;
      class atom;
      class force;
      class neighbor;
      class integrate;
      class thermo;
      class comm;
      class timer;

      friend class lib_compute_minimd;

      /// The input file name.
      std::string infile_;
      /// The interpolator/lookup table for our execution times.
      lib_compute_minimd* execution_;
      /// The nested private objects.
      sprockit::refcount_ptr<in> in_;
      sprockit::refcount_ptr<atom> atom_;
      sprockit::refcount_ptr<force> force_;
      sprockit::refcount_ptr<neighbor> neighbor_;
      sprockit::refcount_ptr<integrate> integrate_;
      sprockit::refcount_ptr<thermo> thermo_;
      sprockit::refcount_ptr<comm> comm_;
      sprockit::refcount_ptr<timer> timer_;

    private:
      /// Mirror static functions in mpiMD.
      void
      input();
      void
      create_box();
      void
      create_atoms();
      void
      create_velocity();
      void
      output();

    public:
      explicit
      minimd();

      /// Goodbye.
      virtual
      ~minimd() throw ();

      /// Get a copy of this object.
      app*
      clone_type(){
        return new minimd;
      }

      /// We use init_mpi to initialize a bunch of other stuff as well.
      void
      init_minimd();

      /// Go.
      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "minimd";
      }
    };

} // end of namespace mini

#endif
