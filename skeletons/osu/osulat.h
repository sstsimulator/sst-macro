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

#ifndef SSTMAC_SOFTWARE_SKELETONS_OSU_OSULAT_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_OSU_OSULAT_H_INCLUDED

#include <sstmac/sstmacro.h>

namespace osu
{

    /**
     * Reimplementation of the OSU MPI Latency Test v.3.1.1 for SST/macro.
     *
     * The original OSU MPI Latency Test is
     * Copyright (C) 2002-2008 the Network-Based Computing Laboratory
     * (NBCL), The Ohio State University.
     * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
     * Distributed under the BSD license.
     */
    class osulat : public sstmac::sw::mpi_app
    {

    public:
      /// Max message size.  Defaults to 1<<22.
      int max_msg_size;
      /// Number of times through the loop.  Defaults to 100.
      int loop;
      /// Number of iterations skipped before timing starts.  Defaults to 10.
      int skip;
      /// Number of times through the loop for large messages.  Defaults to 20.
      int loop_large;
      /// Number of skipped iterations for large transmissions.  Defaults to 2.
      int skip_large;
      /// What is considered a large message? Defaults to 8192.
      int large_message_size;

    public:
      osulat();
      
      /// Goodbye.
      virtual
      ~osulat() throw ();

      /// Get a copy. 
      app*
      clone_type(){
        return new osulat;
      }

      /// Go.
      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "osulat";
      }
    };

} // end of namespace osu

#endif
