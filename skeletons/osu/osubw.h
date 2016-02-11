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

#ifndef SSTMAC_SOFTWARE_SKELETONS_OSU_OSUBW_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_OSU_OSUBW_H_INCLUDED

#include <sstmac/sstmacro.h>

namespace osu
{
    /**
     * Reimplementation of the OSU MPI Bandwidth Test v.3.1.1 for SST/macro.
     *
     * The original OSU MPI Bandwidth Test is
     * Copyright (C) 2002-2008 the Network-Based Computing Laboratory
     * (NBCL), The Ohio State University.
     * Contact: Dr. D. K. Panda (panda@cse.ohio-state.edu)
     * Distributed under the BSD license.
     */
    class osubw : public sstmac::sw::mpi_app
    {

    public:
      /// Max message size.  Defaults to 1<<22.
      int max_msg_size;
      /// Number of times through the loop.  Defaults to 100.
      int loop;
      /// Window size (pending immediate-mode requests) for transmissions.
      /// Defaults to 64.
      int window_size;
      /// Number of iterations skipped before timing starts.  Defaults to 10.
      int skip;
      /// Number of times through the loop for large messages.  Defaults to 20.
      int loop_large;
      /// Window size for large transmissions.  Defaults to 64.
      int window_size_large;
      /// Number of skipped iterations for large transmissions.  Defaults to 2.
      int skip_large;
      /// What is considered a large message? Defaults to 8192.
      int large_message_size;

    public:
      osubw();

      /// Goodbye.
      virtual
      ~osubw() throw ();

      /// Get a copy.
      app*
      clone_type(){
        return new osubw;
      }

      /// Go.
      void
      skeleton_main();

      virtual void
      consume_params(sprockit::sim_parameters* params);

      virtual std::string
      to_string() const
      {
        return "osubw";
      }
    };

} // end of namespace osu

#endif
