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

#include "osulat.h"
#include <stdio.h>
#include <errno.h>
using namespace sstmac;
using namespace sstmac::sw;

namespace osu {

    SpktRegisterApp("OSU_latency", osulat, "OSU latency test for measuring inter/intra node latency");

  //
  // Hi.
  //
  osulat::osulat() :
    max_msg_size(1<<22), loop(100),  skip(10), loop_large(20),
    skip_large(2), large_message_size(8192)
  { }

  //
  // Goodbye.
  //
  osulat::~osulat() throw() {
  }

  //
  // Go.
  //
  void osulat::skeleton_main() {
    int i, size;
    mpi_id myid, numprocs;
    timestamp t_start, t_end, t;
    static const mpi_id zeroid(0), oneid(1);
    int currloop = loop;
    FILE *fout = stderr;
    
    mpi()->init();
    mpi_comm* world = mpi()->comm_world();
    myid = world->rank();
    numprocs = world->size();

    if(numprocs != 2) {
      if(myid == zeroid) {
        fprintf(stderr, "osulat: requires exactly two processes\n");
      }
      mpi()->finalize();
      return;
    }

    if(myid == zeroid) {
      fprintf(fout, "# %s %s\n", "SST/macro OSU latency", "v3.1.1");
      fprintf(fout, "%-9s %20s\n", "# Size", "Latency (us)");
      fflush(fout);
    }

    /* Latency test */
    for(size = 0; size <= max_msg_size; size = (size ? size *= 2 : size + 1)) {
      if(size > large_message_size) {
        currloop = loop_large;
        skip = skip_large;
      }
      mpi()->barrier(world);
      if(myid == zeroid) {
        for(i = 0; i < currloop + skip; i++) {
          if(i == skip) {
            t_start = mpi()->wtime();
          }
          std::vector<mpi_status*> reqstat(1);
          mpi()->send(size,mpi_type::mpi_char->id,oneid,
                      mpi_tag(1),world);
          mpi()->recv(size,mpi_type::mpi_char->id,oneid,
                      mpi_tag(1),world,reqstat.at(0));
        }
        t_end = mpi()->wtime();
      } else if(myid == oneid) {
        for(i = 0; i < currloop + skip; i++) {
          std::vector<mpi_status*> reqstat(1);
          mpi()->recv(size,mpi_type::mpi_char->id,zeroid,
                      mpi_tag(1),world,reqstat.at(0));
          mpi()->send(size,mpi_type::mpi_char->id,zeroid,
                      mpi_tag(1),world);
        }
      }

      if(myid == zeroid) {
        t = t_end - t_start;
        double latency = t.usec() / (2.0 * currloop);

        fprintf(fout, "%-9d %20.2f\n", size, latency);
        fflush(fout);
      }
    }
    
    mpi()->finalize();
  }

  //
  // Set up OSU parameters. Default values are as set by main constructor
  //
  void
  osulat::consume_params(sprockit::sim_parameters* params)
  { 
    max_msg_size = params->get_optional_int_param("osu_max_msg_size",
						  max_msg_size);
    loop = params->get_optional_int_param("osu_loop", loop);
    skip = params->get_optional_int_param("osu_skip", skip);
    loop_large = params->get_optional_int_param("osu_loop_large", loop_large);
    skip_large = params->get_optional_int_param("osu_skip_large", skip_large);
    large_message_size = params->get_optional_int_param("osu_large_message_size", 
							large_message_size);
  }

} //end namespace
