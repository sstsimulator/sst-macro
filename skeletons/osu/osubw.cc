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

#include "osubw.h"
#include <stdio.h>
#include <errno.h>

using namespace sstmac;
using namespace sstmac::sw;

namespace osu {
    SpktRegisterApp("OSU_bandwidth", osubw, "OSU bandwidth test for measuring inter/intra node bandwidth");

  //
  // Hi.
  //
  osubw::osubw() :
    max_msg_size(1<<22),
    loop(100), window_size(64), skip(10), loop_large(20), 
    skip_large(2), large_message_size(8192)
  {}

  //
  // Goodbye.
  //
  osubw::~osubw() throw() {
  }

  //
  // Go.
  //
  void osubw::skeleton_main() {
    int i, j, size;
    mpi_id myid, numprocs;
    timestamp t_start, t_end, t;
    static const mpi_id zeroid(0), oneid(1);
    int winsize = window_size;
    int currloop = loop;

    FILE *fout = stdout;
    
    mpi()->init();
    mpi_comm* world = mpi()->comm_world();

    myid = world->rank();
    numprocs = world->size();

    //align_size = getpagesize();
    //assert(align_size <= MAX_ALIGNMENT);

    if(numprocs.id_ != 2) {
      if(myid == zeroid) {
        fprintf(stderr, "osubw: requires exactly two processes\n");
      }

      mpi()->finalize();
      return;
    }

    if(myid == zeroid) {
      fprintf(fout, "# %s %s\n", "SST/macro OSU bandwidth", "v3.1.1");
      fprintf(fout, "%-9s %20s\n", "# Size", "Bandwidth (MB/s)");
      fflush(fout);
    }

    /* Bandwidth test */
    for(size = 1; size <= max_msg_size; size *= 2) {
      if(size > large_message_size) {
        currloop = loop_large;
        skip = skip_large;
        winsize = window_size_large;
      }
      if(myid == zeroid) {
        for(i = 0; i < currloop + skip; i++) {
          std::vector<mpi_request*> request;
          if(i == skip) {
            t_start = mpi()->wtime();
          }
          for(j = 0; j < winsize; j++) {
            mpi_request* req;
            mpi()->isend(size, mpi_type::mpi_char->id, oneid, mpi_tag(100), world,
                         req);
            request.push_back(req);
          }
          std::vector<mpi_status> reqstat(1);
          mpi()->waitall(request, reqstat);
          mpi()->recv(4, mpi_type::mpi_char->id, oneid, mpi_tag(101), world,
                      &reqstat.at(0));
        }

        t_end = mpi()->wtime();
        t = t_end - t_start;
      }

      else if(myid == oneid) {
        std::vector<mpi_request*> request;
        for(i = 0; i < currloop + skip; i++) {
          for(j = 0; j < winsize; j++) {
            mpi_request* req;
            mpi()->irecv(size, mpi_type::mpi_char->id, zeroid, mpi_tag(100),
                         world, req);
            request.push_back(req);
          }
          std::vector<mpi_status> reqstat(1);
          mpi()->waitall(request, reqstat);
          mpi()->send(4, mpi_type::mpi_char->id, zeroid, mpi_tag(101), world);
        }
      }

      if(myid == zeroid) {
        double tmp = size / 1e6 * currloop * winsize;

        fprintf(fout, "%-9d %20.2f\n", size, tmp / t.sec());
        fflush(fout);
      }
    }
 
    mpi()->finalize();
  }

  //
  // Set up OSU parameters. Default values are as set by main constructor
  //
  void
  osubw::consume_params(sprockit::sim_parameters* params)
  { max_msg_size = params->get_optional_int_param("osu_max_msg_size",
						      max_msg_size);
    loop = params->get_optional_int_param("osu_loop", loop);
    skip = params->get_optional_int_param("osu_skip", skip);
    loop_large = params->get_optional_int_param("osu_loop_large", loop_large);
    skip_large = params->get_optional_int_param("osu_skip_large", skip_large);
    large_message_size = params->get_optional_int_param("osu_large_message_size", 
							large_message_size);

    window_size = params->get_optional_int_param("osu_window_size", window_size);
    window_size_large = params->get_optional_int_param("osu_window_size_large",
						       window_size_large);
  }

}

