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

#include <iostream>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/libraries/compute/compute_event.h>

using namespace sstmac;
using namespace sstmac::sw;

app*
current_app()
{
  thread* t = operating_system::current_thread();
  app* a = t->parent_app();
  if (!a) {
    spkt_throw(sprockit::spkt_error, "sstmac_compute::current_app: no parent application");
  }
  return a;
}

extern "C" void
SSTMAC_compute(double seconds)
{
  app* app = current_app();
  app->compute(timestamp(seconds));
}

extern "C" void
SSTMAC_compute_block_read(long long bytes)
{
  app* app = current_app();
  app->compute_block_read(bytes);
}

extern "C" void
SSTMAC_compute_loop(long long from, long long to, long long numlines)
{
  app* app = current_app();
  lib_compute_loops* lib = app->compute_loops_lib();
  lib->compute_loop(from, to, numlines);
}

extern "C" void
SSTMAC_compute_loop2(long long from1, long long to1, long long from2,
                     long long to2, long long numlines)
{
  app* app = current_app();
  lib_compute_loops* lib = app->compute_loops_lib();
  lib->compute_loop2(from1, to1, from2, to2, numlines);
}

extern "C" void
SSTMAC_compute_loop3(long long from1, long long to1, long long from2, int to2,
                     long long from3,
                     long long to3, long long numlines)
{
  app* app = current_app();
  lib_compute_loops* lib = app->compute_loops_lib();
  lib->compute_loop3(from1, to1, from2, to2, from3, to3, numlines);
}

extern "C" void
SSTMAC_compute_loop4(long long from1, long long to1, long long from2,
                     long long to2, long long from3,
                     long long to3, int from4, long long to4, long long numlines)
{
  app* app = current_app();
  lib_compute_loops* lib = app->compute_loops_lib();
  lib->compute_loop4(from1, to1, from2, to2, from3, to3, from4, to4, numlines);
}

