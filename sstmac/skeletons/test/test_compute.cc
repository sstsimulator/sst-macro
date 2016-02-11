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

#include <sstmac/skeletons/test/test_compute.h>
#include <sstmac/common/sstmac_config.h>

extern "C" {
  void test_compute_c(double time, int blocks);
}

namespace sstmac {
namespace sw {

SpktRegister("test_compute", app, test_compute);


void
test_compute::consume_params(sprockit::sim_parameters* params)
{
}

test_compute::~test_compute()
{
}

void
test_compute::skeleton_main()
{
  double ctime = 0.1234;
  int cblocks = 1024;

  // test the C++ app compute mechanism
  timestamp tstart( now() );
  compute( timestamp(ctime) );
  compute_block_read(cblocks);
  timestamp tstop( now() );
  std::cout << "C++ compute test consumed " << tstop - tstart << "\n";

  // test the C compute interface
  tstart = timestamp( now() );
  test_compute_c(ctime,cblocks);
  tstop = timestamp( now() );
  std::cout << "C   compute test consumed " << tstop - tstart << "\n";

  double t_start, t_stop, t_total;
  t_start = now().msec();
  compute_detailed(1e5, 1e5, 1e5);
  t_stop = now().msec();
  t_total = t_stop - t_start;
  std::cout << sprockit::printf("Compute detailed for %12.6f ms\n", t_total);

  t_start = now().msec();
  compute_loop(1000, 10, 10, 10);
  t_stop = now().msec();
  t_total = t_stop - t_start;
  std::cout << sprockit::printf("Compute loops for %12.6f ms\n", t_total);
}

}
} //end of namespace sstmac

