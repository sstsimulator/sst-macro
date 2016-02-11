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

#include <sstmac/skeletons/test/test_compute_f.h>

extern "C" {
  void test_compute_f90_();
}

namespace sstmac {
namespace sw {

SpktRegister("test_compute_f", app, test_compute_f);

void
test_compute_f::consume_params(sprockit::sim_parameters* params)
{
}

test_compute_f::~test_compute_f()
{
}

void
test_compute_f::skeleton_main()
{
  std::cout << "t=" << now() << ": Hello World, i'm computer number "
               << tasknum() << ", about to begin computing.\n";

  // test the F90 compute interface
  // it takes some finesse for C++ to call into Fortran, hardwire for now
  timestamp tstart( now() );
  test_compute_f90_();
  timestamp tstop( now() );
  std::cout << "F90 compute test consumed " << tstop - tstart << "\n";

}

}
} //end of namespace sstmac

