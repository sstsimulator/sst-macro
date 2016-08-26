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

#include <stdio.h>
#include <time.h>
#include <sstmac/software/process/global.h>

namespace test_structs
{
  using namespace sstmac::sw;

  struct st {
    int x;
    int y;
  };

  sstmac_global_struct<st*> g;

  void
  report(bool pass, std::string name)
  {
    if (pass)
    {
      std::cout << name << ": Passed \n";
    }
    else
    {
      std::cout << name << ": Failed \n";
    }
  }

  int
  test_structs(int argc, char *argv[])
  {

    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    g = new st();
    g->x = 4;
    g->y = 5;

    std::cout << "Passed\n";

    delete (st*) g;

    MPI_Finalize();
    return 0;
  }

}
