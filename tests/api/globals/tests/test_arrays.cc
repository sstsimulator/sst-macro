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

namespace test_arrays
{
  using namespace sstmac::sw;

#define NUM 10

  global_arr<int, NUM> arr;

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

  void
  fill_arr(int* x, int n)
  {
    for (int i = 0; i < n; i++)
    {
      x[i] = 10 + i;

    }
    return;
  }

  int
  test_arrays(int argc, char *argv[])
  {

    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    for (int i = 0; i < NUM; i++)
    {
      arr[i] = i;

    }

    memset(arr, 0, NUM * sizeof(int));

    for (int i = 0; i < NUM; i++)
    {
      report(arr[i] == 0, "arr[i]: setting to zero");
    }

    fill_arr(arr, NUM);

    for (int i = 0; i < NUM; i++)
    {
      report(arr[i] == 10 + i, "arr[i]");
    }

    MPI_Finalize();
    return 0;
  }

}
