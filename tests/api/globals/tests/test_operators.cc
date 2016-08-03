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
#include <sstmac/compute.h>

namespace test_operators
{
  using namespace sstmac::sw;

  global_int x;

  void
  report(bool pass, int rank, std::string name)
  {
    if (rank == 3)
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
  }

  int
  test_operators(int argc, char *argv[])
  {

    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    // ------------------- primitive type operations -----------------//

    if (rank != 0) //rank 0 doesn't participate because of /
    {
      x = 0;

      x += rank;
      report(x == rank, rank, "+=");

      x *= rank;
      report(x == (rank * rank), rank, "*=");

      x /= rank;
      report(x == rank, rank, "/=");

      x -= rank;
      report(x == 0, rank, "-=");

      x++;
      report(x == 1, rank, "++");

      x--;
      report(x == 0, rank, "--");

    }
    int temp;

    x = 2;

    temp = x + 4;
    report(temp == 6, rank, "+");

    temp = x - 2;
    report(temp == 0, rank, "-");

    temp = 4 * x;
    report(temp == 8, rank, "*");

    temp = x / 2;
    report(temp == 1, rank, "/");

    temp = 8 / x;
    report(temp == 4, rank, "/ other");

    temp = x << 2;
    report(temp == 8, rank, "<<");

    temp = 32 >> x;
    report(temp == 8, rank, ">>");

    x = 4;
    temp = 15 % x;
    report(temp == 3, rank, "%");

    temp = x % 3;
    report(temp == 1, rank, "% other");

    int* temptr = &x;
    report(*temptr == 4, rank, "&, *");

    // ------------------- pointer type operations --------------------//

    MPI_Finalize();
    return 0;
  }

}
