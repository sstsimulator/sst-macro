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

namespace test_functions
{
  using namespace sstmac::sw;

  global_int g;

  global_int* ptrg;

  global_arr<int, 3> arrg;

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
  int_func(int x)
  {
    report(x == 1, "int_func:in");
    x = 3;
    return;
  }

  void
  int_ref_func(int &x)
  {
    x = 4;
    return;
  }

  void
  int_ptr_func(int *x)
  {
    *x = 5;
    return;
  }

  void
  void_ptr_func(void* x)
  {
    int* y = (int*) x;
    *y = 6;
    return;
  }

  void
  arr_ptr_func(int *x)
  {
    x[1] = 7;
    return;
  }

  int
  test_functions(int argc, char *argv[])
  {

    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    g = 1;
    int_func(g);
    report(g == 1, "int_func:after");

    /// Passing by reference is not supported (bummer)
    //int_ref_func(g);
    //report(g == 4, "int_ref_func");

    int_ptr_func(&g);
    report(g == 5, "int_ptr_func");

    void_ptr_func(&g);
    report(g == 6, "void_ptr_func");

    arrg[1] = 1;

    /// Passing arrays to a pointer is only supported if you use the array type correctly
    arr_ptr_func(arrg);
    report(arrg[1] == 7, "arr_ptr_func");

    MPI_Finalize();
    return 0;
  }

}
