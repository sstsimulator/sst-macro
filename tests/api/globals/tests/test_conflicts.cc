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
#include <sstmac/replacements/mpi.h>
#include <sstmac/software/process/global.h>
#include <sstmac/compute.h>

namespace test_conflicts
{
  using namespace sstmac::sw;

  global_int global;
  static global_int st;
  global_int_ptr gptr;

  global_int arr[3];
  global_arr<int, 3> arr_type; // this is the correct way to do an array
  global_ptr_arr<int, 3> ptr_arr;

  struct str
  {
    int x;
    int y;
  };

  sstmac_global_struct<str*> global_struct;

  //this is a real global
  bool init = false;

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
  test_conflicts(int argc, char *argv[])
  {

    if (!init)
    {
      srand(time(NULL));
      init = true;
    }

    MPI_Init(NULL, NULL);
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    //sleep randomly
    SSTMAC_compute((double) rand() / (double) RAND_MAX);

    global = rank;
    st = rank;

    gptr = (int*) malloc(sizeof(int));
    *gptr = rank;

    arr[1] = rank;
    arr_type[1] = rank;
    ptr_arr[1] = (int*) malloc(sizeof(int));
    *ptr_arr[1] = rank;

    global_struct = new str();
    global_struct->x = rank;
    global_struct->y = 100 + rank;

    MPI_Barrier( MPI_COMM_WORLD);

    //sleep randomly
    SSTMAC_compute((double) rand() / (double) RAND_MAX);

    report(global == rank, "global");

    report(st == rank, "static");

    report(*gptr == rank, "ptr");

    delete_global(gptr); //sucks that we have to do this

    std::cout << "ptr deletion: Passed\n";

    report(arr[1] == rank, "array");

    report(arr_type[1] == rank, "array_type");

    report(*ptr_arr[1] == rank, "ptr array");

    delete ptr_arr[1];

    std::cout << "ptr array deletion: Passed\n";

    report(global_struct->x == rank, "global struct.x");
    report(global_struct->y == (100 + rank), "global struct.y");

    delete_global(global_struct);

    std::cout << "struct deletion: Passed\n";

    MPI_Finalize();
    return 0;
  }

}
