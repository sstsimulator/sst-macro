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

#include <sstmac/skeletons/test/global_test.h>

namespace sstmac {
namespace sw {
SpktRegister("global_test | test_global", app, global_test);


//either initialization mechanism works, apparently.
global_int global_test::class_global_(9);
global_double global(10);

void
global_test::consume_params(sprockit::sim_parameters* params)
{

}

void
global_test::skeleton_main()
{
  std::cout << "Hello World! " << appnum() << " "  << tasknum() << "\n"
               " I'm located at "
               << physical_address()
               << ", and I have initial value of " << class_global_
               << " for my class global, and an initial value of " << global
               << " for the file global.\n";

  class_global_ = global = tasknum();

  std::cout << "class global after assignment: " << class_global_
               << "\n";
  std::cout << "file global after assignment: " << global << "\n";

  timestamp t(rand() % 20);
  sleep(t);

  class_global_ = class_global_ + 1;
  global = global + 1;

  std::cout << "test(" << tasknum() << "): added one to class: " <<
               class_global_ << "\n";
  std::cout << "test(" << tasknum() << "): added one to file: " << global
               << "\n";


}

}
} //end of namespace sstmac

