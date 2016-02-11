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

#include <sstmac/skeletons/test/app_hello_world.h>
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace sw {

SpktRegister("hello | hello_world", app, app_hello_world);

void
app_hello_world::consume_params(sprockit::sim_parameters* params)
{
  whatToSay = params->get_param("hello_world_what");
}

app_hello_world::~app_hello_world()
{

}

void
app_hello_world::skeleton_main()
{
  int my_app_id = sid().app_;
  int my_task_id = sid().task_;
  std::cout << "t=" << now() << ": Hello World! AppID = " << my_app_id
               << ", my id = " << my_task_id << ", I'm located at "
               << physical_address()
               << ", and I'm supposed to say: " << whatToSay <<
               ".  I'll compute for a little!\n";

  timestamp t(1e-6);
  compute(t);

  std::cout <<"t=" << now() << "It's me again! (" << my_task_id << ") - I'm all done! \n";

}

}
} //end of namespace sstmac

