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

#define sstmac_app_name hello_world

#include <sstmac/skeleton.h>
#include <sstmac/compute.h>
#include <sstmac/util.h>

int USER_MAIN(int argc, char** argv)
{
  sprockit::sim_parameters* params = get_params();
  std::string message = params->get_param("message");
  sstmac_compute(1e-6);
  std::cout <<"t=" << sstmac_now() << " " << message << std::endl;
  return 0;
}

