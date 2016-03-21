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

#include <sstmac/skeletons/test/mpi_pairwise.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/sim_parameters.h>
#include <cstring>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <sstream>

namespace sstmac {
namespace sw {

SpktRegister("MPI_pairwise | mpi_pairwise", app, mpi_pairwise);

void
mpi_pairwise::consume_params(sprockit::sim_parameters* params)
{

  iterations_ = params->get_int_param("mpipairwise_iterations");

  count_ = params->get_int_param("mpipairwise_count");

  std::string line;
  std::string fname = params->get_param("mpipairwise_file");
  std::ifstream myfile;
  myfile.open(fname.c_str());

  if(!myfile.good()) {
    spkt_throw_printf(sprockit::io_error, "mpipairwise: could not open file %s",
                     fname.c_str());
  }

  while (!myfile.eof()) {
    std::getline(myfile, line);
    std::istringstream iss(line);
    std::string temp;
    iss >> temp;
    this->from_.push_back(mpi_id(atoi(temp.c_str())));
    iss >> temp;
    this->to_.push_back(mpi_id(atoi(temp.c_str())));
    iss >> temp;
    this->when_.push_back(timestamp(atof(temp.c_str())));
  }
  myfile.close();

}

//
// Goodbye.
//
mpi_pairwise::~mpi_pairwise() throw ()
{
}


//
// Go.
//
void
mpi_pairwise::skeleton_main()
{
  if (mpi() == 0) {
    spkt_throw(sprockit::null_error, "mpipairwise::run: mpi_api pointer is null");
  }
  timestamp start = mpi()->init();
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  mpi_tag tag(0);

  for (int j = 0; j < iterations_; j++) {

    for (int i = 0; i < from_.size(); i++) {
      compute(when_[i]);
      if (rank == from_[i]) {
        // do send
        mpi()->send(count_, mpi_type::mpi_double->id, to_[i], tag, world);
      }
      if (rank == to_[i]) {
        // do receive
        mpi_status* stat = new mpi_status;
        mpi()->recv(count_, mpi_type::mpi_double->id, from_[i], tag, world,
                    stat);
      }
    }
  }

  mpi()->finalize();
}

}
} //end of namespace sstmac

