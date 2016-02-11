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

#include <sstmac/software/launch/launch_info.h>
#include <sstmac/software/process/app.h>

namespace sstmac {
namespace sw {

launch_info::launch_info(app* app,
                         app_id a,
                         long num_tasks,
                         const std::vector<int>& core_affinities) :
  apptype_(app), aid_(a),
  num_tasks_(num_tasks),
  core_affinities_(core_affinities)
{
}

int
launch_info::core_affinity(int intranode_rank) const
{
  if (core_affinities_.size() > 0) { //we are assigned
    return core_affinities_[intranode_rank];
  }
  else {
    return thread::no_core_affinity;
  }
}

launch_info::~launch_info()
{
}

}
}

