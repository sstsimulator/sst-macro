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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_launchinfo_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_launchinfo_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/software/process/app_id.h>
#include <sstmac/software/process/app.h>

namespace sstmac {
namespace sw {

class launch_info  {

 public:
  launch_info(app* app,
              app_id aid,
              long num_tasks,
              const std::vector<int>& core_affinities);

  virtual std::string
  to_string() const {
    return "launchinfo";
  }

  int
  core_affinity(int intranode_rank) const;

  app_id
  aid() const {
    return aid_;
  }

  app*
  app_template() const {
    return apptype_;
  }

  virtual ~launch_info();

 protected:
  app* apptype_;

  app_id aid_;

  long num_tasks_;

  std::vector<int> core_affinities_;

};

}
}
#endif

