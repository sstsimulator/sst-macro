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

#ifndef SSTMAC_BACKENDS_NATIVE_LAUNCH_LAUNCHER_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_LAUNCH_LAUNCHER_H_INCLUDED

#include <sstmac/software/libraries/service.h>
#include <sstmac/common/vis/vis.h>
#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {

/**
 * A launcher that can be cooperatively scheduled by a very naive scheduler.
 */
class launcher :
  public service,
  public vis::vis_comp
{

 public:
  launcher();

  virtual std::string
  to_string() const {
    return "launcher";
  }

  /// Hasta la vista.
  virtual ~launcher() throw ();

  virtual void
  incoming_message(sst_message* msg);

  virtual void
  start();

 protected:
  bool is_completed_;

  spkt_unordered_map<app_id, int> num_apps_launched_;
};

}
} // end of namespace sstmac

#endif

