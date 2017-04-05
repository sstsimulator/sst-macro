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
#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {

/**
 * A launcher that can be cooperatively scheduled by a very naive scheduler.
 */
class app_launcher :
  public service
{

 public:
  app_launcher(operating_system* os);

  /// Hasta la vista.
  virtual ~app_launcher() throw ();

  virtual void
  incoming_event(event* ev) override;

  virtual void
  start() override;

 protected:
  bool is_completed_;

  spkt_unordered_map<app_id, int> num_apps_launched_;
};

}
} // end of namespace sstmac

#endif

