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

#ifndef SSTMAC_SOFTWARE_PROCESS_PTHREAD_RUNNER_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_PTHREAD_RUNNER_H_INCLUDED

#include <sstmac/software/process/thread.h>

namespace sstmac {
namespace sw {

class pthread_runner : public thread
{

 protected:
  typedef void* (*start_fxn)(void*);
  start_fxn start_routine_;
  void * arg_;

 public:
  virtual void
  run();

  virtual void
  clear_subthread_from_parent_app();

  pthread_runner(software_id id, app* parent,
                 start_fxn start_routine, void* arg,
                 operating_system* os);

};
}

}

#endif

