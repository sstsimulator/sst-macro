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

#include <sstmac/libraries/pthread/sstmac_pthread_runner.h>
#include <sstmac/software/process/app.h>

namespace sstmac {
namespace sw {

pthread_runner::pthread_runner(software_id id, app* parent,
                               start_fxn start_routine, void* arg,
                               operating_system* os)
  : thread(parent->params(), id, os),
    start_routine_(start_routine),
    arg_(arg)
{
  parent_app_ = parent;
  parent_app_->add_subthread(this);
}

void
pthread_runner::clear_subthread_from_parent_app()
{
  parent_app_->set_subthread_done(this);
}

void
pthread_runner::run()
{
  p_txt_ = parent_app_->get_process_context();
  start_routine_(arg_);
}


}
}

