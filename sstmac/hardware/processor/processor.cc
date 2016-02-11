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

#include <sstmac/hardware/processor/processor.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/software/libraries/compute/compute_message.h>
#include <sprockit/statics.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::processor);
RegisterDebugSlot(processor);

namespace sstmac {
namespace hw {

static sprockit::need_delete_statics<processor> del_statics;

processor::processor()
{
}

processor::~processor()
{
}

void
processor::finalize_init()
{
  init_loc_id(node_->event_location());
}

void
processor::init_factory_params(sprockit::sim_parameters *params)
{
  /** sstkeyword {
        gui=2.1GHz;
        docstring=The clock cycle frequency for the node.ENDL
        For simple models, synonymous with flop frequency.;
  } */
  freq_ = params->get_freq_param("frequency");
  mem_freq_ = freq_;
}

void
processor::delete_statics()
{
}

void
processor::handle(const sst_message::ptr &msg)
{
  spkt_throw(sprockit::unimplemented_error,
    "processor::handle: should never handle anything"); 
}

void
processor::os_delayed_notify(timestamp delay, const sw::compute_message::ptr& msg)
{
  START_VALID_SCHEDULE(node_)
  node_->schedule_delay(delay, node_, msg);
  STOP_VALID_SCHEDULE(node_)
}

void
processor::os_notify_now(const sw::compute_message::ptr& msg)
{
  START_VALID_SCHEDULE(node_)
  node_->schedule_now(node_, msg);
  STOP_VALID_SCHEDULE(node_)
}

}
} //end of namespace sstmacs

