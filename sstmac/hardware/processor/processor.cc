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

#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/hardware/processor/processor.h>
#include <sstmac/hardware/memory/memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/statics.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::processor);
RegisterDebugSlot(processor);

namespace sstmac {
namespace hw {

static sprockit::need_delete_statics<processor> del_statics;


processor::~processor()
{
}

void
processor::finalize_init()
{
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

}
} //end of namespace sstmacs

