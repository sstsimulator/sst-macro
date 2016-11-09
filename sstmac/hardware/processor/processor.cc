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
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::hw::processor);
RegisterDebugSlot(processor);

RegisterKeywords(
"frequency",
"ncores",
);

namespace sstmac {
namespace hw {

static sprockit::need_delete_statics<processor> del_statics;


processor::~processor()
{
}

processor::processor(sprockit::sim_parameters* params, memory_model* mem, node* nd) :
  mem_(mem), node_(nd)
{
  freq_ = params->get_freq_param("frequency");
  mem_freq_ = freq_;
  ncores_ = params->get_int_param("ncores");
}

void
processor::delete_statics()
{
}

}
} //end of namespace sstmacs

