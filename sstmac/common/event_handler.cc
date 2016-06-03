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

#include <sstmac/common/event_handler.h>
#include <limits>

namespace sstmac {

event_loc_id event_loc_id::null = event_loc_id(switch_id(std::numeric_limits<int32_t>::max()));
event_loc_id event_loc_id::uninitialized = event_loc_id(switch_id(std::numeric_limits<int32_t>::max()-1));

} // end of namespace sstmac

