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

#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/event_callback.h>

#define sst_msg_invalid(fxn) \
    spkt_throw_printf(sprockit::illformed_error, \
        "sst_message:%s: not implemented for %s", \
        #fxn, to_string().c_str());

namespace sstmac {


} // end of namespace sstmac

