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

#ifndef SSTMAC_COMMON_NODEADDRESS_H_INCLUDED
#define SSTMAC_COMMON_NODEADDRESS_H_INCLUDED

#include <cstdint>

namespace sstmac {

typedef uint32_t endpoint_id;
typedef endpoint_id node_id;

typedef int32_t topology_id;
typedef topology_id switch_id;

typedef endpoint_id netlink_id;


} // end namespace sstmac

#endif

