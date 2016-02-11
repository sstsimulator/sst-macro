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

#include <sprockit/opaque_typedef.h>

namespace sstmac {

typedef_opaque_int(endpoint_id, int);
typedef endpoint_id node_id;

typedef_opaque_int(topology_id, int);
typedef topology_id switch_id;


typedef endpoint_id netlink_id;
//typedef_opaque_int(netlink_id, int);

implement_opaque_int(endpoint_id)
implement_opaque_int(topology_id)

} // end namespace sstmac

#if SPKT_HAVE_CPP11
namespace std {
template <>
struct hash<sstmac::endpoint_id>
  : public std::hash<sprockit::opaque_type<int>>
{ };
template <>
struct hash<sstmac::topology_id>
  : public std::hash<sprockit::opaque_type<int>>
{ };
}
#endif

namespace sstmac {

inline std::size_t
hash_value(const endpoint_id& id){
  return id.id_;
}

inline std::size_t
hash_value(const topology_id& id){
  return id.id_;
}

}

#endif

