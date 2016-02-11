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

#ifndef SSTMAC_SOFTWARE_PROCESS_APPID_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_APPID_H_INCLUDED

#include <sprockit/opaque_typedef.h>
#include <sprockit/spkt_config.h>

namespace sstmac {
namespace sw {

typedef_opaque_int(app_id, int);
implement_opaque_int(app_id)


}
} // end of namespace sstmac

#if SPKT_HAVE_CPP11
namespace std {
template <>
struct hash<sstmac::sw::app_id>
  : public std::hash<sprockit::opaque_type<int>>
{ };
}
#else
namespace sstmac {
namespace sw {
inline std::size_t
hash_value(const app_id& aid){
  return aid.id_;
}
}
}
#endif

#endif

