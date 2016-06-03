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

#include <sstmac/hardware/router/routing_info.h>
#include <sstmac/hardware/router/router.h>

namespace sstmac {
namespace hw {

#define enumcase(x) case x: return #x

namespace routing {

const char*
tostr(routing::algorithm_t algo)
{
  switch(algo) {
      enumcase(minimal);
      enumcase(valiant);
      enumcase(deflt);
      enumcase(ugal);
    default:
      spkt_throw_printf(sprockit::value_error,
                       "invalud routing algorithm enum %d",
                       algo);
  }
  return 0;
}

}
}
}

