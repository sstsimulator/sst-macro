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

#ifndef SSTMAC_SOFTWARE_PROCESS_PROCESS_CONTEXT_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_PROCESS_CONTEXT_H_INCLUDED

#include <string>

namespace sstmac {
namespace sw {

struct process_context {
  long ctxt;

  process_context(long id) : ctxt(id) {}

  void operator=(long id){
    ctxt = id;
  }

  operator long() const {
    return ctxt;
  }

  bool
  operator==(long id) const {
    return ctxt == id;
  }

  static const long none = -1;
};

}
}

#endif

