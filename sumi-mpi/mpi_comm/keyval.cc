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

#include <sumi-mpi/mpi_comm/keyval.h>
#include <sprockit/errors.h>

namespace sumi {

keyval*
keyval::clone(int k) const {
    keyval* ret = new keyval(k, copy_, del_, extra_);
    int flag = 0;
    (copy_)(-1, key_, extra_, val_, ret->val_, &flag);
    if(flag != 0) {
      spkt_throw_printf(sprockit::spkt_error,
                       "mpi_comm::keyval - copy function returned with flag %d", flag);
    }
    return ret;
}

}

