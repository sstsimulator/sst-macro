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

#include <sstmac/common/messages/wrapper_payload.h>
#include <sstream>

namespace sstmac {

void
wrapper_payload::serialize_order(serializer& ser)
{
  spkt_throw_printf(sprockit::unimplemented_error, "wrapperpayload::serialize");
}

std::string
wrapper_payload::to_string() const
{
  std::stringstream ss;
  ss << "wrapperpayload(" << (size_t) base_ << ", " << size_ << ")";
  return ss.str();
}


}


