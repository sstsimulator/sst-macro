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
#include <sprockit/serializer.h>

DeclareSerializable(sstmac::sst_message);

#define sst_msg_invalid(fxn) \
    spkt_throw_printf(sprockit::illformed_error, \
        "sst_message:%s: not implemented for %s", \
        #fxn, to_string().c_str());

namespace sstmac {

ImplementEnum(sst_message::message_type_t);
ImplementEnum(sst_message::field);
RegisterEnum(sst_message::message_type_t, sst_message::SST);
RegisterEnum(sst_message::message_type_t, sst_message::NONE);

sst_message::sst_message() :
  msgtype_(NONE),
  key_(0)
{
}

sst_message*
sst_message::parent() const
{
  return const_cast<sst_message*>(this);
}

void
sst_message::serialize_order(sprockit::serializer& ser)
{
  ser & msgtype_.value;
  ser & fields_;
}

long
sst_message::byte_length() const
{
  sst_msg_invalid(byte_length);
}

uint64_t
sst_message::unique_id() const
{
  sst_msg_invalid(unique_id);
}

} // end of namespace sstmac

