/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef PACKET_ALLOCATOR_H
#define PACKET_ALLOCATOR_H

#include <sprockit/factories/factory.h>
#include <sstmac/hardware/pisces/pisces_fwd.h>
#include <sstmac/common/messages/sst_message_fwd.h>
#include <sstmac/common/serializable.h>
#include <sstmac/common/node_address.h>

namespace sstmac {
namespace hw {

/**
 * @brief The packet_allocator class
 * Factory for creating packets.
 * Default packet allocator adds the bare minimum needed for congestion modeling.
 * Non-default packet allocators can add extra fields to the packets to track more statistics.
 */
class packet_allocator
{
  DeclareFactory(packet_allocator)
 public:
  packet_allocator(sprockit::sim_parameters* params){}

  /**
   * @brief new_packet Allocates a new packet corresponding to a subset
   *  of a flow (message)
   * @param bytes       Number of bytes in the packet
   * @param byte_offset The offset within the message (flow) the packet begins at
   * @param msg         The message being packetized
   * @return  A packet compatible with pisces model
   */
  virtual pisces_payload*
  new_packet(int bytes, uint64_t flow_id, bool is_tail,
             node_id toaddr, node_id fromaddr,
             serializable* msg) = 0;

  virtual ~packet_allocator(){}

};

}
}

#endif // PACKET_ALLOCATOR_H