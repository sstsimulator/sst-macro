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

#ifndef SSTMAC_COMMON_MESSAGES_SST_MESSAGE_H_INCLUDED
#define SSTMAC_COMMON_MESSAGES_SST_MESSAGE_H_INCLUDED


#include <sstmac/common/serializable.h>
#include <sstmac/common/node_address.h>
#include <sprockit/metadata_bits.h>
#include <sstmac/common/sst_event.h>


namespace sstmac {

class flow : public event
{
 public:
  virtual uint64_t
  unique_id() const = 0;

  /**
   * Virtual function to return size. Child classes should impement this
   * if they want any size tracked / modeled.
   * @return Zero size, meant to be implemented by children.
   */
  virtual long
  byte_length() const = 0;
};

/**
 * A class describing an event.
 */
class message :
  public flow
{

 public:
  virtual ~message() {}

  virtual node_id
  toaddr() const = 0;

  virtual node_id
  fromaddr() const = 0;

};





} // end of namespace sstmac
#endif

