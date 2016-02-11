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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_UNBLOCKHANDLER_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_UNBLOCKHANDLER_H_INCLUDED

#include <sstmac/common/event_handler.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key.h>


namespace sstmac {
namespace sw {

class unblock_handler : public event_handler
{

 public:
  virtual std::string
  to_string() const {
    return "unblock handler";
  }

  static unblock_handler*
  construct(operating_system* os, key* k) {
    return new unblock_handler(os, k);
  }

  virtual void
  handle(const sst_message::ptr& msg);

 protected:
  unblock_handler(operating_system* os, key* k) :
    os_(os), key_(k) {
  }

 protected:
  operating_system* os_;
  key* key_;

};

}
} //end of namespace sstmac

#endif

