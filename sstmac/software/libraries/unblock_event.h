#ifndef UNBLOCK_EVENT_H
#define UNBLOCK_EVENT_H

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

#include <sstmac/common/sst_event.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/process/key.h>


namespace sstmac {
namespace sw {

class unblock_event : public event_queue_entry
{

 public:
  unblock_event(operating_system* os, key* k);

  virtual void execute();

 protected:
  operating_system* os_;
  key* key_;

};

class timeout_event : public event_queue_entry
{

 public:
  timeout_event(operating_system* os, key* k);

  virtual void execute();

 protected:
  operating_system* os_;
  key* key_;

};

}
} //end of namespace sstmac


#endif // UNBLOCK_EVENT_H
