
#ifndef SSTMAC_MICRO_MESSAGE_WRAPPER_H_
#define SSTMAC_MICRO_MESSAGE_WRAPPER_H_

#include <sstmac/common/sstmac_config.h>

////////////////////////////////////////////////////////////////////////////////

#include <sst/core/event.h>

#include <sstmac/common/event_handler.h>
#include <sstmac/common/messages/sst_message.h>
#include <sstmac/common/sst_event.h>

#include <sprockit/util.h>

#include <sprockit/serializer.h>

namespace sstmac {

class SSTMessageEvent : public SST::Event {

  public:
    SSTMessageEvent(const sst_message::ptr& msg) : msg_to_deliver_(msg)
    {

    }

    sst_message::ptr
    message() const {
      return msg_to_deliver_;
    }

    virtual ~SSTMessageEvent() { }

  protected:

    SSTMessageEvent() { }

  private:

    sst_message::ptr msg_to_deliver_;

};

} // end namespace sstmac

#endif /* SSTMAC_MICRO_MESSAGE_WRAPPER_H_ */

