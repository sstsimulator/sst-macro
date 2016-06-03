
#ifndef SSTMAC_MICRO_MESSAGE_WRAPPER_H_
#define SSTMAC_MICRO_MESSAGE_WRAPPER_H_

#include <sstmac/common/sstmac_config.h>

////////////////////////////////////////////////////////////////////////////////

#include <sst/core/event.h>
#include <sstmac/common/event_handler.h>
#include <sstmac/common/sst_event.h>
#include <sprockit/util.h>

namespace sstmac {

class SSTEventWrapper : public SST::Event {

  public:
    SSTEventWrapper(event_queue_entry* ev) : ev_to_deliver_(ev)
    {

    }

    event_queue_entry*
    toDeliver() const {
      return ev_to_deliver_;
    }

    virtual ~SSTEventWrapper() { }

  protected:

    SSTEventWrapper() { }

  private:

    event_queue_entry* ev_to_deliver_;

};

} // end namespace sstmac

#endif /* SSTMAC_MICRO_MESSAGE_WRAPPER_H_ */

