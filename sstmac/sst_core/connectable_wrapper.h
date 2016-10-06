#ifndef CONNECTABLE_WRAPPER_H
#define CONNECTABLE_WRAPPER_H

#include <sstmac/hardware/common/connection.h>
#include <sprockit/unordered.h>
#include <sst/core/link.h>
#include <Python.h>

namespace sstmac {

/**
 * @brief The link_wrapper class  SST/macro is built almost entirely around event handlers.
 * In the integrated core, Links serve the purpose of event handlers.
 * This wraps an SST link inside an event handler to preserve the sst/macro API.
 */
class link_wrapper :
    public event_handler
{
  public:
    std::string
    to_string() const override {
      return "link_wrapper";
    }

    link_wrapper(SST::Link* link) :
        event_handler(device_id()),
        link_(link)
    {
    }

    void
    handle(event* ev) override {
      spkt_throw(sprockit::unimplemented_error,
        "link_wrapper::handle: should never be called");
    }

    SST::Link*
    link() const override {
      return link_;
    }

  private:
    SST::Link* link_;
};

}

#endif // CONNECTABLE_WRAPPER_H
