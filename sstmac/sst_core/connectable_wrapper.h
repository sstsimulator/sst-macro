#ifndef CONNECTABLE_WRAPPER_H
#define CONNECTABLE_WRAPPER_H

#include <sstmac/hardware/common/connection.h>
#include <sprockit/unordered.h>
#include <sst/core/link.h>
#include <Python.h>

namespace sstmac {

class integrated_connectable_wrapper :
    public event_handler
{
  public:
    std::string to_string() const override { return "integrated_connectable_wrapper"; }

    integrated_connectable_wrapper(SST::Link* link) :
        event_handler(event_loc_id::null),
        link_(link)
    {
    }

    void
    handle(event* ev) override {
      spkt_throw(sprockit::unimplemented_error,
        "integrated_connectable_wrapper::handle: should never be called");
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
