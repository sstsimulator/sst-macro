#ifndef CONNECTABLE_WRAPPER_H
#define CONNECTABLE_WRAPPER_H

#include <sstmac/hardware/common/connection.h>
#include <sprockit/unordered.h>
#include <sst/core/link.h>
#include <Python.h>

namespace sstmac {

class integrated_connectable_wrapper
  : public hw::connectable,
    public event_handler
{
  public:
    std::string to_string() const { return "integrated_connectable_wrapper"; }

    integrated_connectable_wrapper(SST::Link* link) :
        event_handler(link_handler),
        link_(link)
    {
    }

    void
    handle(event* ev){
      spkt_throw(sprockit::unimplemented_error,
        "integrated_connectable_wrapper::handle: should never be called");
    }

    virtual void
    connect_output(
      sprockit::sim_parameters* params,
      int src_outport,
      int dst_inport,
      connectable* mod);

    virtual void
    connect_input(
      sprockit::sim_parameters* params,
      int src_outport,
      int dst_inport,
      connectable* mod);

    event_loc_id
    event_location() const {
      return event_loc_id::null;
    }

    SST::Link*
    link() const {
      return link_;
    }

  private:
    SST::Link* link_;
};

}

#endif // CONNECTABLE_WRAPPER_H
