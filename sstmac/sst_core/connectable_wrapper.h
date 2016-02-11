#ifndef CONNECTABLE_WRAPPER_H
#define CONNECTABLE_WRAPPER_H

#include <sstmac/hardware/common/connection.h>
#include <sst/core/link.h>
#include <Python.h>

namespace sstmac {

class connection_details
{
  public:
    typedef enum {
      node,
      sw
    } endpoint_t;

    hw::connectable::connection_type_t type;
    int src_id;
    int dst_id;
    int src_port;
    int dst_port;
    endpoint_t src_type;
    endpoint_t dst_type;
    double weight;
    int redundancy;

    void parse_type_id(const std::string& str, endpoint_t& ep, int& id);
    void parse_src(const std::string& str);
    void parse_dst(const std::string& str);

};

connection_details
parse_port_name(const std::string& port_name);

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
    handle(const sst_message::ptr &msg){
      spkt_throw(sprockit::unimplemented_error,
        "integrated_connectable_wrapper::handle: should never be called");
    }

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

class connectable_proxy_component
  : public hw::connectable
{
  public:

    static PyObject* sst;
    typedef spkt_unordered_map<switch_id, connectable_proxy_component*> switch_map;
    static switch_map registered_switches;
    typedef spkt_unordered_map<endpoint_id, connectable_proxy_component*> endpoint_map;
    static endpoint_map registered_endpoints;

    std::string to_string() const
    {
      return "connectable_proxy";
    }

    typedef enum {
      Switch,
      Endpoint,
      Other
    } component_category_t;

    event_loc_id
    event_location() const
    {
      return event_loc_id::null;
    }

    void
    connect_weighted(
      int src_outport,
      int dst_inport,
      connection_type_t ty,
      connectable* other,
      double weight,
      int red);

    ~connectable_proxy_component();

    PyObject* component_proxy;
    std::string component_name;
    std::string hop_latency;
    std::string injection_latency;
    component_category_t category;
};

}

#endif // CONNECTABLE_WRAPPER_H
