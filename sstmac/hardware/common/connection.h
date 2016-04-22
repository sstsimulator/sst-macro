#ifndef CONNECTION_H
#define CONNECTION_H

#include <sstmac/common/event_scheduler.h>

#define connectable_type_invalid(ty) \
   spkt_throw_printf(sprockit::value_error, "invalid connectable type %s", connectable::str(ty))

#define connect_str_case(x) case x: return #x

namespace sstmac {
namespace hw {


class connectable
{
 public:
  virtual std::string
  to_string() const = 0;

  static const int any_port = -1;

  struct config {
    double src_buffer_weight;
    double dst_buffer_weight;
    double xbar_weight;
    double link_weight;
    int red;

    config() :
      src_buffer_weight(-1),
      dst_buffer_weight(-1),
      xbar_weight(-1),
      link_weight(-1),
      red(-1)
    {
    }
  };

  typedef enum {
    output,
    input
  } connection_type_t;


  static const char*
  str(connection_type_t ty) {
    switch (ty) {
      connect_str_case(output);
      connect_str_case(input);
    }
  }

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg) = 0;

};

class connectable_component :
  public event_scheduler,
  public connectable
{
 protected:
#if SSTMAC_INTEGRATED_SST_CORE
  connectable_component(SST::ComponentId_t id,
    SST::Params& params) : event_scheduler(id, params)
  {
  }
#endif
};

class connectable_subcomponent :
  public event_subscheduler,
  public connectable
{
#if SSTMAC_INTEGRATED_SST_CORE
 public:
  virtual void
  init_sst_params(SST::Params& params, SST::Component* parent){}
#endif
};

}

}

#endif // CONNECTION_H

