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
    connectable* mod) {
    connect_weighted(src_outport, dst_inport, ty, mod, 1.0, 1);
  }

  virtual void
  connect_weighted(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    double weight, int red) {
    spkt_throw_printf(sprockit::unimplemented_error,
                     "connectable %s does not implement connect for %s",
                     to_string().c_str(), str(ty));
  }

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
};

}

}

#endif // CONNECTION_H

