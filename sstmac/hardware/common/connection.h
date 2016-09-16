#ifndef CONNECTION_H
#define CONNECTION_H

#include <sstmac/common/event_scheduler.h>
#include <sprockit/sim_parameters_fwd.h>

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

  virtual void
  connect_output(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) = 0;

  virtual void
  connect_input(
    sprockit::sim_parameters* params,
    int src_outport,
    int dst_inport,
    connectable* mod) = 0;

};

class connectable_component :
  public event_scheduler,
  public connectable
{
 protected:
  connectable_component(sprockit::sim_parameters* params, uint64_t id, event_manager* mgr)
    : event_scheduler(params, id, mgr)
  {
  }
};

class connectable_subcomponent :
  public event_subscheduler,
  public connectable
{
protected:
 connectable_subcomponent(event_scheduler* parent)
   : event_subscheduler(parent)
 {
 }
};

}

}

#endif // CONNECTION_H

