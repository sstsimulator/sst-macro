#include <sstmac/hardware/nic/netlink.h>
#include <sstmac/hardware/node/node.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::hw::netlink);

RegisterNamespaces("netlink", "injection", "ejection");

namespace sstmac {
namespace hw {

netlink::netlink(sprockit::sim_parameters* params, node *parent) :
  event_subcomponent(parent) //no self events
{
  id_ = netlink_id(params->get_int_param("id"));
  conc_ = params->get_int_param("concentration");
  num_tiles_ = params->get_optional_int_param("num_tiles", 1);
}

}
}
