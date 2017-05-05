#include <sstmac/hardware/router/multipath_routing.h>
#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/router/ugal_routing.h>

namespace sstmac {
namespace hw {

class multipath_minimal : public multipath_router<minimal_router> {
  FactoryRegister("minimal_multipath", router, multipath_minimal)
 public:
  multipath_minimal(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};

class multipath_valiant : public multipath_router<valiant_router> {
  FactoryRegister("valiant_multipath", router, multipath_valiant)
 public:
  multipath_valiant(sprockit::sim_parameters* params, topology* top, network_switch* netsw) :
   multipath_router(params,top,netsw){}
};

}
}
