#include <sstmac/hardware/router/multipath_routing.h>
#include <sstmac/hardware/router/minimal_routing.h>
#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/router/ugal_routing.h>

namespace sstmac {
namespace hw {

SpktTemplateRegister("minimal_multipath", router, multipath_router<minimal_router>, minmp);

SpktTemplateRegister("valiant_multipath", router, multipath_router<valiant_router>, valmp);

//SpktTemplateRegister("ugal_multipath", router, multipath_router<ugal_router>, ugalmp);

}
}
