#ifndef NULLNIC_H
#define NULLNIC_H

#include <sstmac/hardware/analytic/simple/simple_nic.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>

namespace sstmac {
namespace hw {

class null_nic :
  public simple_nic
{

 public:
  virtual ~null_nic() throw () {}

  std::string
  to_string() const {
    return sprockit::printf("Null NIC(%d)", int(my_addr_));
  }

  void
  init_factory_params(sprockit::sim_parameters* params);

};

}
}


#endif // NULLNIC_H

