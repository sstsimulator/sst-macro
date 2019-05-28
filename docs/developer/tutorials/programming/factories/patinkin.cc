#include "patinkin.h"
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace tutorial {

MandyPatinkin::MandyPatinkin(SST::Params& params)
 : Actor(params)
{
  sword_hand_ = params.find<std::string>("sword_hand");

  if (sword_hand_ == "left") {
    sprockit::abort("I am not left handed!");
  } else if (sword_hand_ != "right") {
    spkt_throw_printf(sprockit::ValueError,
                     "Invalid hand specified: %s",
                     sword_hand_.c_str());
  }
}

void
MandyPatinkin::act()
{
  std::cout <<
            "Hello. My name is Inigo Montoya. You killed my father. Prepare to die!"
            << std::endl;
}


}
}
