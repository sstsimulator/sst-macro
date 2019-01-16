#include "patinkin.h"

namespace sstmac {
namespace tutorial {

mandy_patinkin::mandy_patinkin(SST::Params& params)
 : actor(params)
{
  sword_hand_ = params->get_param("sword_hand");

  if (sword_hand_ == "left") {
    sprockit::abort("I am not left handed!");
  } else if (sword_hand_ != "right") {
    spkt_throw_printf(sprockit::value_error,
                     "Invalid hand specified: %s",
                     sword_hand_.c_str());
  }
}

void
mandy_patinkin::act()
{
  std::cout <<
            "Hello. My name is Inigo Montoya. You killed my father. Prepare to die!"
            << std::endl;
}


}
}
