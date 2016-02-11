#include "patinkin.h"

namespace sstmac {
namespace tutorial {

SpktRegister("patinkin", actor, mandy_patinkin,
            "He's on one of those shows now... NCIS? CSI?");

void
mandy_patinkin::init_factory_params(sprockit::sim_parameters* params)
{
  sword_hand_ = params->get_param("sword_hand");

  if (sword_hand_ == "left") {
    spkt_throw(sprockit::value_error,
              "I am not left handed!");
  }
  else if (sword_hand_ != "right") {
    spkt_throw_printf(sprockit::value_error,
                     "Invalid hand specified: %s",
                     sword_hand_.c_str());
  }

  actor::init_factory_params(params);
}

actor*
mandy_patinkin::clone() const
{
  mandy_patinkin* cln = new mandy_patinkin;
  clone_into(cln);
  return cln;
}

void
mandy_patinkin::clone_into(mandy_patinkin* cln) const
{
  cln->sword_hand_ = sword_hand_;
  actor::clone_into(cln);
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


