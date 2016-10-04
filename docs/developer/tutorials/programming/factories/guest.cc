#include "guest.h"

namespace sstmac {
namespace tutorial {

SpktRegister("guest", actor, christopher_guest,
            "Renowned writer, actor, improvisor of Spinal Tap fame");

christopher_guest::christopher_guest(sprockit::sim_parameters* params) :
  actor(params)
{
  num_fingers_ = params->get_int_param("num_fingers");
  if (num_fingers_ != 6) {
    spkt_throw_printf(sprockit::value_error,
                     "invalid number of fingers %d - must be 6");
  }
}

void
christopher_guest::act()
{
  std::cout << "You've been chasing me your entire life only to fail now.\n"
            << "I think that's the worst thing I've ever heard. How marvelous."
            << std::endl;
}

}
}

