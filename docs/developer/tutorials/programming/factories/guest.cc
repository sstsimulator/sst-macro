#include "guest.h"
#include <sprockit/sim_parameters.h>

namespace sstmac {
namespace tutorial {

ChristopherGuest::ChristopherGuest(SST::Params& params) :
  Actor(params)
{
  num_fingers_ = params.find<int>("num_fingers");
  if (num_fingers_ != 6) {
    spkt_throw_printf(sprockit::ValueError,
                     "invalid number of fingers %d - must be 6");
  }
}

void
ChristopherGuest::act()
{
  std::cout << "You've been chasing me your entire life only to fail now.\n"
            << "I think that's the worst thing I've ever heard. How marvelous."
            << std::endl;
}

}
}
