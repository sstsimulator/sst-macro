#include "guest.h"

namespace sstmac {
namespace tutorial {

SpktRegister("guest", actor, christopher_guest,
            "Renowned writer, actor, improvisor of Spinal Tap fame");

void
christopher_guest::init_factory_params(sprockit::sim_parameters* params)
{
  num_fingers_ = params->get_int_param("num_fingers");
  if (num_fingers_ != 6) {
    spkt_throw_printf(sprockit::value_error,
                     "invalid number of fingers %d - must be 6");
  }
  actor::init_factory_params(params);
}

actor*
christopher_guest::clone() const
{
  christoper_guest* cln = new christopher_guest;
  clone_into(cln);
  return cln;
}

void
christopher_guest::clone_into(christopher_guest* cln) const
{
  cln->num_fingers_ = num_fingers_;
  actor::clone_into(cln);
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

