#include "actor.h"

namespace sstmac {
namespace tutorial {

class mandy_patinkin :
  public actor
{

 public:
  std::string
  to_string() const {
    return "inigo montoya";
  }

  static ptr
  construct(sprockit::sim_parameters* params) {
    return new mandy_patinkin;
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  act();

 protected:
  std::string sword_hand_;

};

}
}


