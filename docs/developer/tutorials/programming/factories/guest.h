#include "actor.h"

namespace sstmac {
namespace tutorial {

class christopher_guest :
  public actor
{

 public:
  std::string
  to_string() const override {
    return "count rugen";
  }

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  act();

 protected:
  int num_fingers_;

};

}
}

