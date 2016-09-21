#include "actor.h"

namespace sstmac {
namespace tutorial {

class christopher_guest :
  public actor
{

 public:
  christopher_guest(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "count rugen";
  }

  virtual void
  act() override;

 protected:
  int num_fingers_;

};

}
}

