#include "actor.h"

namespace sstmac {
namespace tutorial {

class mandy_patinkin :
  public actor
{

 public:
  mandy_patinkin(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "inigo montoya";
  }

  virtual void
  act() override;

 protected:
  std::string sword_hand_;

};

}
}


