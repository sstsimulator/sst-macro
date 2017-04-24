#include "actor.h"

namespace sstmac {
namespace tutorial {

class mandy_patinkin :
  public actor
{
  FactoryRegister("patinkin", actor, mandy_patinkin,
              "He's on one of those shows now... NCIS? CSI?")
 public:
  mandy_patinkin(sprockit::sim_parameters* params);

  std::string to_string() const override {
    return "inigo montoya";
  }

  virtual void act() override;

 protected:
  std::string sword_hand_;

};

}
}


