#include "actor.h"

namespace sstmac {
namespace tutorial {

class MandyPatinkin :
  public Actor
{
 public:
  SST_ELI_REGISTER_DERIVED(
    Actor,
    MandyPatinkin,
    "macro",
    "patinkin",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "He's on CSI now?")

  MandyPatinkin(SST::Params& params);

  virtual void act() override;

 protected:
  std::string sword_hand_;

};

}
}
