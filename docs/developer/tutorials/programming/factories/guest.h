#include "actor.h"

namespace sstmac {
namespace tutorial {

class ChristopherGuest :
  public Actor
{

 public:
  SST_ELI_REGISTER_DERIVED(
    Actor,
    ChristopherGuest,
    "macro",
    "guest",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Renowned writer, actor, improvisor of Spinal Tap fame")

  ChristopherGuest(SST::Params& params);

  virtual void act() override;

 protected:
  int num_fingers_;

};

}
}
