#include <sstmac/hardware/snappr/snappr_inport.h>
#include <sstmac/hardware/snappr/snappr_switch.h>

namespace sstmac {
namespace hw {

void
SnapprInPort::handle(Event *ev)
{
  parent->handlePayload(static_cast<SnapprPacket*>(ev), number);
}

std::string
SnapprInPort::toString() const
{
  return sprockit::printf("SNAPPR InPort %d", number);
}

}
}

