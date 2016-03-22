#include <sstmac/common/messages/timed_event.h>
#include <sprockit/serializer.h>

namespace sstmac {

void
timed_interface::serialize_order(sprockit::serializer& ser)
{
  ser & time_;
}

}
