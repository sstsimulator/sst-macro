#include <sstmac/common/messages/sleep_message.h>
#include <sprockit/serializer.h>

namespace sstmac {

void
sleep_message::serialize_order(sprockit::serializer& ser)
{
  library_interface::serialize_order(ser);
  timed_interface::serialize_order(ser);
  sst_message::serialize_order(ser);
}

}
