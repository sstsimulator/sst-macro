#include <sstmac/common/messages/library_message.h>
#include <sprockit/serializer.h>

namespace sstmac {

void
library_interface::serialize_order(sprockit::serializer& ser)
{
  ser & lib_name_;
}

}
