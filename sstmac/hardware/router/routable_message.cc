#include <sstmac/hardware/router/routable_message.h>

namespace sstmac {
namespace hw {

routable::routable(node_id toaddr, node_id fromaddr)
  : toaddr_(toaddr), fromaddr_(fromaddr)
{
}

void
routable::serialize_order(serializer& ser)
{
  ser & toaddr_;
  ser & fromaddr_;
  ser & rinfo_;
}

}
}




