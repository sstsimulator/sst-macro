#include <sstmac/hardware/router/routable_message.h>
#include <sprockit/serializer.h>

namespace sstmac {
namespace hw {

routable::routable(node_id toaddr, node_id fromaddr)
  : toaddr_(toaddr), fromaddr_(fromaddr), n_hops_(0)
{
}

void
routable::serialize_order(sprockit::serializer& ser)
{
  ser & toaddr_;
  ser & fromaddr_;
  ser & rinfo_;
}

}
}




