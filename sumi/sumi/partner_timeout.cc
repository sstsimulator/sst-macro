#include <sumi/partner_timeout.h>

namespace sumi {

void
collective_timeout::time_out(int partner)
{
  actor_->partner_ping_failed(partner);
}

}

