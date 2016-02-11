#include <sstmac/software/libraries/unblock_handler.h>
#include <sstmac/software/process/operating_system.h>

namespace sstmac {
namespace sw {

void
unblock_handler::handle(const sst_message::ptr& msg)
{
  os_->unblock(key_);
}

}
}
