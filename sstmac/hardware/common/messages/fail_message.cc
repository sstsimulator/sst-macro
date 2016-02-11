#include <sstmac/hardware/common/messages/fail_message.h>

namespace sstmac {
namespace hw {

RegisterEnum(sst_message::message_type_t,fail_message::FAILURE);

fail_message::fail_message()
{
 sst_message::msgtype_ = FAILURE;
}

}
}
