#ifndef MESSAGE_FWD_H
#define MESSAGE_FWD_H

#include <sprockit/clonable.h>
#include <sprockit/ser_ptr_type.h>

namespace sstmac {

class transport_message;
namespace sumi {
typedef sprockit::clonable<sprockit::serializable_ptr_type> payload_t;
typedef sprockit::refcount_ptr<payload_t> payload_ptr;
}

}

#endif // MESSAGE_FWD_H

