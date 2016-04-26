#ifndef sumi_message_FWD_H
#define sumi_message_FWD_H

#include <sprockit/refcount_ptr.h>

namespace sumi {

class message;

typedef sprockit::refcount_ptr<message> message_ptr;

}

#endif 
