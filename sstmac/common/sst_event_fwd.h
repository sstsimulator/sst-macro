#ifndef sstmac_sst_EVENT_FWD_H
#define sstmac_sst_EVENT_FWD_H

#include <sstmac/common/sstmac_config.h>

#if SSTMAC_INTEGRATED_SST_CORE
namespace SST {
class Event;
}
namespace sstmac {
typedef SST::Event event;
}
#else
namespace sstmac {
class event;
}
#endif

namespace sstmac {
class event_queue_entry;
class callback;
}

#endif 

