#ifndef EVENT_MANAGER_FWD_H
#define EVENT_MANAGER_FWD_H

#include <sstmac/common/sstmac_config.h>

namespace sstmac {

#if SSTMAC_INTEGRATED_SST_CORE
class event_scheduler;
typedef event_scheduler event_manager;
#else
class event_manager;
#endif

}

#endif // EVENT_MANAGER_FWD_H

