#ifndef sstmac_common_ipc_event_h
#define sstmac_common_ipc_event_h

#include <cstdint>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sst_event_fwd.h>

namespace sstmac {

struct ipc_event_base {
  timestamp t;
  uint32_t dst;
  uint32_t src;
  uint32_t seqnum;
  int port;
  int rank;
  bool credit;
};

struct ipc_event_t : public ipc_event_base {
  event* ev;
};

}

#endif
