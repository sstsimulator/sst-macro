#ifndef sstmac_common_ipc_event_h
#define sstmac_common_ipc_event_h

#include <cstdint>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sst_event_fwd.h>

namespace sstmac {

struct IpcEventBase {
  uint32_t ser_size;
  Timestamp t;
  uint64_t link;
  uint32_t seqnum;
  uint32_t rank;
  uint32_t thread;
};

struct IpcEvent : public IpcEventBase {
  Event* ev;
};

}

#endif
