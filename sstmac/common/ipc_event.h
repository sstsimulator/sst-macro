#ifndef sstmac_common_ipc_event_h
#define sstmac_common_ipc_event_h

#include <cstdint>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/sst_event_fwd.h>

namespace sstmac {

struct IpcEventBase {
  uint32_t ser_size;
  uint32_t dst;
  Timestamp t;
  uint32_t src;
  uint32_t link;
  uint32_t seqnum;
  int port;
  int rank;
  bool credit;
};

struct IpcEvent : public IpcEventBase {
  Event* ev;
};

}

#endif
