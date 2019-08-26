#include "sst/elements/ariel/ariel_shmem.h"
#include "sstmac/software/process/ipc_tunnel.h"

#include <atomic>
#include <cstdint>
#include <memory>
#include <string>

namespace {

namespace smac = sstmac::sw;
namespace Ariel = SST::ArielComponent;

using SPSync = smac::ShadowPuppetSync;
using IPCType = smac::IPCTunnel<SPSync>;
using SPTunnelType = std::unique_ptr<IPCType>;

std::unique_ptr<Ariel::ArielTunnel> APTunnel = nullptr;
std::unique_ptr<IPCType> IPCTunnel = nullptr;

// Will just be a reference to the IPCTunnel memeber
SPSync *SPTunnel = nullptr;

std::atomic_int32_t RegionRefCount{0};

Ariel::ArielCommand getArielControlMessage(Ariel::ArielShmemCmd_t cmd,
                                           void *addr, int size) {
  Ariel::ArielCommand ac;
  ac.command = cmd;
  ac.instPtr = static_cast<uintptr_t>(NULL);
  ac.inst.addr = reinterpret_cast<uintptr_t>(addr);
  ac.inst.size = size;
  ac.inst.instClass = 0;
  ac.inst.simdElemCount = 0;

  return ac;
}
} // namespace

extern "C" {
void sstmac_puppet_address_load(void *addr, int64_t size, int32_t thread_id) {
  auto ac = getArielControlMessage(Ariel::ARIEL_PERFORM_READ, addr, size);
  APTunnel->writeMessage(thread_id, ac);
}

void sstmac_puppet_address_store(void *addr, int64_t size, int32_t thread_id) {
  auto ac = getArielControlMessage(Ariel::ARIEL_PERFORM_WRITE, addr, size);
  APTunnel->writeMessage(thread_id, ac);
}

void sstmac_puppet_start_trace() {
  while (!SPTunnel->allowPuppetEnter()) {
  }
  ++RegionRefCount;
}

void sstmac_puppet_end_trace() {
  --RegionRefCount;
  if (RegionRefCount == 0) { // This must happen in the main thread
    SPTunnel->setAllowShadowExit();
  }
}

void sstmac_puppet_init(std::string TunnelName = "manglythingy") {
  // Init the Shadow Puppet Communication Tunnel
  IPCTunnel = decltype(IPCTunnel)(new IPCType(std::move(TunnelName), false));
  SPTunnel = IPCTunnel->get();

  // Init the Ariel Puppet Communication Tunnel
  APTunnel =
      decltype(APTunnel)(new Ariel::ArielTunnel(SPTunnel->getTunnelName()));
}

void sstmac_puppet_finalize() {}
}
