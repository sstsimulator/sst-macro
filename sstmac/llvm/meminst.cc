#include <atomic>
#include <cstdint>
#include <iostream>
#include <mutex>
#include <numeric>
#include <vector>

#include "sst/elements/ariel/ariel_shmem.h"
#include "sstmac/software/process/operating_system.h"

namespace {

namespace smac = sstmac::sw;
namespace Ariel = SST::ArielComponent;

namespace puppet {

struct PuppetContext {
  Ariel::ArielTunnel *APTunnel = nullptr;
  smac::IPCTunnel<smac::ShadowPuppetSync> SPTunnel;

  PuppetContext(std::string const &SPTunnelName)
      : SPTunnel(SPTunnelName, false) {
    APTunnel = new Ariel::ArielTunnel(getSPSync()->getTunnelName());
  }

  smac::ShadowPuppetSync *getSPSync() { return SPTunnel.get(); }
  Ariel::ArielTunnel *getAPTunnel() { return APTunnel; }
};

PuppetContext *Ctx = nullptr;
std::atomic_int32_t RegionRefCount{0};

} // namespace puppet

namespace shadow {

smac::OperatingSystem *getCurrentOs() {
  return smac::OperatingSystem::currentOs();
}

smac::ShadowPuppetSync *getSPSync() { return getCurrentOs()->syncTunnel(); }

} // namespace shadow

} // namespace

extern "C" {
void sstmac_puppet_address_load(void *addr, int64_t size, int32_t thread_id) {
  Ariel::ArielCommand ac;
  ac.command = Ariel::ARIEL_PERFORM_READ;
  ac.instPtr = static_cast<uintptr_t>(NULL);
  ac.inst.addr = reinterpret_cast<uintptr_t>(addr);
  ac.inst.size = size;
  ac.inst.instClass = 0;
  ac.inst.simdElemCount = 0;

  puppet::Ctx->getAPTunnel()->writeMessage(thread_id, ac);
}

void sstmac_puppet_address_store(void *addr, int64_t size, int32_t thread_id) {
  Ariel::ArielCommand ac;
  ac.command = Ariel::ARIEL_PERFORM_WRITE;
  ac.instPtr = static_cast<uintptr_t>(NULL);
  ac.inst.addr = reinterpret_cast<uintptr_t>(addr);
  ac.inst.size = size;
  ac.inst.instClass = 0;
  ac.inst.simdElemCount = 0;

  puppet::Ctx->getAPTunnel()->writeMessage(thread_id, ac);
}

void sstmac_puppet_init() {
  std::string SPTunnelName = "manglythingy";
  puppet::Ctx = new puppet::PuppetContext(SPTunnelName);
}

void sstmac_puppet_finalize() { delete puppet::Ctx; }

void sstmac_shadow_init() {}
void sstmac_shadow_finalize() {}

void sstmac_puppet_start_trace() {
  while (!puppet::Ctx->getSPSync()->allowPuppetEnter()) {
  }
  ++puppet::RegionRefCount;
}

void sstmac_puppet_end_trace() {
  --puppet::RegionRefCount;
  if (puppet::RegionRefCount == 0) { // This must happen in the main thread
    puppet::Ctx->getSPSync()->setAllowShadowExit();
  }
}

void sstmac_shadow_start_trace() { shadow::getSPSync()->setAllowPuppetEnter(); }

void sstmac_shadow_end_trace() {
  while (!shadow::getSPSync()->allowShadowExit()) {
    shadow::getCurrentOs()->block();
  }
  shadow::getCurrentOs()->block();
}
}
