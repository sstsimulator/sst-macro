#include <atomic>
#include <cstdint>
#include <iostream>
#include <mutex>
#include <numeric>
#include <vector>

#include "sst/elements/ariel/ariel_shmem.h"
#include "sstmac/software/process/operating_system.h"

namespace {

struct ShadowPuppetSharedData {};


namespace puppet {
namespace Ariel = SST::ArielComponent;

Ariel::ArielTunnel &getArielTunnel(char const *TunnelName) {
  static Ariel::ArielTunnel ArielPuppetTunnel(TunnelName);
  return ArielPuppetTunnel;
}

ShadowPuppetTunnel &getShadowPuppetTunnel() {
  std::string TunnelName = "mangledThingy";
  static ShadowPuppetTunnel Tunnel(TunnelName);
  return Tunnel;
}

struct PuppetContext {
  Ariel::ArielTunnel *ArielTunnel = nullptr;
  ShadowPuppetTunnel *SPTunnel = nullptr;

  PuppetContext(std::string const& SPTunnelName){
  }
} Ctx;

} // namespace puppet

namespace shadow {

namespace smac = sstmac::sw;
smac::OperatingSystem *getCurrentOs() {
  return smac::OperatingSystem::currentOs();
}


} // namespace shadow

} // namespace

extern "C" {
void sstmac_puppet_address_load(void *addr, int64_t size, int32_t thread_id) {}
void sstmac_puppet_address_store(void *addr, int64_t size, int32_t thread_id) {}

void sstmac_puppet_init() {
  auto &SPTunnel = puppet::getShadowPuppetTunnel();
  auto &ATunnel = puppet::getArielTunnel(SPTunnel.getArielTunnelName());
}
void sstmac_puppet_finalize() {}

void sstmac_shadow_init() {}
void sstmac_shadow_finalize() {}

void sstmac_puppet_start_trace() {}
void sstmac_puppet_end_trace() {}

void sstmac_shadow_start_trace() {}
void sstmac_shadow_end_trace() {}
}
