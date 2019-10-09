#include "sst/elements/ariel/ariel_shmem.h"
#include "sstmac/software/process/operating_system.h"

namespace {
namespace smac = sstmac::sw;

smac::OperatingSystem *getCurrentOs() {
  return smac::OperatingSystem::currentOs();
}

smac::ShadowPuppetSync *getSPSync() { return getCurrentOs()->syncTunnel(); }

} // namespace

extern "C" {

void sstmac_shadow_init() {}
void sstmac_shadow_finalize() {}

void sstmac_shadow_start_trace() { getSPSync()->setAllowPuppetEnter(); }

void sstmac_shadow_end_trace() {
  while (!getSPSync()->allowShadowExit()) {
    getCurrentOs()->block();
  }
  getCurrentOs()->block();
}

}
