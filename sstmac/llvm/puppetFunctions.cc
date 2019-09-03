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

bool hasShadow = true;

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

void ArielStartInst() {
  Ariel::ArielCommand ac;
  ac.command = Ariel::ARIEL_START_INSTRUCTION;
  APTunnel->writeMessage(0, ac);
}

void ArielEndInst() {
  Ariel::ArielCommand ac;
  ac.command = Ariel::ARIEL_END_INSTRUCTION;
  APTunnel->writeMessage(0, ac);
}

void startTrace() {
  while (!SPTunnel->allowPuppetEnter()) {
  }
  ++RegionRefCount;
}

void stopTrace() {
  --RegionRefCount;
  if (RegionRefCount == 0) { // This must happen in the main thread
    SPTunnel->setAllowShadowExit();
  }
}

} // namespace

extern "C" {
void sstmac_puppet_address_load(void *addr, int64_t size, int32_t thread_id) {
  ArielStartInst();
  auto ac = getArielControlMessage(Ariel::ARIEL_PERFORM_READ, addr, size);
  APTunnel->writeMessage(thread_id, ac);
  ArielEndInst();
}

void sstmac_puppet_address_store(void *addr, int64_t size, int32_t thread_id) {
  ArielStartInst();
  auto ac = getArielControlMessage(Ariel::ARIEL_PERFORM_WRITE, addr, size);
  APTunnel->writeMessage(thread_id, ac);
  ArielEndInst();
}

void sstmac_puppet_start_trace() {
  std::cout << "Start Trace(" << RegionRefCount++ << ")" << std::endl;
  if (SPTunnel != nullptr) { // Have shadow app
    startTrace();
  }
}

void sstmac_puppet_end_trace() {
  std::cout << "Stop Trace(" << RegionRefCount-- << ")" << std::endl;
  if (SPTunnel != nullptr) { // Have shadow app
    stopTrace();
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

void sstmac_puppet_init_no_shadow(int argc, char **argv) {
  // Init the Ariel Puppet Communication Tunnel
  std::string name(argv[1]);
  std::cout << "Starting app in";
  for (int i=3; i > 0; --i){
    std::cout << " ..." << i;
    std::cout.flush();
    sleep(1);
  }
  std::cout << std::endl;
  APTunnel = decltype(APTunnel)(new Ariel::ArielTunnel(name));
  std::cout << "APTunnel was initialized to: " << APTunnel.get() << std::endl;
}

void sstmac_puppet_finalize() {
  Ariel::ArielCommand ac;
  ac.command = Ariel::ARIEL_PERFORM_EXIT;
  APTunnel->writeMessage(0, ac);
  std::cout << "Finalized was called." << std::endl;
}

}
