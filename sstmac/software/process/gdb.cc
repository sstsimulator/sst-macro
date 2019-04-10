#include <sstmac/software/process/gdb.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

extern "C"
void sst_gdb_select_rank(int rank){
  sstmac::sw::OperatingSystem::gdbSwitchToThread(rank);
}

extern "C"
void sst_gdbReset(){
  sstmac::sw::OperatingSystem::gdbReset();
}

extern "C"
void sst_gdbSetActive(int flag){
  sstmac::sw::OperatingSystem::gdbSetActive(flag);
}
