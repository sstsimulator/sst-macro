#include <sstmac/software/process/gdb.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

extern "C"
void sst_gdb_select_rank(int rank){
  sstmac::sw::operating_system::gdb_switch_to_thread(rank);
}

extern "C"
void sst_gdb_reset(){
  sstmac::sw::operating_system::gdb_reset();
}

extern "C"
void sst_gdb_set_active(int flag){
  sstmac::sw::operating_system::gdb_set_active(flag);
}
