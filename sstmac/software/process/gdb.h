#ifndef sstmac_sw_process_gdb_h
#define sstmac_sw_process_gdb_h

#ifdef __cplusplus
extern "C" {
#endif

void sst_gdb_print_rank();

void sst_gdb_select_rank(int rank);

void sst_gdb_reset();

void sst_gdb_set_active(int flag);

#ifdef __cplusplus
}
#endif

#endif

