#ifndef sstmac_software_process_tls_H
#define sstmac_software_process_tls_H

#define TLS_GLOBAL_MAP 0
#define TLS_SANITY_CHECK sizeof(void*)
#define TLS_THREAD_ID (TLS_SANITY_CHECK + sizeof(int))

#endif

