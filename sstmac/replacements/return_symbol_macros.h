
#ifdef sstmac_must_return_free
#define free sstmac_free
#undef sstmac_must_return_free
#endif

#ifdef sstmac_must_return_memset
#undef memset
#define memset sstmac_memset
#undef sstmac_must_return_memset
#endif

#ifdef sstmac_must_return_memcpy
#undef memcpy
#define memcpy sstmac_memcpy
#undef sstmac_must_return_memcpy
#endif

#ifdef sstmac_must_return_gethostname
#undef gethostname
#define gethostname sstmac_gethostname
#undef sstmac_must_return_gethostname
#endif

#ifdef sstmac_must_return_mutex
#define mutex sstmac_mutex
#undef sstmac_must_return_mutex
#endif

#ifdef sstmac_must_return_getenv
#define getenv sstmac_getenv
#undef sstmac_must_return_getenv
#endif

#ifdef sstmac_must_return_setenv
#define setenv sstmac_setenv
#undef sstmac_must_return_setenv
#endif

#ifdef sstmac_must_return_putenv
#define putenv sstmac_putenv
#undef sstmac_must_return_putenv
#endif

