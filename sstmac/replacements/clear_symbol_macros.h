
#ifdef free
#undef free
#define sstmac_must_return_free
#endif

#ifdef memset
#undef memset
#define sstmac_must_return_memset
#endif

#ifdef memcpy
#undef memcpy
#define sstmac_must_return_memcpy
#endif

#ifdef gethostname
#undef gethostname
#define sstmac_must_return_gethostname
#endif

#ifdef mutex
#undef mutex
#define sstmac_must_return_mutex
#endif

#ifdef getenv
#undef getenv
#define sstmac_must_return_getenv
#endif

#ifdef setenv
#undef setenv
#define sstmac_must_return_setenv
#endif

#ifdef putenv
#undef putenv
#define sstmac_must_return_puttenv
#endif

