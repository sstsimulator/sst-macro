
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

