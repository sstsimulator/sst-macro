
#ifdef free
#undef free
#define sstmac_must_return_free
#endif

#ifdef memset
#undef memset
#define sstmac_must_return_memset
#endif

