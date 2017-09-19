#ifndef sprockit_thread_safe_h
#define sprockit_thread_safe_h

#ifdef __APPLE__
#include <libkern/OSAtomic.h>
#define add_int32_atomic(inc,ptr) OSAtomicAdd32(inc,ptr)
#define add_int64_atomic(inc,ptr) OSAtomicAdd64(inc,ptr)
#define cas_int32(old,newVal,ptr) OSAtomicCompareAndSwap32(old,newVal,ptr)
#else
#define add_int32_atomic(inc,ptr) __sync_add_and_fetch(ptr,inc)
#define add_int64_atomic(inc,ptr) __sync_add_and_fetch(ptr,inc)
#define cas_int32(old,newVal,ptr) __sync_bool_compare_and_swap(ptr,old,newVal)
#endif

#endif

