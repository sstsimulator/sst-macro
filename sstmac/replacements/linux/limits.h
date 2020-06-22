#ifdef __APPLE__
//we are on apple simulating a linux environment
#define PATH_MAX 1024
#else
#include_next <linux/limits.h>
#endif
