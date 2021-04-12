#ifdef __APPLE__
#ifdef __cplusplus
extern "C"
#endif
ssize_t sstmac_sendfile(int out_fd, int in_fd, off_t *offset, size_t count);
#define sendfile sstmac_sendfile
#else
#include_next <sys/sendfile.h>
#endif
