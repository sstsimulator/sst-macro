#ifndef RDMA_interface_H
#define RDMA_interface_H

#include <sumi/config.h>

#define ImplementRdmaAPI \
public: \
 sumi::public_buffer& local_buffer() { return local_buffer_; } \
 sumi::public_buffer& remote_buffer() { return remote_buffer_; } \
private: \
 sumi::public_buffer local_buffer_; \
 sumi::public_buffer remote_buffer_;

#endif

