#ifndef sumi_rdma_h
#define sumi_rdma_h

#include <sumi/rdma_mdata.h>

namespace sumi {

struct public_buffer :
 public public_buffer_base
{
 public:
  explicit public_buffer(void* buf){
    ptr = buf;
  }

  public_buffer() {
    ptr = nullptr;
  }

  void offset_ptr(int offset) {
    if (ptr){
      public_buffer_base::offset_ptr(offset);
    }
  }

};

}


#endif // RDMA_H

