#ifndef RDMA_MDATA_H
#define RDMA_MDATA_H

namespace sumi {

struct public_buffer_base
{
  void* ptr;

  operator void*() const {
    return ptr;
  }

  operator char*() const {
    return (char*)ptr;
  }

  operator bool() const {
    return ptr;
  }

  void
  offset_ptr(int offset) {
    ptr = ((char*)ptr) + offset;
  }

  void
  operator[](int idx){
   //don't ever use this directly
  }

  template <class T>
  T * data() { return static_cast<T *>(ptr); }

  template <class T>
  const T * data() const { return static_cast<const T *>(ptr); }

};

}

#endif // RDMA_MDATA_H
