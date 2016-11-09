#ifndef MESSAGE_BUFFER_CACHE_H
#define MESSAGE_BUFFER_CACHE_H

#include <list>
#include <string>

namespace sstmac {

class message_buffer_cache  {
 public:

 public:
  void*
  pop();

  void
  push(void* buffer);

  message_buffer_cache(int buf_size, int num_bufs_window) :
    buf_size_(buf_size),
    num_bufs_window_(num_bufs_window)
  {
  }

  message_buffer_cache(){}

  void
  init(int buf_size, int num_bufs_window){
    buf_size_ = buf_size;
    num_bufs_window_ = num_bufs_window;
  }

 protected:
   std::list<void*> buffers_;

   int buf_size_;

   int num_bufs_window_;

};

}

#endif // MESSAGE_BUFFER_CACHE_H
