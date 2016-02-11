#include <sstmac/common/messages/message_buffer_cache.h>
#include <sprockit/errors.h>

namespace sstmac {

void*
message_buffer_cache::pop()
{
  if (buffers_.empty()){
    char* new_buf = new char[buf_size_ * num_bufs_window_];
    char* bufptr = new_buf;
    for (int i=0; i < num_bufs_window_; ++i, bufptr += buf_size_){
      buffers_.push_back(bufptr);
    }
  }

  void* ret = buffers_.front();
  buffers_.pop_front();
  return ret;
}

void
message_buffer_cache::push(void *buffer)
{
  buffers_.push_front(buffer);
}

}
