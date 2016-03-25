#include <gni/gni_transport.h>
#include <list>
#include <cstring>

namespace sumi {

void
gni_transport::register_mem(uint64_t length, void* buffer, gni_mem_handle_t* mem_handle, gni_nic_handle_t nic_handle, gni_cq_handle_t cq_handle)
{
  gni_return_t rc = GNI_MemRegister(nic_handle,
                                    (unsigned long) buffer,
                                    length,
                                    cq_handle,
                                    GNI_MEM_READWRITE,
                                    -1, mem_handle);

  gni_debug("Registering buffer %p of length %lu on node %d at handle (%lu,%lu)",
          buffer, length, rank_, mem_handle->qword1, mem_handle->qword2);


  if (rc != wunderbahr){
    if (rc == GNI_RC_ERROR_RESOURCE){
        gni_error("Could not register memory region %p of size %lu", buffer, length);
    }
    else{
        gni_rc_error(rc, "MemRegister buffer %p of length %lu", buffer, length);
    }
  }
}

public_buffer
gni_transport::allocate_public_buffer(int size)
{
  void* buf = ::malloc(size);
  return make_public_buffer(buf, size);
}

public_buffer
gni_transport::make_public_buffer(void *buf, int size)
{
  public_buffer pbuf;
  pbuf.ptr = buf;
  register_mem(size, buf, &pbuf.mem_handle, tx_context_.nic_handle, rdma_rx_cq_);
  return pbuf;
}

void
gni_transport::unregister_mem(gni_nic_handle_t nic_handle, gni_mem_handle_t* mem_handle)
{
  gni_debug("Unregistering on node %d at handle (%lu,%lu)",
            rank_, mem_handle->qword1, mem_handle->qword2);

  gni_return_t rc = GNI_MemDeregister(nic_handle, mem_handle);
  if (rc != wunderbahr){
    gni_rc_error(rc, "MemDeregister");
  }
}

void
gni_transport::unmake_public_buffer(public_buffer buf, int size)
{
  unregister_mem(tx_context_.nic_handle, &buf.mem_handle);
}

void
gni_transport::free_public_buffer(public_buffer buf, int size)
{
  unregister_mem(tx_context_.nic_handle, &buf.mem_handle);
  ::free(buf.ptr);
}

}
