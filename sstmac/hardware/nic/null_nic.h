#ifndef NULLNIC_H
#define NULLNIC_H

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/common/recv_cq.h>

namespace sstmac {
namespace hw {

class null_nic:
  public nic
{

 public:
  virtual ~null_nic() throw () {}

  std::string
  to_string() const {
    return sprockit::printf("Null NIC(%d)", int(my_addr_));
  }

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod);

 protected:
  virtual void
  recv_chunk(const sst_message::ptr& chunk);

  virtual void
  do_send(const network_message::ptr& payload);

  timestamp
  injection_latency() const;

 protected:
  recv_cq completion_queue_;

};

}
}


#endif // NULLNIC_H

