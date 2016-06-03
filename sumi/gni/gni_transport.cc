#include <gni/gni_transport.h>
#include <sprockit/sim_parameters.h>
#include <sys/time.h>


RegisterDebugSlot(gni);

namespace sumi {

SpktRegister("gni", transport, gni_transport,
            "Create a SUMI transport suitable for uGNI");

#define enumcase(x) case x: return #x

gni_transport::gni_transport()  :
  dlvr_mode_(GNI_DLVMODE_PERFORMANCE),
  rdma_mode_(GNI_RDMAMODE_PHYS_ADDR),
  smsg_get_count_(0),
  smsg_poll_again_count_(50),
  in_progress_(false),
  smsg_num_endpoints_queued_(0),
  smsg_endpoint_head_(0),
  smsg_endpoint_tail_(0),
  current_rdma_tag_(0),
  smsg_data_(0),
  current_smsg_id_(15)
{
  max_transaction_id_ = 1<<NUM_BITS_ID;
}

gni_transport::~gni_transport()
{
}

uint32_t
gni_transport::allocate_smsg_id()
{
  uint32_t id = current_smsg_id_;
  current_smsg_id_ = (current_smsg_id_ + 1) % max_smsg_id_;
  return id;
}

int
gni_transport::allocate_rdma_tag(const message::ptr& msg)
{
  int tag = current_rdma_tag_;
  current_rdma_tag_ = (current_rdma_tag_ + 1) % max_rdma_tag;
  return tag;

  rdma_messages_[tag] = msg;
  return tag;
}

void
gni_transport::do_send_terminate(int dst)
{
  terminate_header_t* header = new terminate_header_t;
  header->type = TERMINATE;
  smsg_send(dst, header, sizeof(terminate_header_t), 0, 0);
}

void
gni_transport::go_die()
{
  *ping_buffer_ = i_am_dead;
}

void
gni_transport::go_revive()
{
 *ping_buffer_ = i_am_alive;
}

int*
gni_transport::allocate_ping_buffer()
{
  int* buf = ping_response_buffers_.front();
  ping_response_buffers_.pop_front();
  return buf;
}

void
gni_transport::free_ping_buffer(int* buf)
{
  ping_response_buffers_.push_back(buf);
}

void
gni_transport::do_send_ping_request(int dst)
{
  peer_segment_data_t& peer = peers_[dst];
  int* ping_response_buf = allocate_ping_buffer();
  post_rdma(dst, sizeof(int), ping_response_tag,
    ping_response_buf, ping_mem_handle_,
    peer.ping_buffer, peer.ping_mem_handle,
    GNI_POST_RDMA_GET, GNI_CQMODE_LOCAL_EVENT, GET_ACK);
}

void
gni_transport::block_inner_loop()
{
  smsg_poll();
  send_cq_poll();
  rdma_poll();
  renew_pings();
}

void
gni_transport::do_nvram_get(int dst, const message::ptr& msg)
{
  spkt_throw(sprockit::unimplemented_error,
	"gni_transport::do_nvram_get");
}

}
