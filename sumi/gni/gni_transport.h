/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef mpi_mpi_transport_h
#define mpi_mpi_transport_h

#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>
#include <sumi/active_msg_transport.h>
#include <sumi/thread_lock.h>
#include <gni_pub.h>
#include <pmi.h>

DeclareDebugSlot(gni)


#define NUM_BITS_ID     8
#define NUM_BITS_EVENT  1
#define NUM_BITS_RANK   15
#define ID_BIT_OFFSET   (NUM_BITS_EVENT+NUM_BITS_RANK)

#define bitmaskRank ((1L<<NUM_BITS_RANK)-1)
#define bitmaskEvent ((1L<<NUM_BITS_EVENT)-1)
#define TID(id,ev,rank) ((uint32_t(id)<< ID_BIT_OFFSET) | uint32_t(ev) << NUM_BITS_RANK | uint32_t(rank))
#define RANK(tid) (tid & bitmaskRank)
#define EVENT(tid) ((tid>>NUM_BITS_RANK) & bitmaskEvent)
#define ID(tid) (tid>>ID_BIT_OFFSET)

#define gni_debug(...) \
  debug_printf(sprockit::dbg::gni, "Rank %d: %s", rank_, sprockit::printf(__VA_ARGS__).c_str())

#define gni_rc_error(rc, ...) \
    spkt_throw_printf(sprockit::value_error, "Failure on %d: %s : %s", \
         rank_, strerror(rc), sprockit::printf(__VA_ARGS__).c_str())

#define gni_error(...) \
    spkt_throw_printf(sprockit::value_error, "GNI operation failed : %s", \
         sprockit::printf(__VA_ARGS__).c_str())

#define wunderbahr GNI_RC_SUCCESS

namespace sumi {

struct gni_comm_context :
  public lockable
{
  gni_nic_handle_t nic_handle;
  gni_ep_handle_t* ep_handles;
  gni_cdm_handle_t cdm_handle;
  gni_cq_handle_t cq_handle;
};

class gni_transport :
  public active_msg_transport
{

  FactoryRegister("gni", transport, gni_transport,
              "Create a SUMI transport suitable for uGNI")
 public:
  gni_transport();

  virtual ~gni_transport();

  void init();

  void finalize();

  public_buffer allocate_public_buffer(int size);

  public_buffer make_public_buffer(void* buf, int size);

  void free_public_buffer(public_buffer buf, int size);

  void unmake_public_buffer(public_buffer buf, int size);

  bool supports_hardware_ack() const {
    return true;
  }

 protected:
  void block_inner_loop();

  void do_smsg_send(int dst, const message::ptr &msg);

  void do_rdma_get(int src, const message::ptr &msg);

  void do_send_terminate(int dst);

  void do_send_ping_request(int dst);

  void rdma_get_done(const message::ptr& msg);

  void do_rdma_put(int dst, const message::ptr &msg);

  void rdma_put_done(const message::ptr& msg);

  void do_nvram_get(int src, const message::ptr &msg);

  void go_die();

  void go_revive();

 private:
  int smsg_buffer_size_;

  typedef enum {
    RDMA_PUT_RECV_ACK,
    RDMA_GET_SEND_ACK,
    PAYLOAD,
    TERMINATE
  } header_type_t;

  void smsg_send(int dst, const message::ptr& msg, header_type_t type);

  void rdma_done(int src, gni_cq_entry_t& event_data, gni_cq_handle_t cqh);

  void send_cq_poll();

  void smsg_send_done(int src, int msg_id);

  void gather_peer_data();

  void gather_nic_data();

  struct pmi_env
  {
    const char *pmi_tag;
    const char *pmi_cookie;
    const char *pmi_dev_id;
    const char *pmi_addr_str;
    const char *pmi_job_id;
    uint32_t ptag;
    uint32_t cookie;
    uint32_t dev_id;
    uint32_t job_id;
  };

  typedef struct {
    int rank;
    uint32_t nic_addr;
  } nic_data_t;

  #define MAX_HOSTNAME_LENGTH 64
  typedef struct {
    int rank;
    char hostname[MAX_HOSTNAME_LENGTH];
    gni_smsg_attr_t smsg_attr;
    void* ping_buffer;
    gni_mem_handle_t ping_mem_handle;
  } peer_segment_data_t;

  gni_smsg_attr_t my_smsg_attr_;


  struct pending_smsg_t
  {
    int dst;
    void* header;
    uint32_t header_length;
    void* payload;
    uint32_t payload_length;
    bool done;
    gni_return_t rc;
  };

  struct smsg_endpoint_t
  {
    int rank;
    int refcount;
    bool queued;
    smsg_endpoint_t* next;
    smsg_endpoint_t* prev;
  };

  typedef enum {
    GET_ACK = 0,
    PUT_ACK = 1
  } remote_rdma_event_t;

  struct terminate_header_t
  {
    header_type_t type;
  };

  struct ack_header_t
  {
    header_type_t type;
    int tag;
  };

  struct smsg_payload_header_t
  {
    header_type_t type;
    int length;
    void* buffer;
  };

  void init_cdm();

  void init_cdm(gni_comm_context& c, pmi_env& env, int modes);

  void destroy_cdm(gni_comm_context& c);

  void init_smsg_metadata();

  void init_smsg_buffer();

  void init_smsg();

  void finalize_smsg_buffer();

  void delete_smsg_header(void* header_buf);

  void init_cq(gni_comm_context& c, gni_cq_handle_t* cqh, int num_entries);

  void finalize_cq(gni_cq_handle_t cq);

  void init_end_points(gni_comm_context& c);

  void finalize_end_points(gni_comm_context& c, bool wait);

  void print_post_descriptor(gni_post_descriptor_t* pd);

  void print_event_data(gni_cq_entry_t event_data);

  const char* strerror(gni_return_t);

  void register_mem(uint64_t length,
    void* buffer,
    gni_mem_handle_t* mem_handle,
    gni_nic_handle_t nic_handle,
    gni_cq_handle_t cq_handle);

  void unregister_mem(gni_nic_handle_t nic_handle,
    gni_mem_handle_t* mem_handle);

  void post_rdma(
    int dst,
    size_t length,
    int tag,
    void *local_buffer,
    gni_mem_handle_t local_mem_handle,
    void *remote_buffer,
    gni_mem_handle_t remote_mem_handle,
    gni_post_type_t post_type,
    uint64_t cqmode,
    remote_rdma_event_t ev,
    int transaction_id=-1);

  int progress_loop();

  void poll_smsg_queue();

  void smsg_recv();

  void smsg_poll();

  void smsg_queue_endpoint(smsg_endpoint_t *ep);

  void smsg_unqueue_endpoint(smsg_endpoint_t *ep);

  gni_return_t get_next_smsg(int src);

  void send_pending_smsg();

  void smsg_send(
    int dst,
    void* header,
    uint32_t header_length,
    void* payload,
    uint32_t payload_length);

  void rdma_poll();

  int allocate_rdma_tag(const message::ptr& msg);

  int* allocate_ping_buffer();

  void free_ping_buffer(int* buf);

  uint32_t allocate_smsg_id();

  uint32_t current_smsg_id_;
  static const uint32_t max_smsg_id_ = 10000000;

  static const int max_rdma_tag = 1000000;
  static const int ping_response_tag = 1000001;
  int current_rdma_tag_;
  std::map<int, message::ptr> rdma_messages_;
  std::map<int, void*> smsg_headers_;

  int* ping_buffer_;
  std::list<int*> ping_response_buffers_;
  gni_mem_handle_t ping_mem_handle_;

  gni_comm_context tx_context_;
  gni_cq_handle_t smsg_rx_cq_;
  gni_cq_handle_t rdma_rx_cq_;

  uint32_t* nic_addrs_;
  uint32_t cpu_id_;
  uint32_t my_global_nic_addr_;
  uint32_t my_local_nic_addr_;

  nic_data_t* nics_;
  uint16_t rdma_mode_;
  uint16_t dlvr_mode_;

  uint32_t smsg_bytes_total_;
  uint32_t smsg_bytes_mbox_;
  peer_segment_data_t* peers_;

  int smsg_get_count_;
  int smsg_poll_again_count_;
  bool in_progress_;
  int smsg_num_endpoints_queued_;
  smsg_endpoint_t* smsg_endpoint_head_;
  smsg_endpoint_t* smsg_endpoint_tail_;
  smsg_endpoint_t* smsg_endpoints_;
  void* smsg_data_;

  struct smsg_buffer {
    void* payload;
    int length;
    smsg_buffer(){}
    smsg_buffer(void* pload, int lngth) :
      payload(pload), length(lngth) {}
  };
  typedef std::map<int, smsg_buffer> smsg_match_map;
  typedef smsg_match_map::iterator smsg_match_iterator;

  smsg_match_map smsg_recvers_;
  smsg_match_map smsg_senders_;
  std::list<int> smsg_payloads_done_;
  std::list<pending_smsg_t*> pending_smsg_;

};

#define heisenbug fprintf(stderr, "rank %d %s:%d\n", rank_, __FILE__, __LINE__); fflush(stderr)

}

#endif // MPIMGR_H