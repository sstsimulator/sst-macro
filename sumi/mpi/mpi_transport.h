#ifndef mpi_mpi_transport_h
#define mpi_mpi_transport_h

#include <sumi/monitor.h>
#include <sumi/timeout.h>
#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/transport.h>
#include <sumi/comm_functions.h>
#include <sumi/active_msg_transport.h>
#include <mpi.h>

namespace sumi {


class PendingMPI
{
 public:
  typedef enum {
    SmsgSend,
    SmsgRecv,
    RecvGetReq,
    SendGetReq,
    RecvPutReq,
    SendPutReq,
    SendPutAck,
    RDMAGetRecv,
    RDMAGetSend,
    RDMAPutRecv,
    RDMAPutSend,
    SendPingRequest,
    SendPingResponse,
    Null
  } type_t;


  int size;
  int sender;
  int recver;
  int rdma_tag;
  void* send_buf;
  void* recv_buf;
  type_t type;
  PendingMPI* rdma_req;
  int smsg_tag;
  MPI_Request* req;
  message::ptr msg;
  int id;

  PendingMPI();

  void clear();

  static const char*
  tostr(type_t ty);

};


class mpi_transport :
  public active_msg_transport
{
  FactoryRegister("mpi", transport, mpi_transport,
              "Create a SUMI transport suitable for MPI")
 public:
  mpi_transport();

  virtual ~mpi_transport();

  void init();

  void finalize();

  void wait_on_pending();

  public_buffer allocate_public_buffer(int size){
    return public_buffer(::malloc(size));
  }

  public_buffer make_public_buffer(void* buf, int size){
    return public_buffer(buf); //nothing to do
  }

 protected:
  void block_inner_loop();

  void do_send_terminate(int dst);

  void do_send_ping_request(int dst);

  void do_smsg_send(int dst, const message::ptr &msg);

  void do_rdma_get(int src, const message::ptr &msg);

  void do_rdma_put(int dst, const message::ptr &msg);

  void do_nvram_get(int src, const message::ptr &msg);

 private:
  int ping_status_;

  int max_num_requests_;

  int poll_burst_size_;

  MPI_Request* requests_;

  PendingMPI* pending_;

  std::list<PendingMPI*> pending_pool_;

  std::list<PendingMPI*> pending_mpi_;

  std::list<MPI_Request*> request_pool_;

  void new_incoming_msg(int src, int tag, int size);

  void recv_ping_request(int src);

  void recv_ping_response(int src);

  void recv_smsg(int src, int tag, int size);

  void recv_rdma_req(int src, int size, int tag, PendingMPI::type_t ty);

  void process_rdma_get_req(PendingMPI *pending);

  void process_rdma_put_req(PendingMPI *pending);

  void process_smsg(PendingMPI* pending);

  void send_transaction_ack(int dst, const message::ptr& msg);

  void recv_transaction_ack(int src);

  void rdma_get_ack(const message::ptr& msg);

  message::ptr
  deserialize_smsg(PendingMPI* pending, void* extra_md = 0, int md_size = 0);

  typedef enum {
   mpi_rdma_get_tag = 0,
   mpi_rdma_put_tag = 1,
   smsg_send_tag = 2,
   ping_request_tag = 3,
   ping_response_tag = 4,
   transaction_ack = 5,
   terminate_tag = 6,
   rdma_get_payload_tag = 10,
   rdma_put_payload_tag = 16000
  } tag_t;

  static const char*
  tostr(ping_status_t stat);

  static const char*
  tostr(tag_t tag);

  void add_pending(PendingMPI* pending);

  void free_pending(PendingMPI* pending);

  PendingMPI* allocate_pending();

  void clear_pending(PendingMPI* pending);

  void poll();

  void poll_burst();

  void recv_terminate(int src);

  void go_die();

  void go_revive();

  char* allocate_smsg_buffer();

  void transport_smsg_send(int dst, int tag, PendingMPI::type_t ty,
    const message::ptr& msg,
    void* extra_md = 0, int md_size = 0);

};

}

#endif // MPIMGR_H
