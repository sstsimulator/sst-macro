#include <pthread.h>
#include <sumi/transport.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/serializer.h>

using namespace sumi;

#define BUFSIZE 1024
#define EAGERSIZE 100
#define DEBUG 0

class sync_message :
  public message,
  public sprockit::serializable_type<sync_message>
{
 ImplementSerializable(sync_message)

 public:
  typedef sprockit::refcount_ptr<sync_message> ptr;

 public:
  public_buffer put_buffer;
  public_buffer get_buffer;
  public_buffer send_buffer;
  int tid;

  virtual void
  serialize_order(sprockit::serializer &ser){
    ser & put_buffer;
    ser & get_buffer;
    ser & send_buffer;
    ser & tid;
    message::serialize_order(ser);
  }

};

class eager_message :
  public message,
  public sprockit::serializable_type<eager_message>
{
 ImplementSerializableDefaultConstructor(eager_message)

 public:
  typedef sprockit::refcount_ptr<eager_message> ptr;

 public:
  int size;
  void* buffer;

  virtual void
  serialize_order(sprockit::serializer &ser){
    message::serialize_order(ser);
    ser & sprockit::buffer(buffer, size);
  }

  eager_message(){
    class_ = pt2pt;
  }

};

DeclareSerializable(sync_message);
DeclareSerializable(eager_message);

void*
msg_thread_run(void* args);

static inline int
val(int rank, int idx){
  return idx*10 + rank;
}

struct thread_data_t {
  int* eager_recv_buf;
  int* unexp_rdma_recv_buf;
  transport* t;
};

void
run_test()
{
  sprockit::sim_parameters params;
  params["ping_timeout"] = "100ms";
  params["transport"] = DEFAULT_TRANSPORT;
  transport* t = transport_factory::get_param("transport", &params);

  t->init();
  printf("Rank %d initialized\n", t->rank());
  //t->start_heartbeat(1); //1 s

  int partner = (t->rank() + 1) % 2; //0->1, 1->0

  public_buffer send_buf = t->allocate_public_buffer(BUFSIZE*sizeof(int));
  public_buffer put_recv_buf = t->allocate_public_buffer(BUFSIZE*sizeof(int));
  public_buffer get_recv_buf = t->allocate_public_buffer(BUFSIZE*sizeof(int));
  int* eager_send_buf = new int[EAGERSIZE];
  int* sender = (int*) send_buf.ptr;
  int* put_recver = (int*) put_recv_buf.ptr;
  int* get_recver = (int*) get_recv_buf.ptr;

  int me = t->rank();
  for (int i=0; i < BUFSIZE; ++i){
    sender[i] = val(me,i);
  }

  for (int i=0; i < EAGERSIZE; ++i){
    eager_send_buf[i] = val(me,i);
  }

  sync_message::ptr my_smsg = new sync_message;
  my_smsg->set_class_type(message::pt2pt);
  my_smsg->put_buffer = put_recv_buf;
  my_smsg->get_buffer = get_recv_buf;
  my_smsg->send_buffer = send_buf;
  printf("Rank %d sent remote buffers %p, %p, %p\n",
    t->rank(), (void*) put_recv_buf, (void*) get_recv_buf, (void*) send_buf);
  t->send_header(partner, my_smsg);

  sync_message::ptr his_smsg = ptr_safe_cast(sync_message, t->blocking_poll());

  printf("Rank %d got remote buffers %p, %p, %p and tid %d\n",
    t->rank(),
    (void*) his_smsg->put_buffer,
    (void*) his_smsg->get_buffer,
    (void*) his_smsg->send_buffer,
    his_smsg->tid);

  thread_data_t tdata;
  tdata.t = t;
  tdata.eager_recv_buf = 0;

  pthread_t thr;
  pthread_create(&thr, NULL, msg_thread_run, &tdata);

  rdma_message::ptr rdma_get_msg = new rdma_message;
  rdma_get_msg->local_buffer() = get_recv_buf;
  rdma_get_msg->remote_buffer() = his_smsg->send_buffer;
  rdma_get_msg->set_byte_length(BUFSIZE*sizeof(int));
  t->rdma_get(partner, rdma_get_msg);

  rdma_message::ptr rdma_put_msg = new rdma_message;
  rdma_put_msg->local_buffer() = send_buf;
  rdma_put_msg->remote_buffer() = his_smsg->put_buffer;
  rdma_put_msg->set_byte_length(BUFSIZE*sizeof(int));
  t->rdma_put(partner, rdma_put_msg);

  rdma_message::ptr unexpected_rdma_msg = new rdma_message;
  unexpected_rdma_msg->remote_buffer() = send_buf;
  unexpected_rdma_msg->set_byte_length(BUFSIZE*sizeof(int));
  t->send_unexpected_rdma(partner, unexpected_rdma_msg);

  eager_message::ptr emsg = new eager_message;
  emsg->buffer = eager_send_buf;
  emsg->size = EAGERSIZE * sizeof(int);
  t->send_payload(partner, emsg);


  t->send_self_terminate();

  void* ignore;
  pthread_join(thr, &ignore);

  int* unexp_recv_buf = tdata.unexp_rdma_recv_buf;
  for (int i=0; i < BUFSIZE; ++i){
    int correct = val(partner,i);
    if (put_recver[i] != correct){
      std::cerr << sprockit::printf("Rank %d: put buffer incorrect: a[%d] = %d != %d",
        me, i, put_recver[i], correct);
      abort();
    }
    if (get_recver[i] != correct){
      std::cerr << sprockit::printf("Rank %d: get buffer incorrect: a[%d] = %d != %d",
        me, i, get_recver[i], correct);
      abort();
    }
    if (unexp_recv_buf[i] != correct){
      std::cerr << sprockit::printf("Rank %d: unexp buffer incorrect: a[%d] = %d != %d",
        me, i, unexp_recv_buf[i], correct);
      abort();
    }
  }

  int* eager_recv_buf = tdata.eager_recv_buf;
  if (eager_recv_buf == 0){
    std::cerr << "Rank " << me << " got no eager recv buffer" << std::endl;
    abort();
  }

  for (int i=0; i < EAGERSIZE; ++i){
    int correct = val(partner, i);
    if (eager_recv_buf[i] != correct){
       std::cerr << sprockit::printf("Rank %d: eager buffer incorrect: a[%d] = %d != %d\n",
        me, i, eager_recv_buf[i], correct);
      abort();
    }
  }


  //t->stop_heartbeat();
  t->finalize();
}

int main(int argc, char** argv)
{


  try {
#if DEBUG
  sprockit::debug::turn_on(DEFAULT_TRANSPORT);
  sprockit::debug::turn_on("sumi");
  sprockit::debug::turn_on("sumi_collective");
  sprockit::debug::turn_on("sumi_ping");
#endif
    run_test();
  } catch (std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }

  return 0;
}


void*
msg_thread_run(void* args)
{
  int num_to_receive = 7;
  //rdma get, rdma get ack, rdma put, eager terminate
  //unexp rdma recv, unexp rdma send ack
  bool terminated = false;
  int num_received = 0;
  try{
    thread_data_t* tdata = (thread_data_t*) args;
    transport* t = tdata->t;
    while (num_received < num_to_receive || !terminated){
      message::ptr msg = t->blocking_poll();
      printf("Rank %d got %s\n", t->rank(), msg->to_string().c_str());
      ++num_received;
      if (msg->class_type() == message::terminate){
        terminated = true;
      }
      else if (msg->payload_type() == message::eager_payload){
        eager_message::ptr emsg = ptr_safe_cast(eager_message, msg);
        tdata->eager_recv_buf = (int*) emsg->buffer;
      }
      else if (msg->class_type() == message::unexpected &&
        msg->payload_type() == message::rdma_get){
        tdata->unexp_rdma_recv_buf = (int*) msg->local_buffer().ptr;
      }
    }
  } catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
    abort();
  }
  return 0;
}
