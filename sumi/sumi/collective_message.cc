#include <sumi/collective_message.h>
#include <sumi/serialization.h>

namespace sumi {

#define enumcase(x) case x: return #x;
const char*
collective_work_message::tostr(action_t action)
{
  switch(action)
  {
    enumcase(eager_payload);
    enumcase(nack_eager);
    enumcase(nack_get_header);
    enumcase(nack_put_header);
    enumcase(nack_put_payload);
    enumcase(nack_get_ack);
    enumcase(rdma_get_header);
    enumcase(rdma_put_header);
    enumcase(get_data);
    enumcase(put_data);
  }
  spkt_throw_printf(sprockit::value_error,
    "collective_work_message::invalid action %d",
    action);
}

void
collective_work_message::serialize_order(sumi::serializer &ser)
{
  message::serialize_order(ser);
  ser & action_;
  ser & tag_;
  ser & type_;
  ser & round_;
  ser & dense_sender_;
  ser & dense_recver_;
  ser & failed_procs_;
}

std::string
collective_work_message::to_string() const
{
  return sprockit::printf(
    "message %p for collective %s event %s "
    "recver=%d(%d) sender=%d(%d) nbytes=%d round=%d tag=%d",
    this, collective::tostr(type_), message::tostr(message::payload_type_),
    dense_recver_, recver(), dense_sender_, sender(), num_bytes_, round_, tag_);
}

void
collective_work_message::append_failed(const thread_safe_set<int>& failed)
{
  thread_safe_set<int>::iterator end = failed.start_iteration();
  failed_procs_.insert(failed.begin(), end);
  failed.end_iteration();
}

void
collective_work_message::clone_into(collective_work_message* cln) const
{
  message::clone_into(cln);
  cln->tag_ = tag_;
  cln->type_ = type_;
  cln->round_ = round_;
  cln->dense_sender_ = dense_sender_;
  cln->dense_recver_ = dense_recver_;
}

message*
collective_done_message::clone() const
{
  spkt_throw(sprockit::unimplemented_error,
    "collective_done_message::clone");
  return 0;
}

void
collective_work_message::reverse()
{
  message::reverse();
  int tmp = dense_recver_;
  dense_recver_ = dense_sender_;
  dense_sender_ = tmp;
}

}
