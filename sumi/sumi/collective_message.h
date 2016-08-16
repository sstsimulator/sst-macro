#ifndef sumi_api_COLLECTIVE_MESSAGE_H
#define sumi_api_COLLECTIVE_MESSAGE_H

#include <sumi/message.h>
#include <sumi/collective.h>
#include <sumi/thread_safe_set.h>

namespace sumi {

/**
 * @class collective_done_message
 * The message that is actually delivered when calling #sumi::comm_poll
 * This encapsulates all the information about a collective that has completed in the background
 */
class collective_done_message :
  public message
{
 public:
  typedef sprockit::refcount_ptr<collective_done_message> ptr;

 public:
  std::string
  to_string() const {
    return "collective done message";
  }

  collective_done_message(int tag, collective::type_t ty,
                          communicator* dom) :
    tag_(tag), result_(0), vote_(0), type_(ty),
    all_ranks_know_failure_(false), dom_(dom)
  {
    class_ = collective_done;
    payload_type_ = none;
  }

  int
  tag() const {
    return tag_;
  }

  collective::type_t
  type() const {
    return type_;
  }

  communicator*
  dom() const {
    return dom_;
  }

  void
  set_type(collective::type_t ty) {
    type_ = ty;
  }

  bool
  failed() const {
    return !failed_procs_.empty();
  }

  bool
  succeeded() const {
    return failed_procs_.empty();
  }

  void
  append_failed(int proc) {
    failed_procs_.insert(proc);
  }

  void
  append_failed(const std::set<int>& procs){
    failed_procs_.insert(procs.begin(), procs.end());
  }

  const thread_safe_set<int>&
  failed_procs() const {
    return failed_procs_;
  }

  bool
  all_ranks_know_failure() const {
    return all_ranks_know_failure_;
  }

  void
  set_all_ranks_know_failure(bool flag) {
    all_ranks_know_failure_ = true;
  }

  void
  set_result(void* buf){
    result_ = buf;
  }

  void*
  result() {
    return result_;
  }

  void
  set_vote(int v){
    vote_ = v;
  }

  int
  vote() const {
    return vote_;
  }

  message*
  clone() const;

  int comm_rank() const {
    return comm_rank_;
  }

  void
  set_comm_rank(int rank){
    comm_rank_ = rank;
  }

 protected:
  int tag_;
  void* result_;
  int vote_;
  collective::type_t type_;
  thread_safe_set<int> failed_procs_;
  bool all_ranks_know_failure_;
  int comm_rank_;
  communicator* dom_;

};

/**
 * @class collective_work_message
 * Main message type used by collectives
 */
class collective_work_message :
  public message
{

 public:
  typedef sprockit::refcount_ptr<collective_work_message> ptr;

  typedef enum {
    get_data, //recver gets data
    put_data, //sender puts data
    rdma_get_header, //sender sends a header to recver to configure RDMA get
    rdma_put_header, //recver sends a header to sender to configure RDMA put
    eager_payload, //for small messages, no recv header - just send payload
    nack_get_ack,
    nack_put_payload,
    nack_eager,
    nack_get_header, //collective has failed, send fake message nack instead of real one
    nack_put_header //collective has failed, send fake message nack instead of real one
  } action_t;


 public:
  collective_work_message(
    collective::type_t type,
    action_t action,
    size_t nbytes,
    int tag, int round,
    int src, int dst) :
    message(nbytes),
    tag_(tag),
    type_(type),
    round_(round),
    dense_sender_(src),
    dense_recver_(dst),
    action_(action)
  {
    class_ = collective;
  }

  collective_work_message(
    collective::type_t type,
    action_t action,
    int tag, int round,
    int src, int dst) :
    message(),
    tag_(tag),
    type_(type),
    round_(round),
    dense_sender_(src),
    dense_recver_(dst),
    action_(action)
  {
    class_ = collective;
  }

  collective_work_message(){} //for serialization


  virtual std::string
  to_string() const;

  static const char*
  tostr(action_t action);

  virtual void
  serialize_order(sumi::serializer& ser);

  action_t
  action() const {
    return action_;
  }

  void
  set_action(action_t a) {
    action_ = a;
  }

  int
  tag() const {
    return tag_;
  }

  int
  round() const {
    return round_;
  }

  int
  dense_sender() const {
    return dense_sender_;
  }

  int
  dense_recver() const {
    return dense_recver_;
  }

  void
  reverse();

  collective::type_t
  type() const {
    return type_;
  }

  bool
  is_failure_notice() const {
    return !failed_procs_.empty();
  }

  void
  append_failed(int proc) {
    failed_procs_.insert(proc);
  }

  void
  append_failed(const thread_safe_set<int>& failed);

  const std::set<int>&
  failed_procs() const {
    return failed_procs_;
  }

  message*
  clone() const {
    collective_work_message* cln = new collective_work_message;
    clone_into(cln);
    return cln;
  }

 protected:
  void
  clone_into(collective_work_message* cln) const;

 protected:
  int tag_;

  collective::type_t type_;

  int round_;

  int dense_sender_;

  int dense_recver_;

  action_t action_;

  std::set<int> failed_procs_;

};

}


#endif // COLLECTIVE_MESSAGE_H
