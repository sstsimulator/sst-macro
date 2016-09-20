#ifndef sstmac_sw_api_simpsg_ALLREDUCE_H
#define sstmac_sw_api_simpsg_ALLREDUCE_H

#include <sumi/collective.h>
#include <sumi/collective_actor.h>
#include <sumi/collective_message.h>
#include <sumi/comm_functions.h>

namespace sumi {

class wilke_allreduce_actor :
  public dag_collective_actor
{

 public:
  std::string
  to_string() const override {
    return "virtual all reduce actor";
  }

  void
  buffer_action(void *dst_buffer,
                void *msg_buffer, action* ac) override;

  wilke_allreduce_actor(reduce_fxn fxn) : fxn_(fxn) {}

 private:
  bool is_lower_partner(int virtual_me, int partner_gap);
  void finalize_buffers() override;
  void init_buffers(void *dst, void *src) override;
  void init_dag() override;

 private:
  reduce_fxn fxn_;

  int num_reducing_rounds_;

  int num_total_rounds_;

};

class wilke_halving_allreduce :
  public dag_collective
{
 public:
  std::string
  to_string() const override {
    return "sumi allreduce";
  }

  wilke_halving_allreduce(reduce_fxn fxn) : fxn_(fxn) {}

  wilke_halving_allreduce(){}

  virtual void
  init_reduce(reduce_fxn fxn) override {
    fxn_ = fxn;
  }

  dag_collective_actor*
  new_actor() const override {
    return new wilke_allreduce_actor(fxn_);
  }

  dag_collective*
  clone() const override {
    return new wilke_halving_allreduce(fxn_);
  }

 private:
  reduce_fxn fxn_;

};

}

#endif // ALLREDUCE_H
