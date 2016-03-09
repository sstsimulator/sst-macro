#include <sstmac/hardware/packet_flow/packet_flow_memory_model.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_message.h>
#include <sstmac/common/runtime.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("packet_flow", memory_model, packet_flow_memory_model);

packet_flow_memory_system::packet_flow_memory_system(int mtu, node* parent) :
  packet_flow_MTL(mtu),
  bw_noise_(0),
  interval_noise_(0),
  num_noisy_intervals_(0),
  parent_node_(parent)
{
  int num_channels = 4;
  pending_.resize(num_channels);
  for (int i=0; i < num_channels; ++i){
    channels_available_.push_back(i);
  }
}

void
packet_flow_memory_system::init_params(sprockit::sim_parameters *params)
{
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  latency_ = params->get_time_param("latency");
  arb_ = packet_flow_bandwidth_arbitrator_factory::get_optional_param(
        "arbitrator", "cut_through", params);
  endpoint_ = packet_flow_endpoint_factory::get_optional_param(
    "arbitrator", "cut_through", params);
}

void
packet_flow_memory_system::set_event_parent(event_scheduler *m)
{
  endpoint_->set_event_parent(m);
  packet_flow_sender::set_event_parent(m);
}

void
packet_flow_memory_system::finalize_init()
{
  //in and out ar the same
  arb_->set_outgoing_bw(max_bw_);
  init_noise_model();
  endpoint_->set_exit(parent_node_);
  endpoint_->init_param1(parent_node_->addr());
  init_loc_id(event_loc_id(parent_node_->addr()));
}

void
packet_flow_memory_system::init_noise_model()
{
  if (bw_noise_){
    arb_->partition(interval_noise_, num_noisy_intervals_);
    arb_->init_noise_model(bw_noise_);
  }
}

int
packet_flow_memory_system::num_initial_credits() const
{
  spkt_throw_printf(sprockit::value_error,
    "packet_flow_memory_model::num_initial_credits: should never be called");
}

void
packet_flow_memory_model::init_factory_params(sprockit::sim_parameters *params)
{
  memory_model::init_factory_params(params);
  int mtu = params->get_optional_int_param("mtu", 1<<30); //Defaults to huge value
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  mem_sys_ = new packet_flow_memory_system(mtu, parent_node_);
  mem_sys_->init_params(params);
  mem_sys_->finalize_init();
}

packet_flow_memory_model::~packet_flow_memory_model()
{
}

void
packet_flow_memory_model::finalize_init()
{
  memory_model::finalize_init();
}

void
packet_flow_memory_model::set_event_parent(event_scheduler* m)
{
  memory_model::set_event_parent(m);
  mem_sys_->set_event_parent(m);
}

void
packet_flow_memory_model::access(sst_message* msg)
{
  sw::compute_message* cmsg = safe_cast(sw::compute_message, msg);
  cmsg->set_access_id(parent_node_->allocate_unique_id());
  mem_sys_->mtl_send(msg);
}

int
packet_flow_memory_system::allocate_channel()
{
  if (channels_available_.empty()){
    int oldsize = pending_.size();
    //double size of pending
    int newsize = oldsize*2;
    pending_.resize(newsize);
    for (int i=oldsize; i != newsize; ++i){
      channels_available_.push_back(i);
    }
  }
  int channel = channels_available_.front();
  channels_available_.pop_front();
  return channel;
}

void
packet_flow_memory_system::mtl_send(sst_message*msg)
{
  sw::compute_message* cmsg = safe_cast(sw::compute_message, msg);
  packet_flow_payload* payload = next_chunk(0L, cmsg);
  if (cmsg->max_bw() != 0){
    payload->set_bw(cmsg->max_bw());
  }

  if (!payload->is_tail()){
    int channel = allocate_channel();
    payload->set_inport(channel);
    //not quite done - need to wait for credits to send the rest of this
    pending_msg& p = pending_[channel];
    p.byte_offset = payload->num_bytes();
    p.msg = cmsg;
    debug_printf(sprockit::dbg::packet_flow,
      "memory starting payload %s for compute message of size %d on channel %d at byte offset %d\n",
      payload->to_string().c_str(), cmsg->byte_length(), channel, p.byte_offset);
  }

  handle_payload(payload);
}
void
packet_flow_memory_system::do_handle_payload(packet_flow_payload* msg)
{
  //set the bandwidth to the max single bw
  msg->init_bw(max_single_bw_);
  timestamp packet_head_leaves;
  timestamp packet_tail_leaves;
  timestamp credit_leaves;
  arb_->arbitrate(now(), msg, packet_head_leaves, packet_tail_leaves, credit_leaves);
  timestamp finish = packet_head_leaves + latency_;

  send_to_endpoint(finish, msg);

  //might need to send some credits back
  if (!msg->is_tail()){
    int ignore_vc = -1;
    packet_flow_credit* credit = new packet_flow_credit(msg->inport(), ignore_vc, msg->num_bytes());
    //here we do not optimistically send credits = only when the packet leaves
    packet_flow_handler::send_self_message(packet_tail_leaves, credit);
  }
}

void
packet_flow_memory_system::send_to_endpoint(timestamp finish, packet_flow_payload* msg)
{
  SCHEDULE(finish, endpoint_, msg);
}

void
packet_flow_memory_system::set_input(int my_inport, int dst_outport, event_handler* input)
{

}

void
packet_flow_memory_system::set_output(int my_outport, int dst_inport, event_handler* output)
{
}

void
packet_flow_memory_system::init_credits(int port, int num_credits)
{
}

void
packet_flow_memory_system::handle_credit(packet_flow_credit* msg)
{
  int channel = msg->port();
  pending_msg& p = pending_[channel];

  //printf("Handling credit of size %d for channel %d\n",
  //    msg->num_credits(), channel);

  packet_flow_payload* payload = next_chunk(p.byte_offset, p.msg);
  if (p.msg->max_bw() != 0){
    payload->set_bw(p.msg->max_bw());
  }

  payload->set_inport(channel);
  p.byte_offset += payload->num_bytes();

  //printf("Handling payload of size %d for message of size %d on channel %d at byte offset %d\n",
  //  payload->num_bytes(), p.msg->byte_length(), channel, p.byte_offset);

  if (p.byte_offset == p.msg->byte_length()){
    //whole message has been sent
    //channel is available
    channels_available_.push_back(channel);
    //and clear the pending message
    p.byte_offset = 0;
    p.msg = 0;
  }


  handle_payload(payload);
}

}
}
