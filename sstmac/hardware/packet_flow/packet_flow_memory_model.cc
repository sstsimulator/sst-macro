#include <sstmac/hardware/packet_flow/packet_flow_memory_model.h>
#include <sstmac/hardware/packet_flow/packet_allocator.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/software/libraries/compute/compute_event.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_callback.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>

MakeDebugSlot(packet_flow_memory)

#define debug(...) debug_printf(sprockit::dbg::packet_flow_memory, __VA_ARGS__)

namespace sstmac {
namespace hw {

SpktRegister("packet_flow", memory_model, packet_flow_memory_model);

packet_flow_memory_packetizer::packet_flow_memory_packetizer() :
  arb_(nullptr),
  bw_noise_(nullptr),
  interval_noise_(nullptr),
  num_noisy_intervals_(0)
{
}

void
packet_flow_memory_packetizer::init_factory_params(sprockit::sim_parameters *params)
{
  if (!params->has_param("mtu"))
    params->add_param("mtu", "100GB");
  packet_flow_packetizer::init_factory_params(params);
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  max_bw_ = params->get_bandwidth_param("total_bandwidth");
  latency_ = params->get_time_param("latency");
  arb_ = packet_flow_bandwidth_arbitrator_factory::get_value("cut_through", params);
  pkt_allocator_ = packet_allocator_factory
      ::get_optional_param("packet_allocator", "structured_routable", params);
}

void
packet_flow_memory_packetizer::finalize_init()
{
  //in and out ar the same
  arb_->set_outgoing_bw(max_bw_);
  init_noise_model();
}

packet_flow_memory_packetizer::~packet_flow_memory_packetizer()
{
  if (arb_) delete arb_;
  if (pkt_allocator_) delete pkt_allocator_;
  if (bw_noise_) delete bw_noise_;
  if (interval_noise_) delete interval_noise_;
}

void
packet_flow_memory_model::init_factory_params(sprockit::sim_parameters *params)
{
  nchannels_ = 4;
  for (int i=0; i < nchannels_; ++i){
    channels_available_.push_back(i);
  }

  memory_model::init_factory_params(params);
  max_single_bw_ = params->get_bandwidth_param("max_single_bandwidth");
  mem_packetizer_ = new packet_flow_memory_packetizer;
  mem_packetizer_->init_factory_params(params);
  mem_packetizer_->finalize_init();

  mem_packetizer_->setNotify(this);
}

packet_flow_memory_model::~packet_flow_memory_model()
{
  if (mem_packetizer_) delete mem_packetizer_;
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
  mem_packetizer_->set_event_parent(m);
}

void
packet_flow_memory_model::access(
  long bytes, double max_bw,
  callback* cb)
{
  memory_message* msg = new memory_message(bytes,
                   parent_node_->allocate_unique_id(), max_bw);
  pending_requests_[msg] = cb;
  int channel = allocate_channel();
  debug("starting access %lu on vn %d", msg->unique_id(), channel);
  mem_packetizer_->start(channel, msg);
}

void
packet_flow_memory_model::notify(int vn, message* msg)
{
  debug("finished access %lu on vn %d", msg->unique_id(), vn);

  callback* cb = pending_requests_[msg];
  pending_requests_.erase(msg);
  channels_available_.push_front(vn);
  //happening now
  delete msg;
  parent_->schedule_now(cb);
}

int
packet_flow_memory_model::allocate_channel()
{
  if (channels_available_.empty()){
    //double size of pending
    int newsize = nchannels_*2;
    for (int i=nchannels_; i != newsize; ++i){
      channels_available_.push_back(i);
    }
    nchannels_ = newsize;
  }
  int channel = channels_available_.front();
  channels_available_.pop_front();
  return channel;
}

void
packet_flow_memory_packetizer::init_noise_model()
{
  if (bw_noise_){
    arb_->partition(interval_noise_, num_noisy_intervals_);
    arb_->init_noise_model(bw_noise_);
  }
}

void
packet_flow_memory_packetizer::inject(int vn, long bytes, long byte_offset, message* msg)
{
  packet_flow_payload* payload = pkt_allocator_->new_packet(bytes, byte_offset, msg);

  payload->set_inport(vn);
  memory_message* orig = safe_cast(memory_message, msg);
  if (orig->max_bw() != 0){
    payload->set_bw(orig->max_bw());
  }

  debug("injecting %s on vn %d", payload->to_string().c_str(), vn);

  handle_payload(vn, payload);
}

void
packet_flow_memory_packetizer::handle_payload(int vn, packet_flow_payload* pkt)
{
  //set the bandwidth to the max single bw
  pkt->init_bw(max_single_bw_);
  pkt->set_arrival(now().sec());
  packet_stats_st st;
  st.pkt = pkt;
  st.now = now();
  arb_->arbitrate(st);

  debug("memory packet %s leaving on vn %d at t=%8.4e",
    pkt->to_string().c_str(), vn, st.tail_leaves.sec());

  send_self_event_queue(st.tail_leaves,
    new_callback(this, &packetizer::packetArrived, vn, pkt));

  //might need to send some credits back
  if (!pkt->is_tail()){
    int ignore_vc = -1;
    packet_flow_credit* credit = new packet_flow_credit(vn, ignore_vc, pkt->num_bytes());
    //here we do not optimistically send credits = only when the packet leaves
    send_self_event(st.tail_leaves, credit);
  }
}

void
packet_flow_memory_packetizer::recv_credit(packet_flow_credit* msg)
{
  debug("got credit %s on vn %d", msg->to_string().c_str(), msg->port());

  int channel = msg->port();
  sendWhatYouCan(channel);
}

}
}
