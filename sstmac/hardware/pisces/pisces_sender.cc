#include <sstmac/hardware/pisces/pisces_sender.h>
#include <sstmac/hardware/router/valiant_routing.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>

MakeDebugSlot(pisces_timeline)
ImplementFactory(sstmac::hw::pisces_sender);

namespace sstmac {
namespace hw {

pisces_payload*
payload_queue::front()
{
  if (queue.empty()){
    return NULL;
  }

  return queue.front();
}

void
payload_queue::push_back(pisces_payload *payload)
{
  queue.push_back(payload);
}

pisces_payload*
payload_queue::pop(int num_credits)
{
  auto it = queue.begin(), end = queue.end();
  for (; it != end; ++it){
    pisces_payload* pkt = *it;
    if (pkt->num_bytes() <= num_credits){
      queue.erase(it);
      return pkt;
    }
  }
  return nullptr;
}

pisces_sender::pisces_sender(
  sprockit::sim_parameters* params,
  event_scheduler* parent) :
  event_subcomponent(parent), //no self handlers
  stat_collector_(nullptr),
  update_vc_(true)
{
  send_lat_ = params->get_time_param("send_latency");
  credit_lat_ = params->get_time_param("credit_latency");
}

void
pisces_sender::configure_payload_port_latency(sprockit::sim_parameters* params)
{
  if (!params->has_param("send_latency")){
    params->add_param_override("send_latency", params->get_param("latency"));
  }
  if (!params->has_param("credit_latency")){
    params->add_param_override("credit_latency", "0ns");
  }
}

void
pisces_sender::configure_credit_port_latency(sprockit::sim_parameters* params)
{
  if (!params->has_param("send_latency")){
    params->add_param_override("send_latency", "0ns");
  }
  if (!params->has_param("credit_latency")){
    params->add_param_override("credit_latency", params->get_param("latency"));
  }
}

void
pisces_sender::send_credit(
  const pisces_input& src,
  pisces_payload* payload,
  timestamp credits_ready)
{
  int src_vc = payload->vc(); //we have not updated to the new virtual channel
  pisces_credit* credit = new pisces_credit(src.src_outport,
                                   src_vc, payload->num_bytes());
  //there is a certain minimum latency on credits
  timestamp min_credit_departure = credits_ready - send_lat_;
  //assume for now the packet flow sender is smart enough to pipeline credits efficiently
  timestamp now_ = now();
  timestamp credit_departure = min_credit_departure > now_ ? min_credit_departure : now_;
  pisces_debug(
      "On %s:%p on inport %d, crediting %s:%p port:%d vc:%d {%s} at [%9.5e] after latency %9.5e with %p",
      to_string().c_str(), this, payload->inport(),
      src.handler->to_string().c_str(), src.handler,
      src.src_outport, src_vc,
      payload->to_string().c_str(),
      credit_departure.sec(), credit_lat_.sec(),
      credit);
  //
  send_to_link(credit_departure,
               credit_lat_,
               src.handler, credit);
}

/**
void
pisces_sender::handle(event* ev)
{
  pisces_credit* credit = test_cast(pisces_credit, ev);
  if (credit){
    handle_credit(credit);
  } else {
    handle_payload(static_cast<pisces_payload*>(ev));
  }
}
*/

void
pisces_sender::send(
  pisces_bandwidth_arbitrator* arb,
  pisces_payload* pkt,
  const pisces_input& src,
  const pisces_output& dest)
{
  pkt_arbitration_t st;
  st.incoming_bw = pkt->bw();
  st.now = now();
  st.pkt = pkt;
  st.src_outport = src.src_outport;
  st.dst_inport = dest.dst_inport;

  if (arb) {
    arb->arbitrate(st);
  } else {
    st.head_leaves = st.tail_leaves = st.credit_leaves = now();
  }

  if (stat_collector_) stat_collector_->collect_single_event(st);

#if SSTMAC_SANITY_CHECK
  if (msg->bw() <= 0 && msg->bw() != pisces_payload::uninitialized_bw) {
    spkt_throw_printf(sprockit::value_error,
                     "On %s, got negative bandwidth for msg %s",
                     to_string().c_str(),
                     msg->to_string().c_str());
  }
#endif

  if (src.handler) {
    send_credit(src, pkt, st.credit_leaves);
  }

  pisces_debug(
    "On %s:%p, sending to port:%d vc:%d {%s} to handler %s:%p on "
    "inport %d at head_leaves=%9.5e tail_leaves=%9.5e",
    to_string().c_str(), this,
    pkt->next_port(), pkt->next_vc(),
    pkt->to_string().c_str(),
    dest.handler->to_string().c_str(), dest.handler,
    dest.dst_inport,
    st.head_leaves.sec(),
    st.tail_leaves.sec());

  // leaving me, on arrival at dest message will occupy a specific inport at dest
  pkt->set_inport(dest.dst_inport);
  //weird hack to update vc from routing
  if (update_vc_) pkt->update_vc();

  send_to_link(st.head_leaves, send_lat_, dest.handler, pkt);
}

std::string
pisces_sender::to_string() const
{
  return pisces_name() + topology::global()->label(event_location());
}

}
}

