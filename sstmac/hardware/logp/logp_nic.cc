#include <sstmac/hardware/logp/logp_nic.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/node/node.h>
#include <sstmac/common/event_handler.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/hardware/interconnect/interconnect.h>

namespace sstmac {
namespace hw {

SpktRegister("logP | simple | LogP | logp", nic, logp_nic,
            "implements a nic that models messages via a simple latency/bandwidth injection delay");

logp_nic::logp_nic(sprockit::sim_parameters* params, node* parent) :
  next_free_(0),
  nic(params, parent)
{
  ack_handler_ = new_handler(parent, &node::handle);
  sprockit::sim_parameters* inj_params = params->get_namespace("injection");
  double inj_bw = inj_params->get_bandwidth_param("bandwidth");
  inj_bw_inverse_ = 1.0/inj_bw;
  inj_lat_ = inj_params->get_time_param("latency");
}

logp_nic::~logp_nic()
{
  if (ack_handler_) delete ack_handler_;
}

void
logp_nic::do_send(network_message* msg)
{
  long num_bytes = msg->byte_length();
  timestamp now_ = now();
  timestamp start_send = now_ > next_free_ ? now_ : next_free_;
  nic_debug("logp injection queued at %8.4e, sending at %8.4e for message %s",
            now_.sec(), start_send.sec(), msg->to_string().c_str());

  timestamp extra_delay = start_send - now_;
  //leave the injection latency term to the interconnect
  send_delayed_to_link(extra_delay, logp_switch_, msg);

  timestamp time_to_inject = inj_lat_ + timestamp(inj_bw_inverse_ * num_bytes);
  next_free_ = start_send + time_to_inject;
  if (msg->needs_ack()){
    //do whatever you need to do so that this msg decouples all pointers
    network_message* acker = msg->clone_injection_ack();
    schedule(next_free_, ack_handler_, acker); //send to node
  }
}

void
logp_nic::connect_output(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  if (src_outport == Injection){
    //ignore
  } else if (src_outport == LogP){
    nic_debug("connecting to LogP switch");
    logp_switch_ = mod;
  } else {
    spkt_abort_printf("Invalid switch port %d in logp_nic::connect_output", src_outport);
  }
}

void
logp_nic::connect_input(
  sprockit::sim_parameters* params,
  int src_outport,
  int dst_inport,
  event_handler* mod)
{
  //nothing needed
}

link_handler*
logp_nic::payload_handler(int port) const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return new SST::Event::Handler<nic>(const_cast<logp_nic*>(this), &nic::mtl_handle);
#else
  return mtl_handler();
#endif
}

}
}

