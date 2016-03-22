/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/hardware/circuit/circuit_nic.h>
#include <sstmac/software/ami/ami.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/event_callback.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>

#define circuit_nic_debug(...) \
  nic_debug("circuit model: %s", sprockit::printf(__VA_ARGS__).c_str())

namespace sstmac {
namespace hw {

SpktRegister("circuitnic | circuit", nic, circuit_nic,
            "NIC model compatible with circuit switching");

circuit_nic::map_type circuit_nic::node_map_;

long circuit_nic::got_datas_ = 0;
long circuit_nic::got_teardowns_ = 0;
long circuit_nic::got_path_acks_ = 0;
long circuit_nic::got_setups_ = 0;
long circuit_nic::numsends_ = 0;
bool circuit_nic::printed_ = false;
long circuit_nic::timeouts_ = 0;

//
// Hi.
//
circuit_nic::circuit_nic() :
  nic()
{
  busy_ = false;
  firstsendset_ = true;
}

void
circuit_nic::init_factory_params(sprockit::sim_parameters* params)
{
  bw_ = params->get_bandwidth_param("circuitnic_bandwidth");
  propdelay_ = params->get_time_param("circuitnic_propdelay");
  setup_timeout_ = params->get_optional_time_param("circuitnic_setup_timeout", 0);
}

void
circuit_nic::finalize_init()
{
  node_id pid = parent_->id();
  if (pid != node_id()) {
    node_map_[pid] = this;
  }
  nic::finalize_init();
}

void
circuit_nic::do_send(network_message* payload)
{
  job j;
  j.payload = payload;
  j.recver = payload->toaddr();
  j.arrived = now();
  jobs_.push(j);
  check_jobs();
  numsends_++;
}

void
circuit_nic::check_jobs()
{
  if (!busy_ && jobs_.size() > 0) {
    circuit_nic_debug("starting job");
    spkt_throw(sprockit::unimplemented_error, "circuit_nic::check_jobs");
    //send_to_network_link();
    //sst_message* msg = new_message_callback(this,
    //                       &circuit_nic::send_to_network_link, jobs_.front().payload,
    //                       jobs_.front().recver, jobs_.front().arrived);
    //parent_node()->os()->execute_kernel(sstmac::sw::ami::AMI_COMP_MEM, msg);
    jobs_.pop();
    busy_ = true;
  }
}

//
// Schedule a send operation to happen at the given time.
//
void
circuit_nic::send_to_network_link(
  message* payload,
  node_id recver,
  const timestamp &arrived)
{
  circuit_nic_debug("send to %d payload %s",
    int(recver), payload->to_string().c_str());

  if (payload == 0) {
    spkt_throw(sprockit::null_error,
              "circuitnic::send: null payload pointer");
  }

  timestamp delayed = now(); //don't model delay for now

  circuit_message::ptr circ = new circuit_message(payload, 64, 0,
                              circuit_message::SETUP);
  circ->set_fromaddr(payload->fromaddr());
  circ->set_toaddr(payload->toaddr());
  circ->arrive_ = arrived;

  if (!firstsendset_) {
  }
  else {
    firstsendset_ = false;
  }
  current_ = circ;

  injector_->handle(circ);

  if (setup_timeout_ > timestamp(0)) {
    circ->ttl_ = delayed + setup_timeout_;
    event_handler* ev =
      new_callback(this, &circuit_nic::timeout, circ);
    schedule(delayed + setup_timeout_, ev, circ);
  }
  else {
    circ->ttl_ = timestamp(timestamp::max_time() - 10);
  }

  if (spy_num_messages_){
    spy_num_messages_->add_one(payload->fromaddr(),
        payload->toaddr());
  }
  if (spy_bytes_){
    spy_bytes_->add(payload->fromaddr(),
                  payload->toaddr(),
                  payload->byte_length());
  }
}

void
circuit_nic::handle(message* msg)
{
  nic::handle(msg);
}

void
circuit_nic::timeout(message* msg)
{
  circuit_message::ptr circ = ptr_safe_cast(circuit_message, msg);
  if (circ == current_) { // we're still working on this, redo it
    circuit_nic_debug("timed out, restarting setup");

    timeouts_++;

    circuit_message::ptr teardown = new circuit_message(
                                      circ->orig(), 64, 0, circuit_message::TEARDOWN);

    teardown->set_toaddr(current_->toaddr());
    teardown->set_fromaddr(current_->fromaddr());

    timestamp teartime = now();
    injector_->handle(teardown);

    circuit_message::ptr circ2 = new circuit_message(circ->orig(),
                                 64, 0, circuit_message::SETUP);
    circ2->set_fromaddr(current_->fromaddr());
    circ2->set_toaddr(current_->toaddr());

    current_ = circ2;
    circ2->backoff_ = circ->backoff_ + 1;

    timestamp at = now() + (rand() % 10 == 0 ? timestamp(1e-9)
                            : timestamp(circ2->backoff_ * 50e-9 + (rand() % 5) * 10e-9));

    circ2->ttl_ = at + setup_timeout_;

    event_handler* outev = new_callback(this,
                                            &circuit_nic::send_out_resetup, circ2);
    schedule(at, outev, circ2);

    if (setup_timeout_ > timestamp(0)) {
      event_handler* ev = new_callback(this, &circuit_nic::timeout,
                                           circ2);
      schedule(at + setup_timeout_, ev, circ2);
    }
    else {
      circ2->ttl_ = timestamp(timestamp::max_time() - 10);
    }
  }
}

void
circuit_nic::send_out_resetup(message* msg)
{
  if (current_) {
    injector_->handle(msg);
    //interconn_->send(now(), msg);
  }
}

timestamp
circuit_nic::injection_latency() const
{
  spkt_throw(sprockit::unimplemented_error,
    "circuit_nic::injection_latency");
}


void
circuit_nic::recv_chunk(packet* chunk)
{
  circuit_message::ptr circ = ptr_safe_cast(circuit_message, chunk,
                                        "circuit_nic::do_recv: incoming message");

  if (circ->toaddr() != parent_->id()) {
    spkt_throw(sprockit::spkt_error, "message arrived at wrong nic");
  }

  if (circ->get_circ_type() == circuit_message::BLOCKED) {
    circuit_nic_debug("got BLOCKED while heading for %d", int(circ->fromaddr()));
    circ->set_circ_type(circuit_message::SETUP);
    circ->backoff_++;
    node_id addr = circ->toaddr();
    circ->set_toaddr(circ->fromaddr());
    circ->set_fromaddr(addr);

    timestamp extra_delay = rand() % 10 == 0
        ? timestamp(0)
        : timestamp(circ->backoff_ * 100e-9);

    timestamp delayed = now() + extra_delay;

    //circuit_nic_debug("backing off for %s", extra_delay);


    schedule(delayed, injector_, circ);
    //interconn_->send(delayed, circ);

  }
  else if (circ->get_circ_type() == circuit_message::PATH_ACK) {
    circuit_nic_debug("got PATH_ACK with msg %s for parent %s",
        circ->to_string().c_str(), circ->orig()->to_string().c_str());

    got_path_acks_++;
    network_message* parent_msg = ptr_safe_cast(network_message, circ->orig());
    circuit_message::ptr circout = new circuit_message(parent_msg,
                                     parent_msg->byte_length(), 0, circuit_message::DATA);

    circout->set_fromaddr(circ->toaddr());
    circout->set_toaddr(circ->fromaddr());

    if (!current_) {
      spkt_throw(sprockit::spkt_error,
        "circuitnic: got a path_ack when I'm not waiting for one");
    }

    if (current_->backoff_ != circ->backoff_) {
      circuit_nic_debug("returning");
      return;
    }

    circuit_nic_debug("getting dstnic at %d", int(circ->orig()->toaddr()));

    circuit_nic* dstnic = node_map_[circ->orig()->toaddr()];

    if (!dstnic) {
      spkt_throw(sprockit::spkt_error,
                "circuitnic: can't find dstnic at address ",
                circ->orig()->toaddr());
    }
    timestamp at = now() + propdelay_ + timestamp(
                     (double) circ->orig()->byte_length() / bw_);
    schedule(at, dstnic, circout);

    //schedule a message so we check the jobs again
    event_handler* ev = new_callback(this, &circuit_nic::newmsg,
                                         message*());
    schedule(at + timestamp(10e-9), ev, message*());

    if (parent_msg->needs_ack()) {
      circuit_nic_debug("sending back hardware ack");
      network_message* sendack = parent_msg->clone_injection_ack();
      schedule(now(), parent_, sendack);
    }

    current_ = circuit_message::ptr();
    circuit_message::ptr teardown = new circuit_message(
                                      circ->orig(), 64, 0, circuit_message::TEARDOWN);

    teardown->set_toaddr(circout->toaddr());
    teardown->set_fromaddr(circout->fromaddr());

    timestamp teartime = at - propdelay_;
    schedule(teartime, injector_, teardown);
    //interconn_->send(teartime, teardown);
  }
  else if (circ->get_circ_type() == circuit_message::DATA) {
    circuit_nic_debug("got DATA");
    got_datas_++;

    //allback_message::ptr callback_msg;
    //if (circ->is_tail()) {
    //  callback_msg = new_message_callback(parent_, &node::handle, circ->orig());
    //}

    spkt_throw(sprockit::unimplemented_error,
                "callback message not correct here");

    //parent_node()->os()->execute_kernel(
    //  sstmac::sw::ami::AMI_COMP_MEM, callback_msg);
  }
  else if (circ->get_circ_type() == circuit_message::SETUP) {
    nic_debug("got SETUP");
    got_setups_++;
    circuit_message::ptr pack = new circuit_message(circ->orig(),
                                64, 0, circuit_message::PATH_ACK);
    node_id addr = circ->fromaddr();
    pack->set_fromaddr(circ->toaddr());
    pack->set_toaddr(addr);

    pack->backoff_ = circ->backoff_;

    injector_->handle(pack);
    //interconn_->send(now(), pack);
  }
  else if (circ->get_circ_type() == circuit_message::TEARDOWN) {
    nic_debug("got TEARDOWN");
    got_teardowns_++;
  }
  else {
    spkt_throw_printf(sprockit::spkt_error,
      "circuitnic::recv: got a message of type %d I don't know what to do with",
       circ->get_circ_type());
  }
}

}
} // end of namespace sstmac.

