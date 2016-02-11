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

#include <sstmac/hardware/circuit/circuit_switch.h>
#include <sstmac/hardware/circuit/circuit_message.h>
#include <sstmac/hardware/router/router.h>
#include <sprockit/util.h>
#include <sprockit/output.h>

namespace sstmac {
namespace hw {

SpktRegister("circuit", network_switch, circuit_switch);


circuit_switch::circuit_switch()
{
}

void
circuit_switch::init_factory_params(sprockit::sim_parameters *params)
{
  cycle_accurate_switch::init_factory_params(params);
  blocked_protocol_ = params->get_bool_param("blocked_protocol");
}

#if 0
injector*
circuit_switch::get_new_injector(sprockit::sim_parameters* params)
{
  ptr sel(this);
  return circuitpacketinjector::construct(params, sel);
}

void
circuit_switch::circuitpacketinjector::send(long nbytes, long byte_offset,
    const sst_message::ptr& msg)
{
  timestamp delay(0);
  int vc;
  if (circpar_->numVC_ > 1) {
    circuit_message::ptr circ = safe_cast(circuit_message, msg,
                                          "circuit_switch::circuitpacketinjector::send: incoming message");
    if (circ->get_circ_type() == circuit_message::TEARDOWN
        || circ->get_circ_type() == circuit_message::PATH_ACK) {
      vc = circpar_->numVC_ - 1;
    }
    else {
      vc = rand() % (circpar_->numVC_ - 1);
    }
  }
  else {
    vc = 1;
  }
  int inport;

  network_message::ptr netmsg = safe_cast(network_message, msg);

  if (circpar_->nodebuffers_.find(netmsg->fromaddr()) ==
      circpar_->nodebuffers_.end()) {
    SSTMAC_DEBUG << "circuitswitch(" << parent_->get_addr()
                 << ")::packetinjector - adding node inport "
                 << (circpar_->nodebufcnt_ - 1) << " for address "
                 << netmsg->fromaddr() << "\n";
    std::map<node_id, int>::iterator it, end = circpar_->nodebuffers_.end();
    for (it = circpar_->nodebuffers_.begin(); it != end; it++) {
      SSTMAC_DEBUG << "in map: " << it->first << "\n";
      SSTMAC_DEBUG << "equals: "
                   << (it->first == netmsg->fromaddr())
                   << "\n";
    }
    circpar_->nodebuffers_[netmsg->fromaddr()] = --circpar_->nodebufcnt_;
  }
  inport = circpar_->nodebuffers_[netmsg->fromaddr()];

  long bytes_sent = byte_offset;
  long bytes_total = netmsg->byte_length();
  bool tail_packet = false;
  if (MTU_ > 0) {
    SSTMAC_DEBUG << "packetinjector: injecting packets: "
                 << msg->to_string()
                 << ", bytes-to-send=" << nbytes
                 << ", total-msg-size=" << netmsg->byte_length()
                 << ", MTU=" << MTU_
                 << "\n";

    long bytes_left = std::min(nbytes, bytes_total - byte_offset);
    while (bytes_left > 0) {
      long bytes_to_send = std::min(MTU_, bytes_left);
      cycle_accurate_payload::ptr pack = cycle_accurate_payload::construct(
                                           msg, bytes_to_send, bytes_sent, vc);
      bytes_left -= bytes_to_send;
      bytes_sent += bytes_to_send;
      tail_packet = bytes_sent >= bytes_total;

      pack->inport_ = inport;
      parent_->schedule(parent_->now() + delay, parent_, pack);
      delay = delay + timestamp(bytes_to_send / n2r_);
    }
  }
  else {
    delay = timestamp(nbytes / n2r_);
    SSTMAC_DEBUG << "packetinjector: injecting whole message: "
                 << netmsg->to_string() << " with delay " << delay
                 << ", bytes-to-send="
                 << ", total-msg-size=" << netmsg->byte_length()
                 << ", bw=" << n2r_
                 <<  "\n";
    cycle_accurate_payload::ptr pack = cycle_accurate_payload::construct(netmsg,
                                       nbytes, bytes_sent, vc);
    bytes_sent += nbytes;
    tail_packet = bytes_sent >= bytes_total;
    pack->inport_ = inport;
    parent_->schedule(parent_->now() + delay, parent_, pack);
  }

  if (netmsg->get_needs_ack() && tail_packet) {
    network_message::ptr sendack = netmsg->clone_ack();
    int inj_port = parent_->topol()->node_to_injector_port(msg->fromaddr());
    parent_->schedule(parent_->now() + delay,
                      sendhandle_[inj_port], sendack);
  }

}
#endif

bool
circuit_switch::check_circuit(cycle_accurate_payload::ptr msg, int in,
                              int out, bool &kill)
{
  circuit_message::ptr circ = ptr_safe_cast(circuit_message, msg->orig());

  int realout = out;
  if (out < 0) {
    realout = -1 * (long(circ->toaddr()) + 1);
  }

  if (circ->get_circ_type() == circuit_message::SETUP) {
    if (circ->ttl_ < now()) {
      kill = true;
      return false;
    }
    if (circuitsin_.find(in) == circuitsin_.end()) {
      if (circuitsout_.find(realout) == circuitsout_.end()) {

        return true;
      }
      else {
        //SSTMAC_DEBUG << "circuitswitch[" << my_addr_
        //             << "]: circuit already setup on outport from "
        //             << circuitsout_[realout] << " to " << realout << "\n";

        return false;
      }
    }
    else {
      //SSTMAC_DEBUG << "circuitswitch[" << my_addr_
      //             << "]: circuit already setup on inport from " << in
      //             << " to " << circuitsin_[in] << "\n";

      return false;
    }
  }
  else if (circ->get_circ_type() == circuit_message::TEARDOWN) {
    if (circuitsin_.find(in) == circuitsin_.end()) {
      //throw sprockit::spkt_error(
      //    "circuitswitch::check_circuit -- trying to teardown a circuit that's not there in circuitsin");
      kill = true;
      return false;
    }

    if (circuitsout_.find(realout) == circuitsout_.end()) {
      // throw sprockit::spkt_error(
      //    "circuitswitch::check_circuit -- trying to teardown a circuit that's not there in circuitsout");
      kill = true;
      return false;
    }

    if (circuitsin_[in] != realout || circuitsout_[realout] != in) {
      cerrn << "in = " << in << "\n";
      cerrn << "out = " << realout << "\n";
      cerrn << "circuitsin[" << in << "] = " << circuitsin_[in]
                << "\n";
      cerrn << "circuitsout[" << realout << "] = "
                << circuitsout_[realout] << "\n";
      spkt_throw(sprockit::spkt_error,
        "circuitswitch::check_circuit -- TEARDOWN -- circuit port information doesn't line up");

    }

    return true;

  }
  else if (circ->get_circ_type() == circuit_message::BLOCKED) {
    if (circuitsin_.find(realout) == circuitsin_.end()) {
      spkt_throw(sprockit::spkt_error,
        "circuitswitch::check_circuit -- trying to RELEASE a circuit that's not there in circuitsin");
    }

    if (circuitsout_.find(in) == circuitsout_.end()) {
      spkt_throw(sprockit::spkt_error,
        "circuitswitch::check_circuit -- trying to RELEASE a circuit that's not there in circuitsout");
    }

    if (circuitsin_[realout] != in || circuitsout_[in] != realout) {
      cerrn << "in = " << in << "\n";
      cerrn << "out = " << realout << "\n";
      cerrn << "circuitsin[" << realout << "] = "
                << circuitsin_[realout] << "\n";
      cerrn << "circuitsout[" << in << "] = " << circuitsout_[in]
                << "\n";
      spkt_throw(sprockit::spkt_error,
        "circuitswitch::check_circuit -- BLOCKED -- circuit port information doesn't line up");
    }
    return true;
  }
  else if (circ->get_circ_type() == circuit_message::TURNING) {
    return true;
  }
  else if (circ->get_circ_type() == circuit_message::PATH_ACK) {
    return true;
  }
  else {
    throw sprockit::spkt_error(
      "circuitswitch::check_circuits() - unknown message type");
  }
}

void
circuit_switch::do_route(cycle_accurate_payload::ptr msg, int inport,
                         int &outport, bool &kill)
{

  circuit_message::ptr circ = ptr_safe_cast(circuit_message, msg->orig());

  if (!circ) {
    throw sprockit::value_error("circuit received a non-circuit message");
  }

  if (circ->get_circ_type() == circuit_message::SETUP
      || circ->get_circ_type() == circuit_message::PATH_ACK)

  {
    //SSTMAC_DEBUG << "routing circ type: " << circ->get_circ_type()
    //             << "\n";

    router_->route(circ);
    outport = circ->rinfo().port();
  }
  else if (circ->get_circ_type() == circuit_message::TEARDOWN) {

    if (circuitsin_.find(inport) == circuitsin_.end()) {
      //SSTMAC_DEBUG << "routing teardown by killing it \n";
      kill = true;
      outport = -1;
      return;
    }

    //SSTMAC_DEBUG
   //     << "routing teardown by grabbing the existing circuit from inport "
    //    << inport << "\n";
    outport = circuitsin_[inport];
    //SSTMAC_DEBUG << "raw: " << outport << "\n";
    if (outport < 0) {
      outport = -1;
    }

    //SSTMAC_DEBUG << "returning: " << outport << "\n";

  }
  else if (circ->get_circ_type() == circuit_message::BLOCKED) {
    //SSTMAC_DEBUG
    //    << "routing blocked message by grabbing the reverse circuit \n";
    if (circuitsout_.find(inport) == circuitsout_.end()) {
      spkt_throw_printf(sprockit::spkt_error,
                       "circuitswitch::do_route: trying to find a circuit that's not there on inport %d",
                       inport);
    }

    outport = circuitsout_[inport];
    if (outport < 0) {
      outport = -1;
    }
  }
  else if (circ->get_circ_type() == circuit_message::TURNING) {
    //SSTMAC_DEBUG << "routing turning by returning the inport \n";
    outport = inport;
  }
  else {
    spkt_throw(sprockit::spkt_error,
              "circuitswitch::do_route: unknown message type");
  }
}

void
circuit_switch::modify_circuits(cycle_accurate_payload::ptr msg, int in,
                                int out)
{
  circuit_message::ptr circ = ptr_safe_cast(circuit_message, msg->orig());

  int realout = out;
  if (out < 0) {
    realout = -1 * (long(circ->toaddr()) + 1);
  }

  if (circ->get_circ_type() == circuit_message::SETUP) {

    //SSTMAC_DEBUG << "circuitswitch[" << my_addr_
    //             << "]: setting up circuit from " << in << " to " << realout
    //             << "\n";
    circuitsin_[in] = realout;
    circuitsout_[realout] = in;

    circrouter_->path_is_good(circ->toaddr(), in, out);
  }
  else if (circ->get_circ_type() == circuit_message::TEARDOWN) {

    //SSTMAC_DEBUG << "circuitswitch[" << my_addr_
    //             << "]: tearing down circuit from " << in << " to "
    //             << circuitsin_[in] << " from a TEARDOWN message \n";
    circuitsin_.erase(in);
    circuitsout_.erase(realout);

    circrouter_->path_teardown(in, out);

  }
  else if (circ->get_circ_type() == circuit_message::BLOCKED) {

    //SSTMAC_DEBUG << "circuitswitch[" << my_addr_
    //             << "]: tearing down circuit from " << realout << " to "
    //             << circuitsin_[realout] << " from a BLOCKED message \n";
    circuitsin_.erase(realout);
    circuitsout_.erase(in);

    circrouter_->path_teardown(out, in);
  }
  else if (circ->get_circ_type() == circuit_message::TURNING) {
    circ->set_circ_type(circuit_message::BLOCKED);
    node_id addr = circ->toaddr();
    circ->set_toaddr(circ->fromaddr());
    circ->set_fromaddr(addr);
  }
  else if (circ->get_circ_type() == circuit_message::PATH_ACK) {

  }
  else {
    throw sprockit::spkt_error(
      "circuitswitch::modify_circuits() - unknown message type");
  }
}

int
circuit_switch::check_inport(int inport, bool &clock)
{
  int ret = 0;

  /* SSTMAC_DEBUG << "switch[" << my_addr_->to_string()
   << "]: examining inport " << inport << ", num_in_buff = "
   << inbuffcnt_[inport] << ", time = " << now() << ", inport_free = "
   << inports_free_[inport].sec() << "\n";*/

  if (inbuffcnt_[inport] > 0) {
    if (inports_free_[inport] <= now()) {

      int beginVC = (arb_vc_[inport] + 1) % numVC_;
      bool firstVC = true;
      for (int vc = beginVC; firstVC || vc != beginVC; vc = (vc + 1)
           % numVC_) {

        firstVC = false;
        int index = inport * numVC_ + vc;

        //SSTMAC_DEBUG << "switch[" << my_addr_
        //             << "]: examining vc " << vc << ", with index " << index
        //             << ", queue size = " << inbuffs_[index].size() << "\n";

        if (!inbuffs_[index].empty()) {
          cycle_accurate_payload::ptr msg = inbuffs_[index].front();
          // SSTMAC_DEBUG << "got message: " << msg->to_string()
          //       << "\n";

          circuit_message::ptr circ = ptr_safe_cast(circuit_message, msg->orig());

          int outport;
          bool kill = false;
          do_route(msg, inport, outport, kill);

          if (kill) {
            //SSTMAC_DEBUG << "killing message "
             //            << msg->to_string() << "\n";
            inbuffs_[inport * numVC_ + vc].pop();
            inbuffcnt_[inport]--;
            arb_vc_[inport] = vc;
            clock = true;

            if (inport >= 0) {
              //send credit ack back
              cycle_accurate_ack::ptr ack =
                new cycle_accurate_ack(
                  msg->byte_length(), vc);

              ack->inport_ = msg->prevport_;

              //SSTMAC_DEBUG << "ca-switch("
              //             << my_addr_
              //             << "): sending ack back to "
              //             << msg->prevswitch_->to_string() << "\n";

              schedule(now() + link_lat_, msg->prevswitch_, ack);
            }
            break;
          }

          if (msg->vc_ != vc) {
            throw sprockit::spkt_error(
              "cycle_accurate switch: vcs don't match for some reason");
          }

          /* SSTMAC_DEBUG << "packet going to outport" << outport
           << ", outports_free = "
           << outports_free_[outport].sec();*/

          /* if (outport >= 0)
           {
           SSTMAC_DEBUG << ", outbuff occupancy = "
           << outbuffs_[outport]->get_occupancy()
           << ", credits = " << credits_[outport * numVC_
           + vc] << "\n";
           }
           else SSTMAC_DEBUG << "\n";*/

          if (outports_free_[outport] <= now() && (outport < 0
              || (outBuffSize_
                  - outbuffs_[outport]->occupancy()
                  >= msg->byte_length() && credits_[outport
                      * numVC_ + vc] >= mtu))) {

            bool checked = check_circuit(msg, inport, outport,
                                         kill);

            if (checked) {
              //SSTMAC_DEBUG << "switch["
              //             << my_addr_ << "]: message "
              //             << msg->to_string()
              //             << " arbitration good from " << inport
              //             << " to " << outport << " \n";

              modify_circuits(msg, inport, outport);

              inbuffs_[inport * numVC_ + vc].pop();
              inbuffcnt_[inport]--;
              //now we're ok to send through the crossbar
              //  sendDelayed(msg, arbLatency_, outports[outport]);
              timestamp outbuffarrival =
                now() + switchLatency_
                + timestamp(
                  msg->byte_length()
                  / switchBandwidth_);
              int prevport = msg->prevport_;
              packet_switch* prevswitch = msg->prevswitch_;
              msg->prevswitch_ = this;
              msg->prevport_ = outport;

              // circ->stack_.push(self_);
              schedule(outbuffarrival, outbuffs_[outport], msg);

              if (using_orion_) {
                //power for leaving inport

                //power for entering crossbar
                double initial = power_xbar_->report();
                power_xbar_->record(1, inport, outport,
                                    msg->byte_length());
              }

              inports_free_[inport] = outbuffarrival;
              outports_free_[outport] = outbuffarrival;
              arb_vc_[inport] = vc;
              ret++;
              clock = true;

              if (outport >= 0) {
                int inbuffindex = outport * numVC_ + vc;
                credits_[inbuffindex]
                -= msg->byte_length();
              }

              if (inport >= 0) {
                //send credit ack back
                cycle_accurate_ack::ptr ack =
                  new cycle_accurate_ack(
                    msg->byte_length(), vc);

                ack->inport_ = prevport;

                //SSTMAC_DEBUG << "ca-switch("
                //             << my_addr_
                //             << "): sending ack back to "
                //             << prevswitch->to_string() << "\n";

                schedule(now() + link_lat_, prevswitch, ack);
              }

              break;

            }
            else if (kill) {
              //SSTMAC_DEBUG << "killing message "
              //             << msg->to_string() << "\n";
              inbuffs_[inport * numVC_ + vc].pop();
              inbuffcnt_[inport]--;
              arb_vc_[inport] = vc;
              clock = true;

              if (inport >= 0) {
                //send credit ack back
                cycle_accurate_ack::ptr ack =
                  new cycle_accurate_ack(
                    msg->byte_length(), vc);

                ack->inport_ = msg->prevport_;

                //SSTMAC_DEBUG << "ca-switch("
                //             << my_addr_
                //             << "): sending ack back to "
                //             << msg->prevswitch_->to_string()
                //             << "\n";

                schedule(now() + link_lat_, msg->prevswitch_,
                         ack);
              }
              break;
            }
            else if (blocked_protocol_) {

              if (circ->get_circ_type()
                  == circuit_message::SETUP) {
                //uh oh, gotta turn it around
                //SSTMAC_DEBUG << "circuitswitch("
                //             << my_addr_
                //             << "): turning this boat around \n";
                circ->set_circ_type(circuit_message::TURNING);
                clock = true;
              }
            }

          }
        }
      }
    }
  }

  return ret;
}

}
}

