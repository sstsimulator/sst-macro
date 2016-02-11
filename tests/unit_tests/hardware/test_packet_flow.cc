#include <sstmac/backends/native/event_map.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/hardware/packet_flow/packet_flow.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/software/process/task_id.h>
#include <sstmac/util.h>
#include <tests/unit_tests/util/util.h>

#include <sprockit/test/test.h>

using namespace sstmac;
using namespace sstmac::hw;
using namespace sstmac::sw;

class test_message :
    public network_message
{
    protected:
        int num_bytes_;

    public:
        test_message(int num_bytes) :
            network_message(naddr(0), naddr(1), task_id(), task_id(), num_bytes)
        {
        }

};


static long packet_size = 1000;
static int num_packets_in_parent = 8;
static double link_bw = 1e9;


void
test_arbitrator(UnitTest& unit)
{
  sprockit::sim_parameters params;

  parallel_runtime* rt = new native::serial_runtime;
  rt->init_factory_params(&params);

  native::event_map* ev_mgr = new native::event_map;
  ev_mgr->init_param1(rt);
  ev_mgr->init_factory_params(&params);

  packet_flow_bandwidth_arbitrator* arb
      = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);

  arb->set_bw(link_bw);


  sst_message::ptr parent = new test_message(num_packets_in_parent * packet_size);
  packet_flow_payload::ptr test_msg = new packet_flow_payload(parent, packet_size, 0);
  test_msg->set_bw(link_bw);

  packet_flow_payload::ptr test_msg0 = new packet_flow_payload(parent, 0*packet_size, 0);
  packet_flow_payload::ptr test_msg2 = new packet_flow_payload(parent, 2*packet_size, 0);
  packet_flow_payload::ptr test_msg4 = new packet_flow_payload(parent, 4*packet_size, 0);

  timestamp start_time, stop_time;
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //now send the same message
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(packet_size/link_bw));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //send a message way in the future
  arb->arbitrate(timestamp(1), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(1));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  double delta = fabs(test_msg->bw() - link_bw);

  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);

  arb->set_bw(link_bw);
  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);

  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(2.0*packet_size/link_bw));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);


  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);
  arb->set_bw(link_bw);

  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  //now use all the bw
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);


  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);
  arb->set_bw(link_bw);

  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  //now use all the bw
  test_msg->set_bw(link_bw);
  double delay = packet_size / link_bw;
  test_msg->set_arrival(delay); //come in half-way through message sending
  arb->arbitrate(timestamp(delay), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(1e-6));
  assertEqual(unit, "bandwidth", test_msg->bw(), (1./1.5)*link_bw);

  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);
  arb->set_bw(link_bw);

  //send a bunch of slow messages to create many epochs
  test_msg->set_bw(0.25*link_bw);
  test_msg2->set_bw(0.25*link_bw);
  test_msg4->set_bw(0.25*link_bw);
  test_msg->set_arrival(0);
  test_msg2->set_arrival(0);
  test_msg4->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.25*link_bw);
  arb->arbitrate(timestamp(0), test_msg2, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg2->bw(), 0.25*link_bw);
  arb->arbitrate(timestamp(0), test_msg4, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg4->bw(), 0.25*link_bw);

  //now send a long, fast message
  test_msg4->set_bw(link_bw);
  arb->arbitrate(timestamp(0), test_msg4, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  double time_to_send = ((1./0.25) + (2./0.5) + (1./0.75)) * (packet_size / link_bw);
  double bw = 4. * packet_size / time_to_send;
  assertEqual(unit, "bandwidth", test_msg4->bw(), bw);

  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);
  arb->set_bw(link_bw);
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  test_msg->set_bw(0.8*link_bw);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(packet_size/link_bw));
  //the message starts buffering - whole message can send at max speed because
  //of bytes ready in queue
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //start over
  arb = new packet_flow_cut_through_arbitrator;
  arb->init_factory_params(&params);
  arb->set_bw(link_bw);
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  test_msg->set_bw(0.25*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(timestamp(0), test_msg, start_time, stop_time);
  assertEqual(unit, "start time", start_time, timestamp(packet_size/link_bw));
  //the message starts buffering - but only a part of it can send at max speed
  //the rest sends at the limited bandwidth
  int first_chunk = ceil(packet_size/3.);
  int second_chunk = packet_size - first_chunk;
  time_to_send = packet_size/3./link_bw + second_chunk/(0.25*link_bw);
  bw = packet_size / time_to_send;
  assertEqual(unit, "bandwidth", test_msg->bw(), bw);

  //send a zero length message
  arb->arbitrate(timestamp(0), test_msg0, start_time, stop_time);

}

int
main(int argc, char** argv)
{
    //relax the cutoff
    TestEquals<double>::cutoff = 1e-10;

    //logger::set_user_param("<debug> packet_flow");

    UnitTest unit;
    test_arbitrator(unit);
    unit.validate();
    return 0;
}
