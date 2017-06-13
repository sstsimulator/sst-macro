/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/backends/native/event_map.h>
#include <sstmac/backends/native/serial_runtime.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sstmac/hardware/pisces/pisces_arbitrator.h>
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
            network_message(app_id(0), naddr(0), naddr(1), num_bytes)
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
  params["bandwidth"] = 1e9;
  parallel_runtime* rt = new native::serial_runtime(&params);

  native::event_map* ev_mgr = new native::event_map(&params, rt);

  pisces_bandwidth_arbitrator* arb
      = new pisces_cut_through_arbitrator(&params);


  message* parent = new test_message(num_packets_in_parent * packet_size);
  pisces_payload* test_msg = new_packet(parent, packet_size, 0);
  test_msg->set_bw(link_bw);

  pisces_payload* test_msg0 = new_packet(parent, 0*packet_size, 0);
  pisces_payload* test_msg2 = new_packet(parent, 2*packet_size, 0);
  pisces_payload* test_msg4 = new_packet(parent, 4*packet_size, 0);

  pkt_arbitration_t st;
  st.now = timestamp(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //now send the same message
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(packet_size/link_bw));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //send a message way in the future
  st.now = timestamp(1);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(1));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  double delta = fabs(test_msg->bw() - link_bw);

  //start over
  arb = new pisces_cut_through_arbitrator(&params);

  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);

  st.now = timestamp(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(2.0*packet_size/link_bw));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);


  //start over
  arb = new pisces_cut_through_arbitrator(&params);
  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  //now use all the bw
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);


  //start over
  arb = new pisces_cut_through_arbitrator(&params);

  //only use half the bw
  test_msg->set_bw(0.5*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.5*link_bw);

  //now use all the bw
  test_msg->set_bw(link_bw);
  double delay = packet_size / link_bw;
  test_msg->set_arrival(delay); //come in half-way through message sending
  st.now = timestamp(delay);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(1e-6));
  assertEqual(unit, "bandwidth", test_msg->bw(), (1./1.5)*link_bw);

  //start over
  arb = new pisces_cut_through_arbitrator(&params);

  //send a bunch of slow messages to create many epochs
  test_msg->set_bw(0.25*link_bw);
  test_msg2->set_bw(0.25*link_bw);
  test_msg4->set_bw(0.25*link_bw);
  test_msg->set_arrival(0);
  test_msg2->set_arrival(0);
  test_msg4->set_arrival(0);

  st.now = timestamp(0);

  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), 0.25*link_bw);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg2->bw(), 0.25*link_bw);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg4->bw(), 0.25*link_bw);

  //now send a long, fast message
  test_msg4->set_bw(link_bw);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  double time_to_send = ((1./0.25) + (2./0.5) + (1./0.75)) * (packet_size / link_bw);
  double bw = 4. * packet_size / time_to_send;
  assertEqual(unit, "bandwidth", test_msg4->bw(), bw);

  //start over
  arb = new pisces_cut_through_arbitrator(&params);
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  test_msg->set_bw(0.8*link_bw);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(packet_size/link_bw));
  //the message starts buffering - whole message can send at max speed because
  //of bytes ready in queue
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  //start over
  arb = new pisces_cut_through_arbitrator(&params);
  test_msg->set_bw(link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(0));
  assertEqual(unit, "bandwidth", test_msg->bw(), link_bw);

  test_msg->set_bw(0.25*link_bw);
  test_msg->set_arrival(0);
  arb->arbitrate(st);
  assertEqual(unit, "start time", st.head_leaves, timestamp(packet_size/link_bw));
  //the message starts buffering - but only a part of it can send at max speed
  //the rest sends at the limited bandwidth
  int first_chunk = ceil(packet_size/3.);
  int second_chunk = packet_size - first_chunk;
  time_to_send = packet_size/3./link_bw + second_chunk/(0.25*link_bw);
  bw = packet_size / time_to_send;
  assertEqual(unit, "bandwidth", test_msg->bw(), bw);

  //send a zero length message
  arb->arbitrate(st);

}

int
main(int argc, char** argv)
{
    //relax the cutoff
    TestEquals<double>::cutoff = 1e-10;

    UnitTest unit;
    test_arbitrator(unit);
    unit.validate();
    return 0;
}