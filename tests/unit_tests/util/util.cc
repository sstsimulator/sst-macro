#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE
#include <tests/unit_tests/util/util.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/router/router.h>
#include <sstmac/hardware/pisces/pisces_switch.h>
#include <sprockit/util.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/backends/native/serial_runtime.h>

using namespace sstmac;
using namespace sstmac::hw;
using namespace sstmac::native;

coordinates
get_vector(int a){
    coordinates vec(1);
    vec[0] = a;
    return vec;
}

coordinates
get_vector(int a, int b){
    coordinates vec(2);
    vec[0] = a;
    vec[1] = b;
    return vec;
}

coordinates
get_vector(int a, int b, int c){
    coordinates vec(3);
    vec[0] = a;
    vec[1] = b;
    vec[2] = c;
    return vec;
}

coordinates
get_vector(int a, int b, int c, int d){
    coordinates vec(4);
    vec[0] = a;
    vec[1] = b;
    vec[2] = c;
    vec[3] = d;
    return vec;
}

coordinates
get_vector(int a, int b, int c, int d, int e){
    coordinates vec(5);
    vec[0] = a;
    vec[1] = b;
    vec[2] = c;
    vec[3] = d;
    vec[4] = e;
    return vec;
}

coordinates
get_vector(int a, int b, int c, int d, int e, int f){
    coordinates vec(6);
    vec[0] = a;
    vec[1] = b;
    vec[2] = c;
    vec[3] = d;
    vec[4] = e;
    vec[5] = f;
    return vec;
}

coordinates
get_vector(int a, int b, int c, int d, int e, int f, int g){
    coordinates vec(7);
    vec[0] = a;
    vec[1] = b;
    vec[2] = c;
    vec[3] = d;
    vec[4] = e;
    vec[5] = f;
    vec[6] = g;
    return vec;
}


sstmac::node_id
naddr(long nid)
{
  return sstmac::node_id(nid);
}

class routable_pisces :
 public pisces_payload,
 public routable
{
  NotSerializable(routable_pisces)

  public:
   routable_pisces(
     message* parent,
     int num_bytes,
     bool is_tail) :
    pisces_payload(parent, num_bytes, is_tail),
    routable(parent->toaddr(), parent->fromaddr())
  {
  }

  uint64_t
  flow_id() const override {
    return 0;
  }

  node_id
  toaddr() const override {
   return routable::toaddr();
  }

  node_id
  fromaddr() const override {
    return routable::fromaddr();
  }

  int
  next_port() const override {
    return routable::port();
  }

  int
  next_vc() const override {
    return routable::vc();
  }

};

pisces_payload*
msg(long nid)
{
  network_message* new_msg = new network_message;
  new_msg->set_toaddr(naddr(nid));
  new_msg->set_flow_id(hw::network_id(0,0));
  return new routable_pisces(new_msg, 0, 0);
}

pisces_payload*
new_packet(message *msg, int bytes, int byte_offset)
{
  return new routable_pisces(msg, bytes, byte_offset);
}

void
init_switches(interconnect::switch_map &switches,
              sprockit::sim_parameters& params,
              topology* top)
{
  serial_runtime rt(&params);
  null_event_manager mgr(&params, &rt);
  params["arbitrator"] = "cut_through";
  params["link.bandwidth"] = "1.0GB/s";
  params["link.credits"] = "64KB";
  params["xbar.bandwidth"] = "1.0GB/s";
  params["xbar.credit_latency"] = "100ns";
  params["xbar.send_latency"] = "0ns";
  params["link.credit_latency"] = "0ns";
  params["link.send_latency"] = "100ns";
  params["ejection.bandwidth"] = "1.0GB/s";
  params["ejection.send_latency"] = "1us";
  params["ejection.credit_latency"] = "0ns";
  params["model"] = "pisces";
  params["mtu"] = "8192";
  params["buffer_size"] = "16KB";
  int num_switches = top->num_switches();

  switches.resize(num_switches);

  // create all the switches
  for (int i=0; i < num_switches; i++)
  {
    params.add_param_override("id", i);
    network_switch* sw = network_switch_factory::get_param(
          "model", &params, i, &mgr);
    switches[switch_id(i)] = sw;
  }
  //top->connect_topology(&params, switches);
}




#endif // !SSTMAC_INTEGRATED_SST_CORE
