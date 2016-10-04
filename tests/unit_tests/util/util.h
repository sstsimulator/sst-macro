#ifndef tests_unittest_util_UTIL_H
#define tests_unittest_util_UTIL_H

#include <sstmac/common/sstmac_config.h>
#if !SSTMAC_INTEGRATED_SST_CORE
// TODO get unit tests working with @integrated_core

#include <sstmac/hardware/switch/network_switch.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/hardware/router/routable.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/pisces/pisces.h>
#include <sprockit/test/test.h>
#include <sprockit/sim_parameters.h>
#include <sstmac/util.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/messages/sst_message_fwd.h>

template <class A>
class ContainerAppend<sstmac::hw::coordinates, A>
{
 public:
  static void
  append(sstmac::hw::coordinates& t, const A& a){
   t.push_back(a);
  }
};

template <>
class ClassOutput<sstmac::hw::coordinates>
{
 public:
  static void
  print(const sstmac::hw::coordinates &t, std::ostream &os){
    ClassOutput<std::vector<int> >::print(t, os);
  }
};

sstmac::node_id
naddr(long nid);

sstmac::hw::pisces_payload*
msg(long nid);

sstmac::hw::pisces_payload*
new_packet(sstmac::message* msg, int bytes, int byte_offset);

sstmac::hw::coordinates
get_vector(int a);

sstmac::hw::coordinates
get_vector(int a, int b);

sstmac::hw::coordinates
get_vector(int a, int b, int c);

sstmac::hw::coordinates
get_vector(int a, int b, int c, int d);

sstmac::hw::coordinates
get_vector(int a, int b, int c, int d, int e);

sstmac::hw::coordinates
get_vector(int a, int b, int c, int d, int e, int f);

sstmac::hw::coordinates
get_vector(int a, int b, int c, int d, int e, int f, int g);

void init_switches(sstmac::hw::interconnect::switch_map& switches,
                   sprockit::sim_parameters& params,
                   sstmac::hw::topology* top);


void _assert_dim_dir(UnitTest& unit, const char* descr, const char* file, int line,
                    sstmac::hw::network_switch* sw, const sstmac::hw::routable::path& path,
                    long outport_sw_id);

#define assert_dim_dir(unit, descr, ...) \
    _assert_dim_dir(unit, descr, __FILE__, __LINE__, __VA_ARGS__)

#endif // !SSTMAC_INTEGRATED_SST_CORE

#endif // UTIL_H

