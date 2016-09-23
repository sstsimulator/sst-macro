#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::hw::packet_stats_callback);

namespace sstmac {
namespace hw {

SpktRegister("bytes_sent", stat_collector, stat_bytes_sent);
SpktRegister("congestion_spyplot", packet_stats_callback, congestion_spyplot);
SpktRegister("congestion_delay", packet_stats_callback, packet_delay_stats);
SpktRegister("congestion", packet_stats_callback, spyplot_and_delay_stats);
SpktRegister("bytes_sent", packet_stats_callback, bytes_sent_collector);
SpktRegister("byte_hops", packet_stats_callback, byte_hop_collector);
SpktRegister("delay_histogram", packet_stats_callback, delay_histogram);
SpktRegister("null", packet_stats_callback, null_stats);

static inline double
congestion_delay_us(const pkt_arbitration_t& st)
{
  timestamp delta_t = st.tail_leaves - st.pkt->arrival();
  double min_delta_t = st.pkt->byte_length() / st.incoming_bw;
  double congestion_delay_us = std::max(0., 1e6*(delta_t.sec() - min_delta_t));
  return congestion_delay_us;
}

packet_stats_callback::packet_stats_callback(sprockit::sim_parameters *params, event_scheduler *parent)
{
  id_ = params->get_int_param("id");
}

void
packet_stats_callback::collect_final_event(packet_flow_payload *pkt)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting final events");
}

void
packet_stats_callback::collect_single_event(const pkt_arbitration_t &st)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting single events");
}

congestion_spyplot::congestion_spyplot(sprockit::sim_parameters* params, event_scheduler* parent)
  : packet_stats_callback(params, parent)
{
  congestion_spyplot_ = required_stats<stat_spyplot>(parent, params,
                                               "congestion_spyplot", "spyplot");
}

congestion_spyplot::~congestion_spyplot()
{
  if (congestion_spyplot_) delete congestion_spyplot_;
}

void
congestion_spyplot::collect_final_event(packet_flow_payload *pkt)
{
  spkt_throw_printf(sprockit::unimplemented_error, "congestion_spyplot");
  //long delay_ns = pkt->delay_us() * 1e3; //go to ns
  //congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), delay_ns);
}

void
congestion_spyplot::collect_single_event(const pkt_arbitration_t &st)
{
  double delay = congestion_delay_us(st);
  collect(delay, st.pkt);
}

void
congestion_spyplot::collect(double congestion_delay_us,
  packet_flow_payload* pkt)
{
  congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), congestion_delay_us);
}

delay_histogram::delay_histogram(sprockit::sim_parameters *params, event_scheduler *parent) :
  packet_stats_callback(params, parent)
{
  congestion_hist_ = required_stats<stat_histogram>(parent, params,
                                        "delay_histogram", "histogram");
}

delay_histogram::~delay_histogram()
{
  if (congestion_hist_) delete congestion_hist_;
}

void
delay_histogram::collect_single_event(const pkt_arbitration_t& st)
{
  double delay_us = congestion_delay_us(st);
  congestion_hist_->collect(delay_us);
}

void
delay_histogram::collect_final_event(packet_flow_payload* pkt)
{
  spkt_throw_printf(sprockit::unimplemented_error, "delay_histogram");
  //congestion_hist_->collect(pkt->delay_us()*1e-6); //convert to seconds
}

void
packet_delay_stats::collect(
  double congestion_delay_us,
  packet_flow_payload* pkt)
{
  spkt_throw_printf(sprockit::unimplemented_error, "packet_delay_stats");
  //pkt->add_delay_us(congestion_delay_us);
}

void
packet_delay_stats::collect_single_event(const pkt_arbitration_t& st)
{
  double delay = congestion_delay_us(st);
  collect(delay, st.pkt);
}

spyplot_and_delay_stats::spyplot_and_delay_stats(sprockit::sim_parameters *params,
                                                 event_scheduler *parent) :
  congestion_spyplot(params, parent),
  packet_delay_stats(params, parent),
  packet_stats_callback(params, parent)
{
}

void
spyplot_and_delay_stats::collect_single_event(const pkt_arbitration_t& st)
{
  double delay = congestion_delay_us(st);
  congestion_spyplot::collect(delay, st.pkt);
  packet_delay_stats::collect(delay, st.pkt);
}

bytes_sent_collector::~bytes_sent_collector()
{
  if (bytes_sent_) delete bytes_sent_;
}

bytes_sent_collector::bytes_sent_collector(sprockit::sim_parameters *params,
                                           event_scheduler *parent) :
  packet_stats_callback(params, parent)
{
  bytes_sent_ = required_stats<stat_bytes_sent>(parent, params,
                                        "bytes_sent", "bytes_sent");
}

void
bytes_sent_collector::collect_single_event(const pkt_arbitration_t& st)
{
  bytes_sent_->record(st.pkt->next_port(), st.pkt->byte_length());
}

byte_hop_collector::~byte_hop_collector()
{
  delete byte_hops_;
}

byte_hop_collector::byte_hop_collector(sprockit::sim_parameters *params, event_scheduler *parent)
  : packet_stats_callback(params, parent)
{
  byte_hops_ = required_stats<stat_global_int>(parent, params,
                                        "byte_hops", "global_int");
  byte_hops_->set_label("Byte Hops");
}

void
byte_hop_collector::collect_single_event(const pkt_arbitration_t& st)
{
  byte_hops_->collect(st.pkt->byte_length());
}

void
stat_bytes_sent::output_switch(int sid, std::fstream& data_str)
{
  port_map& pmap = global_aggregation_[sid];
  port_map::iterator it, end = pmap.end();
  long total = 0;
  for (it=pmap.begin(); it != end; ++it){
    int port = it->first;
    long bytes = it->second;
    total += bytes;
    //coordinates neighbor_coords = top->neighbor_at_port(switch_id(sid), port);
    data_str << sprockit::printf("\t%3d %12ld\n", port, bytes);
  }
}

void
stat_bytes_sent::dump_local_data()
{
  spkt_throw(sprockit::unimplemented_error,
    "stat_bytes_sent::dump_local_data: makes no sense to dump local data");
}

void
stat_bytes_sent::dump_global_data()
{
  int num_switches = top_->num_switches();
  std::string data_file = fileroot_ + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  for (int i=0; i < num_switches; ++i){
    data_str << sprockit::printf("Switch %s\n", top_->label(switch_id(i)).c_str());
    output_switch(i, data_str);
  }
  data_str.close();
}

void
stat_bytes_sent::global_reduce_non_root(parallel_runtime* rt, int root, char* buffer, int buffer_size)
{
  rt->send(root, buffer, buffer_size);
}

void
stat_bytes_sent::collect_buffer_at_root(char* buffer, int buffer_size)
{
  serializer ser;
  ser.start_unpacking(buffer, buffer_size);

  std::vector<aggregation::entry> entries;
  ser & entries;

  int num_entries = entries.size();
  for (int i=0; i < num_entries; ++i){
    aggregation::entry& entry = entries[i];
    fflush(stdout);
    global_aggregation_[entry.sid] = entry.pmap;
  }
}

void
stat_bytes_sent::collect_counts_at_root(parallel_runtime* rt, int src, global_gather_stats_t stats)
{
  char* recv_buffer = new char[stats.buffer_size];
  rt->recv(src, recv_buffer, stats.buffer_size);
  collect_buffer_at_root(recv_buffer, stats.buffer_size);
}

void
stat_bytes_sent::global_reduce_root(parallel_runtime* rt, global_gather_stats_t* stats,
                                    char* my_buffer, int my_buffer_size)
{
  //figure out how many total entries we have
  global_aggregation_.resize(top_->num_switches());

  for (int i=0; i < rt->nproc(); ++i){
    if (i != rt->me()){
      collect_counts_at_root(rt, i, stats[i]);
    } else {
      collect_buffer_at_root(my_buffer, my_buffer_size);
    }
  }
}

void
stat_bytes_sent::aggregation::entry::serialize_order(serializer& ser)
{
  ser & pmap;
  ser & sid;
}

stat_bytes_sent::stat_bytes_sent(sprockit::sim_parameters* params) :
    top_(nullptr),
    local_aggregation_(nullptr),
    stat_collector(params)
{
  top_ = sstmac::hw::topology::static_topology(params);
}

stat_bytes_sent::~stat_bytes_sent()
{
  if (local_aggregation_) delete local_aggregation_;
}

void
stat_bytes_sent::global_reduce(parallel_runtime *rt)
{
  //determine how big of an array we we will need to receive from each person
  int root = 0;

  serializer ser;
  ser.start_sizing();
  //this is basically a sanity check
  ser & local_aggregation_->entries_;
  int total_size = ser.size();

  int size_check = local_aggregation_->ser_size();
  if (total_size != size_check){
    spkt_throw_printf(sprockit::value_error,
        "stat_bytes_sent::global_reduce: serialization buffer size %d does not match expected %d - %d entries and %d counts",
         total_size, size_check, local_aggregation_->num_entries(), local_aggregation_->num_counts());
  }

  char* buffer = new char[total_size];
  ser.start_packing(buffer, total_size);
  ser & local_aggregation_->entries_;

  global_gather_stats_t stats;
  stats.buffer_size = total_size;
  global_gather_stats_t* stats_array = 0;

  if (rt->me() == root){
    stats_array = new global_gather_stats_t[rt->nproc()];
  }

  rt->gather(&stats, sizeof(global_gather_stats_t), stats_array, root);

  if (rt->me() == root){
    global_reduce_root(rt, stats_array, buffer, total_size);
    delete[] stats_array;
  } else {
    global_reduce_non_root(rt, root, buffer, total_size);
  }

  delete[] buffer;
}

void
stat_bytes_sent::reduce(stat_collector *coll)
{
  stat_bytes_sent* input = safe_cast(stat_bytes_sent, coll);
  if (local_aggregation_ == nullptr){
    local_aggregation_ = new aggregation;
  }
  local_aggregation_->append(input->id(), input->port_map_);
}

void
stat_bytes_sent::simulation_finished(timestamp end)
{
  //no op
}

}
}
