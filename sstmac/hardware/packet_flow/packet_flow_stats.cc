#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/stats/stat_global_int.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/util.h>

ImplementFactory(sstmac::hw::packet_sent_stats);

namespace sstmac {
namespace hw {

SpktRegister("bytes_sent", stat_collector, stat_bytes_sent);
SpktRegister("congestion_spyplot", packet_sent_stats, congestion_spyplot);
SpktRegister("congestion_delay", packet_sent_stats, packet_delay_stats);
SpktRegister("congestion", packet_sent_stats, spyplot_and_delay_stats);
SpktRegister("bytes_sent", packet_sent_stats, bytes_sent_collector);
SpktRegister("byte_hops", packet_sent_stats, byte_hop_collector);
SpktRegister("delay_histogram", packet_sent_stats, delay_histogram);
SpktRegister("null", packet_sent_stats, null_stats);

static inline double
congestion_delay_us(const packet_stats_st& st)
{
  double delta_t = st.tail_leaves.sec() - st.pkt->arrival();
  double min_delta_t = st.pkt->byte_length() / st.incoming_bw;
  double congestion_delay_us = std::max(0., 1e6*(delta_t - min_delta_t));
  return congestion_delay_us;
}

void
packet_sent_stats::init_factory_params(sprockit::sim_parameters* params)
{
  id_ = params->get_int_param("id");
}

void
packet_sent_stats::collect_final_event(packet_flow_payload *pkt)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting final events");
}

void
packet_sent_stats::collect_single_event(const packet_stats_st &st)
{
  spkt_throw(sprockit::value_error,
             "stats object does not support collecting single events");
}

void
congestion_spyplot::init_factory_params(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* congestion_params = params->get_namespace("congestion_spyplot");
  congestion_spyplot_ = test_cast(stat_spyplot,
        stat_collector_factory::get_optional_param("type", "spyplot_png", congestion_params));
  if (!congestion_spyplot_){
    spkt_throw_printf(sprockit::value_error,
      "packet flow congestion stats must be spyplot or spyplot_png, %s given",
      congestion_params->get_param("type").c_str());
  }
}

void
congestion_spyplot::collect_final_event(packet_flow_payload *pkt)
{
  long delay_ns = pkt->delay_us() * 1e3; //go to ns
  congestion_spyplot_->add(pkt->fromaddr(), pkt->toaddr(), delay_ns);
}

void
congestion_spyplot::collect_single_event(const packet_stats_st &st)
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

void
congestion_spyplot::set_event_manager(event_manager *ev_mgr)
{
  ev_mgr->register_stat(congestion_spyplot_);
}

void
delay_histogram::init_factory_params(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* hist_params = params->get_namespace("delay_histogram");
  congestion_hist_ = test_cast(stat_histogram,
        stat_collector_factory::get_optional_param("type", "histogram", hist_params));
  if (!congestion_hist_){
    spkt_throw_printf(sprockit::value_error,
      "congestion delay stats must be histogram, %s given",
      hist_params->get_param("type").c_str());
  }
}

void
delay_histogram::collect_single_event(const packet_stats_st& st)
{
  double delay_us = congestion_delay_us(st);
  congestion_hist_->collect(delay_us);
}

void
delay_histogram::collect_final_event(packet_flow_payload* pkt)
{
  congestion_hist_->collect(pkt->delay_us()*1e-6); //convert to seconds
}

void
delay_histogram::set_event_manager(event_manager *ev_mgr)
{
  ev_mgr->register_stat(congestion_hist_);
}

void
packet_delay_stats::collect(
  double congestion_delay_us,
  packet_flow_payload* pkt)
{
  pkt->add_delay_us(congestion_delay_us);
}

void
packet_delay_stats::collect_single_event(const packet_stats_st& st)
{
  double delay = congestion_delay_us(st);
  collect(delay, st.pkt);
}

void
spyplot_and_delay_stats::init_factory_params(sprockit::sim_parameters *params)
{
  congestion_spyplot::init_factory_params(params);
}

void
spyplot_and_delay_stats::collect_single_event(const packet_stats_st& st)
{
  double delay = congestion_delay_us(st);
  congestion_spyplot::collect(delay, st.pkt);
  packet_delay_stats::collect(delay, st.pkt);
}

void
bytes_sent_collector::init_factory_params(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* byte_params = params->get_namespace("bytes_sent");
  bytes_sent_ = test_cast(stat_bytes_sent,
                 stat_collector_factory::get_optional_param("type", "bytes_sent", byte_params));
  if (!bytes_sent_){
    spkt_throw_printf(sprockit::value_error,
      "packet flow bytes sent stats must be bytes_sent, %s given",
      byte_params->get_param("type").c_str());
  }
  bytes_sent_->set_topology(sstmac::runtime::current_topology());
}

void
bytes_sent_collector::collect_single_event(const packet_stats_st& st)
{
  bytes_sent_->record(st.pkt->next_port(), st.pkt->byte_length());
}

void
bytes_sent_collector::set_event_manager(event_manager *ev_mgr)
{
  ev_mgr->register_stat(bytes_sent_);
}

void
byte_hop_collector::init_factory_params(sprockit::sim_parameters* params)
{
  sprockit::sim_parameters* traffic_params = params->get_namespace("byte_hops");
  byte_hops_ = test_cast(stat_global_int,
     stat_collector_factory::get_optional_param("type", "global_int", traffic_params));
  byte_hops_->set_label("Byte Hops");
}

void
byte_hop_collector::collect_single_event(const packet_stats_st& st)
{
  byte_hops_->collect(st.pkt->byte_length());
}

void
byte_hop_collector::set_event_manager(event_manager *ev_mgr)
{
  ev_mgr->register_stat(byte_hops_);
}

void
stat_bytes_sent::output_switch(int sid, std::fstream& data_str, structured_topology* top)
{
  port_map& pmap = global_aggregation_[sid];
  port_map::iterator it, end = pmap.end();
  long total = 0;
  for (it=pmap.begin(); it != end; ++it){
    int port = it->first;
    long bytes = it->second;
    total += bytes;
    coordinates neighbor_coords = top->neighbor_at_port(switch_id(sid), port);
    data_str << sprockit::printf("\t%3d %12ld: %s\n",
      port, bytes, neighbor_coords.to_string().c_str());
  }
}

void
stat_bytes_sent::clone_into(stat_bytes_sent* cln) const
{
  cln->top_ = top_;
  stat_collector::clone_into(cln);
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
  structured_topology* regtop = safe_cast(structured_topology, top_,
    "bytes sent stats only compatible with structured topology");
  int num_switches = top_->num_switches();
  std::string data_file = fileroot_ + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  for (int i=0; i < num_switches; ++i){
    data_str << sprockit::printf("Switch %d: %15s\n", i,
        regtop->switch_coords(switch_id(i)).to_string().c_str());
    output_switch(i, data_str, regtop);
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

void
stat_bytes_sent::init_factory_params(sprockit::sim_parameters *params)
{
  stat_collector::init_factory_params(params);
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

}

void
stat_bytes_sent::reduce(stat_collector *coll)
{
  stat_bytes_sent* input = safe_cast(stat_bytes_sent, coll);
  if (local_aggregation_ == 0){
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
