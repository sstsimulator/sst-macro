#include <sstmac/hardware/packet_flow/packet_flow_stats.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/serializer.h>
#include <sprockit/util.h>

namespace sstmac {
namespace hw {

SpktRegister("bytes_sent", stat_collector, stat_bytes_sent);

void
stat_bytes_sent::output_switch(int sid, std::fstream& data_str, structured_topology* top)
{
  port_map& pmap = global_aggregation_[sid];
  port_map::iterator it, end = pmap.end();
  for (it=pmap.begin(); it != end; ++it){
    int port = it->first;
    long bytes = it->second;
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
  sprockit::serializer ser;
  ser.start_unpacking(buffer, buffer_size);

  std::vector<aggregation::entry> entries;
  ser & entries;

  int num_entries = entries.size();
  for (int i=0; i < num_entries; ++i){
    aggregation::entry& entry = entries[i];
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
stat_bytes_sent::global_reduce_root(parallel_runtime* rt, global_gather_stats_t* stats, char* my_buffer, int my_buffer_size)
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
stat_bytes_sent::aggregation::entry::serialize_order(sprockit::serializer& ser)
{
  ser & pmap;
  ser & sid;
}

void
stat_bytes_sent::global_reduce(parallel_runtime *rt)
{
  //determine how big of an array we we will need to receive from each person
  int root = 0;

  sprockit::serializer ser;
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
