#include <sstmac/backends/common/sim_partition.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/serializable.h>
#include <sprockit/output.h>
#include <sprockit/fileio.h>
#include <fstream>

ImplementFactory(sstmac::parallel_runtime);
RegisterDebugSlot(parallel);

namespace sstmac {

const int parallel_runtime::global_root = -1;

partition*
parallel_runtime::topology_partition() const
{
  return part_;
}

void
parallel_runtime::finalize_init()
{
  if (me_ == 0){
    sprockit::output::init_out0(&std::cout);
    sprockit::output::init_err0(&std::cerr);
  }
  else {
    sprockit::output::init_out0(new std::ofstream("/dev/null"));
    sprockit::output::init_err0(new std::ofstream("/dev/null"));
  }
  sprockit::output::init_outn(&std::cout);
  sprockit::output::init_errn(&std::cerr);
}

void
parallel_runtime::bcast_string(std::string& str, int root)
{
  if (nproc_ == 1)
    return;

  if (root == me_){
    int size = str.size(); //+1 for null terminator
    int root = 0;
    bcast(&size, sizeof(int), root);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, root);
  } else {
    int size;
    bcast(&size, sizeof(int), root);
    str.resize(size);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, root);
  }
}

std::istream*
parallel_runtime::bcast_file_stream(const std::string &fname)
{

  if (me_ == 0){
    std::ifstream* fstr = new std::ifstream;
    sprockit::SpktFileIO::open_file(*fstr, fname);

    if (!fstr->is_open()) {
      spkt_throw_printf(sprockit::input_error,
       "could not find file %s in current folder or configuration include path",
       fname.c_str());
    }

    if (nproc_ == 1){
      return fstr; //nothing to do
    }

    std::stringstream sstr;
    std::string line;
    while (fstr->good()){
      std::getline(*fstr, line);
      sstr << line << "\n";
    }
    std::string all_text = sstr.str();
    bcast_string(all_text, 0);
    //go back to the beginning of the file
    fstr->clear();
    fstr->seekg(0, fstr->beg);
    return fstr;
  } else {
    std::string all_text;
    bcast_string(all_text, 0);
    //std::cout << all_text << std::endl;
    return new std::stringstream(all_text);
  }
}

void
parallel_runtime::init_factory_params(sprockit::sim_parameters* params)
{
}

void
parallel_runtime::init_partition_params(sprockit::sim_parameters *params)
{
#if SSTMAC_INTEGRATED_SST_CORE
  spkt_throw(sprockit::unimplemented_error,
    "parallel_runtime::init_partition_params: should not be used with integrated core");
#else
  std::string netname = params->get_param("interconnect");
  if (netname == "simple"){
    sprockit::sim_parameters* subspace = params->get_namespace("topology");
    std::string topology_name = subspace->get_param("name");
    if(topology_name != "simple") {
      subspace->add_param_override("actual_name", topology_name);
      subspace->add_param_override("name", "simple");
    }
    else if (!subspace->has_param("actual_name")) {
      spkt_throw(sprockit::input_error, "must give actual topology other than \"simple\"");
    }
  }
  //out with the old, in with the new
  if (part_) delete part_;
  part_ = partition_factory::get_optional_param("partition", SSTMAC_DEFAULT_PARTITION_STRING, params, this);
#endif
}

void
parallel_runtime::init_runtime_params(sprockit::sim_parameters *params)
{

  //turn the number of procs and my rank into keywords
  nthread_ = params->get_optional_int_param("sst_nthread", 1);

  buf_size_ = params->get_optional_int_param("serialization_buffer_size", 512);
  int num_bufs_window = params->get_optional_int_param("serialization_num_bufs_allocation", 100);
  send_buffer_pools_.resize(nthread_, message_buffer_cache(buf_size_, num_bufs_window));
  send_buffers_.resize(nthread_);
  recv_buffer_pool_.init(buf_size_, num_bufs_window);
}

parallel_runtime::parallel_runtime()
  : part_(nullptr)
{
}

parallel_runtime::~parallel_runtime()
{
  if (part_) delete part_;
}

void
parallel_runtime::send_event(int thread_id,
  timestamp t,
  topology_id dst,
  event_loc_id src,
  uint32_t seqnum,
  event* ev)
{
#if SSTMAC_INTEGRATED_SST_CORE
  spkt_throw_printf(sprockit::unimplemented_error,
      "parallel_runtime::send_event: should not be called on integrated core");
#else
  sprockit::serializer ser;
  void* buffer = send_buffer_pools_[thread_id].pop();
  ser.start_packing((char*)buffer, buf_size_);
  ser & dst;
  ser & src;
  ser & seqnum;
  ser & t;
  ser & ev;
  if (ser.packer().size() > buf_size_){
    spkt_throw_printf(sprockit::value_error,
        "parallel_runtime::send_message:: buffer overrun %d > %d: set param serialization_buffer_size larger",
        ser.packer().size(), buf_size_);
  }
  int lp = part_->lpid_for_switch(dst);
  send_buffers_[thread_id].push_back(buffer);
  lock();
  do_send_message(lp, buffer, ser.packer().size());
  unlock();
#endif
}

void
parallel_runtime::free_recv_buffers(const std::vector<void*>& buffers)
{
  int num_bufs = buffers.size();
  lock();
  for (int i=0; i < num_bufs; ++i){
    recv_buffer_pool_.push(buffers[i]);
  }
  unlock();
}

void
parallel_runtime::send_recv_messages(std::vector<void*>& recv_buffers)
{
  if (nproc_ == 1)
    return;

  if (recv_buffers.size()){
    spkt_throw(sprockit::illformed_error,
        "recv buffers should be empty in send/recv messages");
  }
  do_send_recv_messages(recv_buffers);

  //and all the send buffers are now done
  for (int t=0; t < nthread(); ++t){
    std::vector<void*>& bufs = send_buffers_[t];
    int num_bufs = bufs.size();
    for (int i=0; i < num_bufs; ++i){
      send_buffer_pools_[t].push(bufs[i]);
    }
    bufs.clear();
  }
}

}


