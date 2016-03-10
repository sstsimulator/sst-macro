#ifndef BOXML_H_INCLUDED
#define BOXML_H_INCLUDED

#include <sstmacro.h>
#include <sst/sumi_api.h>
#include <sumi/transport.h>
#include <sstmac/software/process/key.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/sim_thread_lock.h>
#include <sstmac/common/stats/stat_histogram_fwd.h>
#include <sstmac/common/stats/stat_local_double_fwd.h>
#include <tinyxml2.h>
#include <containers.h>

#include <algorithm>
#include <ctime>

#include <sys/time.h>
#include <list>
#include <deque>

#include <sprockit/unordered.h>



namespace lblxml
{

  // typedefs
  typedef int_container_t set_t;
  typedef int_container_iter set_iter;
  typedef std::multiset<int> multiset_t;
  typedef lblxml::event event_t;
  typedef lblxml::comm comm_t;
  typedef lblxml::comp comp_t;
  typedef lblxml::reduce reduce_t;
  typedef lblxml::box box_t;
  typedef std::vector<box*> box_map_t;
  typedef std::vector<box*> box_map_iter;
  typedef std::vector<event*> event_map_t;
  //typedef std::vector<event*>::iterator event_map_iter;
  typedef std::vector<int> index_to_rank_t;
  typedef std::vector<set_t> rank_to_set_t;
  typedef std::vector<std::list<int> > rank_to_list_t;
  typedef std::vector<multiset_t> rank_to_multiset_t;
  typedef std::vector<set_t>::iterator rank_to_set_iter;
  typedef std::deque<int> da_list_t;
  typedef std::vector<da_list_t> rank_to_da_list_t;

  class pt2pt_message : public sumi::rdma_message
  {
  private:
    int event_index_;
  public:
    typedef sprockit::refcount_ptr<pt2pt_message> ptr;

    pt2pt_message(int index, long num_bytes) : event_index_(index),
      sumi::rdma_message(num_bytes)
    { }

    ~pt2pt_message() { }

    int event_index() { return event_index_; }
  };

  class box_domain : public sumi::domain
  {
  private:
    const int* boxes_;
    const int* map_;
    int size_;

  public:

    box_domain() { }

    box_domain (int domain_rank, int nboxes, const int* boxes, const int* map) :
      map_(map), boxes_(boxes), size_(nboxes)
    {
      my_domain_rank_ = domain_rank;
    }

    ~box_domain() { }

    int
    nproc() const { return size_; }

    int
    my_box_number() const {
      return boxes_[my_domain_rank_];
    }

    int
    domain_to_global_rank(int domain_rank) const {
      int box = boxes_[domain_rank];
      int grank =  map_[box];
      return grank;
    }

    int
    global_to_domain_rank(int global_rank) const {
      std::cerr << "global_to_domain_rank() aborting\n";
      abort();
    }

  };

  typedef std::pair<int,int> index_box_pair_t;
  typedef std::map<index_box_pair_t,box_domain*> domain_map_t;
  typedef std::queue<index_box_pair_t> allreduce_queue_t;

  //typedef std::list<int> list_t;
  //typedef std::list<int>::iterator list_iter;
  //typedef std::unordered_map<int,box_t> box_map_t;
  //typedef std::unordered_map<int,box_t>::iterator box_map_iter;
  //typedef std::unordered_map<int,comm_t> comm_map_t;
  //typedef std::unordered_map<int,comm_t>::iterator comm_map_iter;
  //typedef std::unordered_map<int,comp_t> comp_map_t;
  //typedef std::unordered_map<int,comp_t>::iterator comp_map_iter;
  //typedef std::unordered_map<int,int> index_to_rank_t;
  //typedef std::unordered_map<int,int>::iterator index_to_rank_iter;
  //typedef std::unordered_map<int,MPI_Request*> recv_req_map_t;
  //typedef std::unordered_map<int,MPI_Request*>::iterator recv_req_map_iter;
  //typedef std::unordered_map<int,list_t> rank_to_list_t;
  //typedef std::unordered_map<int,list_t>::iterator rank_to_list_iter;
  //typedef std::unordered_map<int,da_list_t> rank_to_da_list_t;
  //typedef std::unordered_map<int,da_list_t>::iterator rank_to_da_list_iter;
  //typedef std::unordered_map<int,MPI_Comm> mpi_comm_map_t;
  //typedef std::unordered_map<int,comm_map_t> comm_maps_t;
  //typedef std::unordered_map<int,comp_map_t> comp_maps_t;
  //typedef std::unordered_map<int,comm_map_t> coll_maps_t;

  //typedef std::map<int,MPI_Request*> recv_req_map_t;
  //typedef std::map<int,MPI_Request*>::iterator recv_req_map_iter;
  //typedef std::list<MPI_Request*> recv_req_list_t;
  //typedef std::list<MPI_Request*>::iterator recv_req_list_iter;
  //typedef std::set<MPI_Request*> recv_req_list_t;
  //typedef std::set<MPI_Request*>::iterator recv_req_list_iter;

  // globals (that we really do want to be global)
  extern index_to_rank_t g_boxindex_to_rank; 
  extern box_map_t g_boxes;
  extern event_map_t g_events;
  extern rank_to_set_t g_rank_to_comps;
  extern rank_to_set_t g_rank_to_recvs;
  extern rank_to_set_t g_rank_to_sends;
  extern rank_to_list_t g_rank_to_allreduces;
  extern rank_to_da_list_t g_rank_to_valid_sends;
  extern rank_to_da_list_t g_rank_to_valid_comps;
  extern std::map<int, std::vector<bool> > g_reduce_to_box_running;
  extern double g_total_idle_time;
  extern int g_active_ranks;
  //extern rank_to_da_list_t g_rank_to_valid_allreduces;

  //extern comm_map_t g_comm_map;
  //extern comm_map_t g_coll_map;
  //extern comp_map_t g_comp_map;
  //extern comm_maps_t g_send_maps;
  //extern comm_maps_t g_recv_maps;
  //extern comm_maps_t g_coll_maps;
  //extern comp_maps_t g_comp_maps;

  extern std::map<int,sstmac::timestamp> g_message_begin_;

  class boxml : virtual public sstmac::sw::mpi_app
  {
  public:
    static sstmac::sim_thread_lock* event_lock;

  private:

    int debug_, rank_, commsize_, message_factor_;
    double compute_scale_;
    std::string boxfile_, assignment_;
    std::vector< std::string> eventfiles_;
    bool do_compute_, randomize_events_, detailed_progress_, round_robin_, minimize_locks_;
    static bool have_data_;
    int barrier_tag_;
    box_map_t my_boxes_;
    domain_map_t  box_domains_;
    /** stores allreduce by box number */
    allreduce_queue_t valid_allreduces_;
    int nevents_;
    static std::fstream bin_file_;
    static bool have_input_bin_;
    static bool have_output_bin_;
    static bool checked_bin_;
    bool xml_read_only_;
    sumi::transport* tport_;
    sstmac::stat_histogram* hist_eff_bw_;
    sstmac::stat_local_double* idle_time_;

    void
    init();

    void
    read_files();

    void
    read_binary();

    void
    process_xml(tinyxml2::XMLDocument* doc,
                std::string l1, std::string l2,
                void (*fp)(tinyxml2::XMLElement*,int));

    void
    distribute_boxes();

    void
    distribute_events();

    void
    distribute_comp(int idx, event* ev);

    void
    distribute_comm(int idx, event* ev);

    void
    distribute_allreduce(int idx, event* ev);

    void
    fake_barrier();

    void
    fake_barrier_serial();

    void
    post_recvs();

    void
    run_loop();

    void
    finalize();

    void
    simple_event_done(int index);

    void
    clear_collective_deps(event* ev, event* evl);

    void
    collective_done(int box_number, int event_id);

    void 
    recv_boxes(int& n_events);

    bool
    send_boxes(int& n_events);

    bool
    compute_boxes(int& n_events);

    bool
    reduce(int& n_events);

    int count_events();

    int incoming_max_size();

    void post_more_recvs(int outcount);

    void post_generic_irecv (MPI_Request* request);

    void populate_listeners();

  public:
    /// Destructor.
    virtual
    ~boxml() throw () {}

    boxml() : barrier_tag_(-1), hist_eff_bw_(0), idle_time_(0) {}

    app*
    clone_type() {
      return new boxml;
    }

    void
    consume_params(sprockit::sim_parameters* params);

    std::string
    to_string() const
    {
      return "boxml";
    }

    /// Go.
    void
    skeleton_main();

  };

} // end of namespace lblxml

static inline double
get_real_time()
{
  //timeval t_st;
  //gettimeofday(&t_st, 0);
  //double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return sstmac_wall_time();
}

#endif
