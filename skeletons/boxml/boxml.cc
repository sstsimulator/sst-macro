#include "boxml.h"
#include "lock_array.h"
#include <sprockit/factories/factory.h>
#include <sstmac/software/libraries/unblock_handler.h>
#include <sstmac/common/sim_thread_lock.h>

using namespace std;
using namespace sstmac;
using namespace sstmac::sw;
using namespace sumi;

DeclareSerializable(lblxml::box);
DeclareSerializable(lblxml::reduce);
DeclareSerializable(lblxml::comm);
DeclareSerializable(lblxml::comp);


namespace lblxml
{
  SpktRegisterApp("boxml", boxml);

  bool boxml::have_data_ = false;
  bool boxml::have_input_bin_ = false;
  bool boxml::have_output_bin_ = false;
  bool boxml::checked_bin_ = false;
  std::fstream boxml::bin_file_;

  void delete_globals(){
    int nevents = g_events.size();
    for (int i=0; i < nevents; ++i){
      event* ev = g_events[i];
      if (ev) delete ev;
    }

    int nboxes = g_boxes.size();
    for (int i=0; i < nboxes; ++i){
      box* b = g_boxes[i];
      delete b;
    }
  }

  void
  boxml::consume_params(sprockit::sim_parameters* params)
  {
    if (eventfiles_.empty())
      params->get_vector_param ("boxml_eventfiles", eventfiles_);
    boxfile_ =
        params->get_param("boxml_boxfile");
    assignment_ =
        params->get_param("boxml_assignment");
    message_factor_ =
        params->get_int_param("boxml_message_factor");
    compute_scale_ =
        params->get_optional_double_param("boxml_compute_scale", 1.0);
    do_compute_ =
        params->get_bool_param("boxml_do_compute");
    debug_ =
        params->get_optional_int_param("boxml_debug",0);
    randomize_events_ =
        params->get_bool_param("boxml_randomize_events");
    detailed_progress_ =
        params->get_optional_bool_param("boxml_detailed_progress",false);
    nevents_ = 
        params->get_int_param("boxml_events");
    round_robin_ =
        params->get_optional_bool_param("boxml_round_robin",false);
    minimize_locks_ = 
        params->get_optional_bool_param("boxml_minimize_locks",false);    

    if (!checked_bin_){
      if (params->has_param("boxml_binary_file")){
        std::string bin_file = params->get_param("boxml_binary_file");
        ifstream test(bin_file.c_str());
        if (test.good()){
          test.close();
          bin_file_.open(bin_file.c_str(), ios::in | ios::binary);
          have_input_bin_ = true;
        }  else {
          bin_file_.open(bin_file.c_str(), ios::out | ios::binary);
          have_output_bin_ = true;
        }
      }
      checked_bin_ = true;
    }

    xml_read_only_ = params->get_optional_bool_param("boxml_xml_only", false);
  }

  // instantiate the globals (that we really do want to be global)
  index_to_rank_t g_boxindex_to_rank;
  box_map_t g_boxes;
  event_map_t g_events;

  rank_to_set_t g_rank_to_comps;
  rank_to_set_t g_rank_to_recvs;
  rank_to_set_t g_rank_to_sends;
  rank_to_list_t g_rank_to_allreduces;
  rank_to_da_list_t g_rank_to_valid_sends;
  rank_to_da_list_t g_rank_to_valid_comps;
  rank_to_da_list_t g_rank_to_valid_allreduces;

  std::map<int, std::vector<bool> > g_reduce_to_box_running;

  /** TODO: don't think these are still used
  comm_map_t g_comm_map;
  comm_map_t g_coll_map;
  comp_map_t g_comp_map;
  comm_maps_t g_send_maps;
  comm_maps_t g_recv_maps;
  comm_maps_t g_coll_maps;
  comp_maps_t g_comp_maps;
  */

  void
  boxml::skeleton_main()
  {

    double start;
    double stop;
    double wall_time;
    static sim_thread_lock* lock = sim_thread_lock::construct();

    init();


    // first rank to reach this point does the setup
#ifdef SSTMAC_USE_MULTITHREAD
    lock->lock();
#endif
    if (!have_data_) {
#ifdef SSTMAC_USE_MULTITHREAD
      init_event_locks();
#endif
      g_events.resize(nevents_, NULL);
      g_rank_to_comps.resize(commsize_);
      g_rank_to_sends.resize(commsize_);
      g_rank_to_recvs.resize(commsize_);
      g_rank_to_allreduces.resize(commsize_);
      g_rank_to_valid_allreduces.resize(commsize_);
      g_rank_to_valid_sends.resize(commsize_);
      g_rank_to_valid_comps.resize(commsize_);

      start = get_real_time();

      // the setup happens here
      if (have_input_bin_)
        read_binary();
      else
        read_files();
      distribute_boxes();
      distribute_events();
      have_data_ = true;

      stop = get_real_time();
      wall_time = stop - start;
      cout << "boxml setup ran for " << wall_time << " s\n";
      cout << std::flush;
      cerr << std::flush;
    }
#ifdef SSTMAC_USE_MULTITHREAD
    lock->unlock();
#endif

    if (xml_read_only_) {
      std::cerr << "readonly exiting\n";
      return;
    }

    if (rank_ == 0) sstmac_runtime::enter_deadlock_region();
    comm_barrier(barrier_tag_); // can't go on before data is set up
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    ++barrier_tag_;

    comm_barrier(barrier_tag_);
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    ++barrier_tag_;
    if (rank_ == 0) {
      std::cout << g_events.size() << " total events\n";
      start = get_real_time();
    }
    comm_barrier(barrier_tag_);
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    ++barrier_tag_;

    // the actual simulation happens here
    run_loop();

    comm_barrier(barrier_tag_);
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    ++barrier_tag_;

    if (rank_ == 0) {
      stop = get_real_time();
      wall_time = stop - start;
      cout << "boxml simulation ran for " << wall_time << " s\n";
    }

    comm_barrier(barrier_tag_);
    comm_collective_block(sumi::collective::barrier, barrier_tag_);
    ++barrier_tag_;

    sstmac_runtime::exit_deadlock_region();
    finalize();
    
    if (rank_==0) {
      std::cout << "Rank 0 finalized" << std::endl;
    }

    //at this point, no one should have any more events - delete stuff
    if (rank_ == 0){
      delete_globals();
    }
  }

  void
  boxml::init()
  {
    SSTMACBacktrace("Init");

    std::srand(std::time(0));

    comm_init();

    tport_ = sumi_api();
    sstmac_runtime::add_deadlock_check(
      new_deadlock_check(tport_, &sumi::transport::deadlock_check));

    rank_ = comm_rank();
    commsize_ = comm_nproc();
    if (debug_ > 1)
      printf("Inited on rank %d of %d\n",
             rank_, commsize_); fflush(stdout);
  }

  void
  boxml::finalize()
  {
    comm_finalize();
    if (debug_ > 0) cout << "past finalize " << now() << "\n";
  }

} // end of namespace sstmac.
