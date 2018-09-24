
#include <sumi-mpi/otf2_output_stat.h>
#include <sumi-mpi/mpi_integers.h>
#include <sumi-mpi/mpi_comm/mpi_comm.h>
#include <sumi-mpi/mpi_api.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <limits>

#ifdef SSTMAC_OTF2_ENABLED

namespace sumi {

otf2_writer::otf2_writer(sprockit::sim_parameters *params) :
  stat_collector(params),
  min_time_(std::numeric_limits<uint64_t>::max()),
  max_time_(std::numeric_limits<uint64_t>::min())
{
}

void
otf2_writer::init(uint64_t start, uint64_t stop, int rank, int size)
{
  rank_ = rank;
  size_ = size;
  writer_.set_verbosity(dumpi::OWV_WARN);

  writer_.register_comm_world(MPI_COMM_WORLD, size, rank);
  writer_.register_comm_self(MPI_COMM_SELF);
  writer_.register_comm_null(MPI_COMM_NULL);
  writer_.register_null_request(MPI_REQUEST_NULL);
  writer_.open_archive(fileroot_);
  writer_.set_clock_resolution(sstmac::timestamp(1.0).ticks());
  writer_.generic_call(start, stop, "MPI_Init");

  std::shared_ptr<dumpi::OTF2_MPI_Comm> world(new dumpi::OTF2_MPI_Comm);
  world->local_id = MPI_COMM_WORLD;
  world->global_id = MPI_COMM_WORLD;
  world->is_root = rank == 0;
  world->local_rank = rank;
  world->world_rank = rank;

  std::shared_ptr<dumpi::OTF2_MPI_Group> group(new dumpi::OTF2_MPI_Group);
  world->group = group;
  group->is_comm_world = true;
  group->local_id = world->local_id;
  group->global_id = world->global_id;
}

void
otf2_writer::add_comm(mpi_comm* comm, dumpi::mpi_comm_t parent_id)
{
  dumpi::OTF2_MPI_Comm::shared_ptr stored = writer_.make_new_comm(comm->id());

  stored->local_id = comm->id();
  stored->global_id = comm->id();
  stored->is_root = comm->rank() == 0;
  stored->local_rank = comm->rank();
  stored->parent = writer_.get_comm(parent_id);
  stored->world_rank = comm->group()->at(comm->rank());

  std::shared_ptr<dumpi::OTF2_MPI_Group> group(new dumpi::OTF2_MPI_Group);
  if (comm->group()->is_comm_world()){
    group->is_comm_world = true;
  } else {
    group->global_ranks = comm->group()->world_ranks();
  }
  group->local_id = comm->group()->id();
  group->global_id = group->local_id;
  stored->group = group;
}

void
otf2_writer::reduce(sstmac::stat_collector* contrib)
{
  otf2_writer* other = dynamic_cast<otf2_writer*>(contrib);

  auto unique_comms = other->writer().find_unique_comms();
  all_comms_.insert(all_comms_.end(), unique_comms.begin(), unique_comms.end());

  if (event_counts_.empty()) event_counts_.resize(size_);

  event_counts_[other->rank_] = other->writer().event_count();

  min_time_ = std::min(min_time_, other->writer().start_time());
  max_time_ = std::min(max_time_, other->writer().stop_time());
}

void
otf2_writer::assign_global_comm_ids(mpi_api* mpi)
{
  //first, we have to do a parallel scan to assign globally unique ids
  //to each MPI rank

  auto& comm_map = writer_.all_comms();

  int numOwnedComms = 0;
  int numComms = 0;
  for (auto& pair : comm_map){
    auto& list = pair.second;
    for (auto& comm : list){
      if (comm->is_root) ++numOwnedComms;
      ++numComms;
    }
  }

  int myCommOffset = 0;

  auto* op = mpi->start_scan("OTF2 ID agree", MPI_COMM_WORLD, 1, MPI_INT,
                             MPI_SUM,  &numOwnedComms, &myCommOffset);
  mpi->wait_collective(op);
  delete op;

  //this is an inclusive scan... we needed an exclusive scan
  myCommOffset -= numOwnedComms;

  int myCommId = myCommOffset;

  //now broadcast the IDs to everyone
  std::vector<collective_op_base*> ops(numComms);
  std::vector<int> globalIds(numComms);
  int idx = 0;
  for (auto& pair : comm_map){
    auto& list = pair.second;
    for (auto& comm : list){
      int* buffer = &globalIds[idx];
      if (comm->is_root){
        comm->global_id = globalIds[idx] = myCommId;
        comm->group->global_id = comm->global_id + 1;
        ++myCommId;
      }
      ops[idx] = mpi->start_bcast("OTF2_finalize_bcast", comm->local_id,
                                  1, MPI_INT, 0, &globalIds[idx]);
      ++idx;
    }
  }

  for (auto* op : ops){
    mpi->wait_collective(op);
    delete op;
  }

  /** all global ids have been received, log them */
  idx = 0;
  for (auto& pair : comm_map){
    auto& list = pair.second;
    for (auto& comm : list){
      if (!comm->is_root){
        comm->global_id = globalIds[idx];
        comm->group->global_id = comm->global_id;
      }
      ++idx;
    }
  }

}

void
otf2_writer::global_reduce(sstmac::parallel_runtime* rt)
{
  if (rt->nproc() == 1) return;

  sprockit::abort("unimplemented: OTF2 trace output global reduce");
}

void
otf2_writer::clear()
{
}

void
otf2_writer::dump_local_data()
{
  writer_.write_local_def_file();
  if (rank_ != 0){
    //still need rank 0 around
    writer_.close_archive();
  }
}

void
otf2_writer::dump_global_data()
{
  if (rank_ != 0){
    spkt_abort_printf("main otf2_writer::dump_global_data() called on rank %d != 0", rank_);
  }
  writer_.write_global_def_file(event_counts_, all_comms_, min_time_, max_time_);
  writer_.close_archive();
}

}

#endif
