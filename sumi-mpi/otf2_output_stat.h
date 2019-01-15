#ifndef sumi_mpi_otf2_output_stat
#define sumi_mpi_otf2_output_stat

#include <sstmac/common/sstmac_config.h>

#ifdef SSTMAC_OTF2_ENABLED
#include <sstmac/common/stats/stat_collector.h>
#include <dumpi/libotf2dump/otf2writer.h>
#include <sumi-mpi/mpi_comm/mpi_comm_fwd.h>
#include <sumi-mpi/mpi_api_fwd.h>

namespace sumi {

class otf2_writer : public sstmac::stat_collector
{
  FactoryRegister("otf2", stat_collector, otf2_writer)
 public:
  otf2_writer(sprockit::sim_parameters::ptr& params);

  dumpi::OTF2_Writer& writer(){
    return writer_;
  }

  std::string toString() const override {
    return "OTF2 trace writer";
  }

  sstmac::stat_collector* doClone(sprockit::sim_parameters::ptr params) const override {
    spkt_abort_printf("otf2_writer should not be cloned");
    return nullptr;
    //otf2_writer* writer = new otf2_writer(params);
    //writer->size_ = size_;
    //return writer;
  }

  bool isMain() const override {
    return rank_ == 0;
  }

  void dumpLocalData() override;

  void dumpGlobalData() override;

  void globalReduce(sstmac::parallel_runtime* rt) override;

  void reduce(sstmac::stat_collector* stat) override;

  void clear() override;

  void init(uint64_t start, uint64_t stop, int rank, int size);

  void add_comm(mpi_comm* comm, dumpi::mpi_comm_t parent_comm);

  void assign_global_comm_ids(mpi_api* mpi);

 private:
  dumpi::OTF2_Writer writer_;
  int rank_;
  int size_;
  uint64_t min_time_;
  uint64_t max_time_;
  std::vector<int> event_counts_;
  std::vector<dumpi::OTF2_MPI_Comm::shared_ptr> all_comms_;

};

}
#endif

#endif
