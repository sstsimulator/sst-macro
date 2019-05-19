#ifndef sumi_mpi_otf2_output_stat
#define sumi_mpi_otf2_output_stat

#include <sstmac/common/sstmac_config.h>

#ifdef SSTMAC_OTF2_ENABLED

#include <sstmac/common/stats/stat_collector.h>
#include <dumpi/libotf2dump/otf2writer.h>
#include <sumi-mpi/mpi_comm/mpi_comm_fwd.h>
#include <sumi-mpi/mpi_api_fwd.h>

namespace sumi {

class OTF2Writer : public SST::Statistics::CustomStatistic
{
 public:
    SST_ELI_REGISTER_CUSTOM_STATISTIC(
      OTF2Writer,
      "macro",
      "otf2writer",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "Writes OTF2 traces capturing the simulation")

  OTF2Writer(SST::BaseComponent* parent, const std::string& name,
             const std::string& subNname, SST::Params& params);

  dumpi::OTF2_Writer& writer(){
    return writer_;
  }

  void dumpLocalData();

  void dumpGlobalData();

  void reduce(OTF2Writer* stat);

  void init(uint64_t start, uint64_t stop, int rank, int size);

  void addComm(MpiComm* comm, dumpi::mpi_comm_t parent_comm);

  void assignGlobalCommIds(MpiApi* mpi); 

 private:
  dumpi::OTF2_Writer writer_;
  int rank_;
  int size_;
  uint64_t min_time_;
  uint64_t max_time_;
  std::vector<int> event_counts_;
  std::vector<dumpi::OTF2_MPI_Comm::shared_ptr> all_comms_;
  std::string fileroot_;

};

class OTF2Output : public SST::Statistics::StatisticOutput
{
 public:
  SST_ELI_REGISTER_DERIVED(
    SST::Statistics::StatisticOutput,
    OTF2Output,
    "macro",
    "otf2",
    SST_ELI_ELEMENT_VERSION(1,0,0),
    "Writes OTF2 traces capturing the simulation")

  OTF2Output(SST::Params& params);

  void startRegisterGroup(SST::Statistics::StatisticGroup *grp) override {}
  void stopRegisterGroup() override {}

  void registerStatistic(SST::Statistics::StatisticBase* stat) override {}

  void startOutputGroup(SST::Statistics::StatisticGroup * grp) override;
  void stopOutputGroup() override;

  void output(SST::Statistics::StatisticBase* statistic, bool endOfSimFlag) override;

  bool checkOutputParameters() override { return true; }
  void startOfSimulation() override {}
  void endOfSimulation() override {}
  void printUsage() override {}

 private:
  OTF2Writer* first_in_grp_;

};

}

#endif

#endif
