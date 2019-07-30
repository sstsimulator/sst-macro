#include <sumi-mpi/mpi_delay_stats.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sumi {

DelayStats::DelayStats(SST::BaseComponent* comp, const std::string& name,
            const std::string& subName, SST::Params& params)
  : Parent(comp, name, subName, params)
{
}

void
DelayStats::addData_impl(int src, int dst, int type, int stage, uint64_t bytes,
                         double sync_delay, double contention_delay,
                         double comm_delay, double min_delay, 
                         double active_sync_delay, double active_delay)
{
  messages_.emplace_back(src,dst,type,stage,bytes,
                         sync_delay, contention_delay,
                         comm_delay, min_delay, 
                         active_sync_delay, active_delay);
}

void
DelayStats::registerOutputFields(SST::Statistics::StatisticFieldsOutput *statOutput)
{
  sprockit::abort("DelayStats::registerOutputFields: should not be called");
}

void
DelayStats::outputStatisticData(SST::Statistics::StatisticFieldsOutput *output, bool endOfSimFlag)
{
  sprockit::abort("DelayStats::outputStatisticData: should not be called");
}

DelayStatsOutput::DelayStatsOutput(SST::Params& params) :
    sstmac::StatisticOutput(params)
{
}

void
DelayStatsOutput::startOutputGroup(sstmac::StatisticGroup *grp)
{
  auto outfile = grp->name + ".csv";
  out_ = std::ofstream(outfile);
  out_ << "component,src,dst,type,stage,size,sync,injection,network,min,active_sync,active_total";
}

void
DelayStatsOutput::stopOutputGroup()
{
  out_.close();
}

void
DelayStatsOutput::output(SST::Statistics::StatisticBase* statistic, bool endOfSimFlag)
{
  DelayStats* stats = dynamic_cast<DelayStats*>(statistic);
  for (auto iter=stats->begin(); iter != stats->end(); ++iter){
    const DelayStats::Message& m = *iter;
    out_ << "\n" << stats->getStatSubId()
         << "," << m.src
         << "," << m.dst
         << "," << m.type
         << "," << m.stage
         << "," << m.length
         << "," << m.sync_delay
         << "," << m.inj_delay
         << "," << m.contention_delay
         << "," << m.min_delay
         << "," << m.active_sync_delay
         << "," << m.active_delay
    ;
  }
}

}


#endif
