#include <sumi-mpi/mpi_delay_stats.h>

#if !SSTMAC_INTEGRATED_SST_CORE

namespace sumi {

DelayStats::DelayStats(SST::BaseComponent* comp, const std::string& name,
            const std::string& subName, SST::Params& params)
  : Parent(comp, name, subName, params)
{
}

void
DelayStats::addData_impl(int src, int dst, int type, uint64_t bytes, double delay)
{
  messages_.emplace_back(src,dst,type,bytes,delay);
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
  out_ << "component,src,dst,type,size,time";
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
         << "," << m.src << "," << m.dst
         << "," << m.type
         << "," << m.length << "," << m.delay;
  }
}

}


#endif
