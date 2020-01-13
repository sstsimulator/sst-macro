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
                         double send_sync_delay, double recv_sync_delay,
                         double contention_delay,
                         double comm_delay, double min_delay, 
                         double active_sync_delay, double active_delay,
                         double time_since_quiesce, double time)
{
  messages_.emplace_back(src,dst,type,stage,bytes,
                         send_sync_delay, recv_sync_delay,
                         contention_delay,
                         comm_delay, min_delay, 
                         active_sync_delay, active_delay, 
                         time_since_quiesce, time);
}

void
DelayStats::registerOutputFields(SST::Statistics::StatisticFieldsOutput * /*statOutput*/)
{
  sprockit::abort("DelayStats::registerOutputFields: should not be called");
}

void
DelayStats::outputStatisticFields(SST::Statistics::StatisticFieldsOutput * /*output*/, bool  /*endOfSimFlag*/)
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
  out_.open(outfile);
  out_ << "component,src,dst,type,stage,size,send_sync,recv_sync,injection,network,min,active_sync,active_total,quiesce_time,time";
}

void
DelayStatsOutput::stopOutputGroup()
{
  out_.close();
}

void
DelayStatsOutput::output(SST::Statistics::StatisticBase* statistic, bool  /*endOfSimFlag*/)
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
         << "," << m.send_sync_delay
         << "," << m.recv_sync_delay
         << "," << m.inj_delay
         << "," << m.contention_delay
         << "," << m.min_delay
         << "," << m.active_sync_delay
         << "," << m.active_delay
         << "," << m.time_since_quiesce
         << "," << m.time
    ;
  }
}

}


#endif
