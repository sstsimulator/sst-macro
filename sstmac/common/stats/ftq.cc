/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/software/process/thread.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/ftq.h>
#include <sstmac/common/stats/ftq_tag.h>
#include <sstmac/common/thread_lock.h>
#include <sstmac/common/event_scheduler.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>
#include <sstream>

RegisterKeywords(
 { "epoch", "the size of a time epoch" },
);

namespace sstmac {

static sstmac::FTQTag inactive("Inactive", 1);

static const char* matplotlib_text_header =
    "#!/usr/bin/env python3\n"
    "\n"
    "try:\n"
    "    import sys\n"
    "    import numpy as np\n"
    "    import matplotlib.pyplot as plt\n"
    "    import argparse\n"
    "except ImportError:\n"
    "    print('ImportError caught. Please install matplotlib')\n"
    "    exit()\n"
    "\n"
    "import numpy as np\n"
    "import matplotlib.pyplot as plt\n"
    "import argparse\n"
    "\n"
    "# Getting CLI args\n"
    "parser = argparse.ArgumentParser()\n"
    "parser.add_argument('--show', action='store_true', help='display the plot on screen')\n"
    "parser.add_argument('--title', default='Histogram plot', help='set the title')\n"
    "parser.add_argument('--eps', action='store_true', help='output .eps file')\n"
    "parser.add_argument('--pdf', action='store_true', help='output .pdf file')\n"
    "parser.add_argument('--png', action='store_true', help='output .png file')\n"
    "parser.add_argument('--svg', action='store_true', help='output .svg file')\n"
    "args = parser.parse_args()\n"
    "\n"
    "# Parsing the data file\n"
    "file_name='";

static const char* matplotlib_histogram_text_footer =
    "'\n"
    "with open(file_name + '.dat') as f:\n"
    "    names = f.readline().split()\n"
    "    data = np.loadtxt(f, dtype=float).transpose()\n"
    "    time, normalized = data[0], np.divide(data[1:-1],data[-1])\n"
    "\n"
    "# Plot fomatting\n"
    "plt.xlabel('Time (us)')\n"
    "plt.xlim(time[0], time[-1])\n"
    "plt.ylim(0,1)\n"
    "plt.yticks([])\n"
    "plt.title(args.title)\n"
    "polys = plt.stackplot(time, normalized)\n"
    "legendProxies = []\n"
    "for poly in polys:\n"
    "   legendProxies.append(plt.Rectangle((0, 0), 1, 1, fc=poly.get_facecolor()[0]))\n"
    "plt.legend(legendProxies, names[1:])\n"
    "\n"
    "# Saving\n"
    "if args.eps: plt.savefig(file_name + '.eps')\n"
    "if args.pdf: plt.savefig(file_name + '.pdf')\n"
    "if args.png: plt.savefig(file_name + '.png')\n"
    "if args.svg: plt.savefig(file_name + '.svg')\n"
    "\n"
    "if args.show:\n"
    "    plt.show()\n";


static const char* matplotlib_mean_text_footer =
    "'\n"
    "with open(file_name + '.dat') as f:\n"
    "    names = f.readline().split()\n"
    "    data = np.loadtxt(f, dtype=float).transpose()\n"
    "    time, means = data[0], data[1]"
    "\n"
    "# Plot fomatting\n"
    "plt.xlabel('Time (us)')\n"
    "plt.xlim(time[0], time[-1])\n"
    "plt.ylabel('Mean')\n"
    "plt.title(args.title)\n"
    "polys = plt.plot(time, means)\n"
    "# Saving\n"
    "if args.eps: plt.savefig(file_name + '.eps')\n"
    "if args.pdf: plt.savefig(file_name + '.pdf')\n"
    "if args.png: plt.savefig(file_name + '.png')\n"
    "if args.svg: plt.savefig(file_name + '.svg')\n"
    "\n"
    "if args.show:\n"
    "    plt.show()\n";


#if !SSTMAC_INTEGRATED_SST_CORE

FTQAccumulator::FTQAccumulator(SST::BaseComponent *comp, const std::string &name,
                               const std::string &subName, SST::Params &params) :
  SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>(comp,name,subName,params)
{
}

void
FTQAccumulator::registerOutputFields(SST::Statistics::StatisticOutput *statOutput)
{
  sprockit::abort("FTQAccumulator::registerOutputFields: not yet implemented");
}

void
FTQAccumulator::outputStatisticData(SST::Statistics::StatisticOutput *statOutput, bool endOfSim)
{
  sprockit::abort("FTQAccumulator::outputStatisticData: not yet implemented");
}

FTQCalendar::FTQCalendar(SST::BaseComponent *comp, const std::string &name,
                         const std::string &subName, SST::Params &params) :
  SST::Statistics::MultiStatistic<int,uint64_t,uint64_t>(comp,name,subName,params),
  events_used_(0)
{
}

void
FTQCalendar::padToMaxTick(uint64_t max_tick)
{
  uint64_t stop = 0;
  uint64_t inactive_length = max_tick;
  if (!events_.empty()){
    auto& last = events_.back();
    stop = last.start + last.length;
    inactive_length = max_tick - stop;
  }
#if SSTMAC_SANITY_CHECK
  if (max_tick < stop){
    spkt_abort_printf("Bad FTQ collection: max tick is less than stop time");
  }
#endif
  addData_impl(inactive.id(), stop, inactive_length);
}

void
FTQCalendar::addData_impl(int event_typeid, uint64_t ticks_begin, uint64_t num_ticks)
{
  if (num_ticks){
    events_.emplace_back(event_typeid, ticks_begin, num_ticks);
    events_used_ = events_used_ | (1<<event_typeid);
  }
}

void
FTQCalendar::registerOutputFields(StatisticFieldsOutput *statOutput)
{
  sprockit::abort("FTQCalendar::registerOutputFields: should never be called - ensure output is type 'ftq'");
}

void
FTQCalendar::outputStatisticData(StatisticFieldsOutput *output, bool endOfSimFlag)
{
  sprockit::abort("FTQCalendar::outputStatisticData: should never be called - ensure output is type 'ftq'");
}

FTQOutput::FTQOutput(SST::Params& params) :
  sstmac::StatisticOutput(params),
  ticks_per_epoch_(0)
{
  if (params.contains("epoch_length")) {
    SST::UnitAlgebra length = params.find<SST::UnitAlgebra>("epoch_length");
    sstmac::TimeDelta time(length.toDouble());
    ticks_per_epoch_ = time.ticks();
  } else {
    spkt_abort_printf("must specify epoch_length for FTQOutput");
  }
  compute_mean_ = params.find<bool>("compute_mean", false);
  use_ftq_tags_ = params.find<bool>("use_ftq_tags", !compute_mean_);
  aggregate_ = params.find<bool>("aggregate", true);

  if (use_ftq_tags_ && compute_mean_){
    spkt_abort_printf("FTQOutput: cannot use FTQ tags with mean computation");
  }
}

void
FTQOutput::startOutputGroup(StatisticGroup *grp)
{
  active_group_ = grp->name;

  std::string dat_fname = sprockit::printf("%s.csv", active_group_.c_str());
  out_.open(dat_fname.c_str());
  includeHeaders_ = true;
}

void
FTQOutput::output(StatisticBase *statistic, bool endOfSimFlag)
{
  FTQCalendar* calendar = dynamic_cast<FTQCalendar*>(statistic);
  if (!calendar){
    spkt_abort_printf("FTQOutput can only be used with FTQCalendar statistic");
  }

  if (aggregate_){
    aggregateCalendars_.push_back(calendar);
  } else {
    individualCalendars_[calendar->getStatSubId()].push_back(calendar);
  }

}

void
FTQOutput::dump(const std::vector<FTQCalendar*>& calendars, std::ostream& os,
                bool includeHeaders, const std::string& name)
{
  uint64_t events_used = 0;
  uint64_t max_tick = 0;
  for (FTQCalendar* calendar : calendars){
    max_tick = std::max(max_tick, calendar->maxTick());
  }
  for (FTQCalendar* calendar : calendars){
    calendar->padToMaxTick(max_tick);
    events_used = events_used | calendar->eventsUsed();
  }
  uint64_t num_epochs = max_tick / ticks_per_epoch_;
  if (max_tick % ticks_per_epoch_){
    //if remainder, add another epoch
    num_epochs++;
  }

  //not all events are used - map them into a dense mapping
  //for only the events that are actually counted
  int dense_index[64];
  int sparse_index[64];
  uint64_t event_totals[64];
  int num_event_types = 0;
  for (int i=0; i < 64; ++i){
    uint64_t mask = uint64_t(1)<<i;
    if (mask & events_used){
      dense_index[i] = num_event_types;
      sparse_index[num_event_types] = i;
      num_event_types++;
    }
    event_totals[i] = 0;
  }
  int num_epoch_columns = compute_mean_ ? 1 : num_event_types;

  EpochList epochs(num_epoch_columns, num_epochs);
  for (FTQCalendar* calendar : calendars){
    for (auto& ev : calendar->events()){
      int event_id_remapped = compute_mean_ ? 0 : dense_index[ev.type];
      int scale = compute_mean_ ? ev.type : 1;

      event_totals[event_id_remapped] += ev.length;
      uint64_t event_stop = ev.start + ev.length;
      uint64_t start_epoch = ev.start / ticks_per_epoch_;
      uint64_t stop_epoch = event_stop / ticks_per_epoch_;

      if (start_epoch == stop_epoch){
        epochs(event_id_remapped,start_epoch) += ev.length;
      } else {
        uint64_t first_time = (start_epoch+1)*ticks_per_epoch_ - ev.start;
        epochs(event_id_remapped,start_epoch) += first_time * scale;
        uint64_t last_time = event_stop - stop_epoch*ticks_per_epoch_;
        epochs(event_id_remapped,stop_epoch) += last_time * scale;
        for (uint64_t ep=start_epoch+1; ep < stop_epoch; ++ep){
          epochs(event_id_remapped,ep) += ticks_per_epoch_ * scale;
        }
      }
    }
  }
  //sort the categories
  std::vector<std::string> keys(num_event_types);
  if (use_ftq_tags_){
    for (int i=0; i < num_event_types; ++i){
      int index = sparse_index[i];
      keys[i] = FTQTag::name(index);
    }
  } else {
    for (int i=0; i < num_event_types; ++i){
      int index = sparse_index[i];
      keys[i] = sprockit::printf("%d", index);
    }
  }

  if (includeHeaders){
    //print the first line header
    os << "Epoch,Time";
    if (compute_mean_){
      os << ",Mean";
    } else {
      for (auto& key : keys){
        os << "," << key;
      }
    }
    if (!name.empty()){
      os << ",Name";
    }
    os << "\n";
  }
  TimeDelta one_ms(1e-3);
  uint64_t ticks_ms = one_ms.ticks();
  double mean_denominator = calendars.size() * ticks_per_epoch_;
  for (uint64_t ep=0; ep < num_epochs; ++ep) {
    //figure out how many us
    double num_ms = double(ep * ticks_per_epoch_) / (double) ticks_ms;
    os << ep << "," << sprockit::printf("%12.4f", num_ms);
    for (int i=0; i < num_epoch_columns; ++i) {
      uint64_t total_ev_ticks = epochs(i,ep);
      if (compute_mean_){
        double mean = total_ev_ticks / mean_denominator;
        os << "," << mean;
      } else {
        os << "," << total_ev_ticks;
      }
    }
    if (!name.empty()){
      os << "," << name;
    }
    os << "\n";
  }
  if (!compute_mean_ && name.empty()){
    TimeDelta stamp_sec(1.0);
    uint64_t ticks_s = stamp_sec.ticks();
    std::cout << sprockit::printf("Aggregate time stats: %s\n", active_group_.c_str());
    for (int idx=0; idx < num_epoch_columns; ++idx){
      double num_s = event_totals[idx] / ticks_s;
      uint64_t remainder = event_totals[idx] - ticks_s*num_s;
      double rem_s = double(remainder) / double(ticks_s);
      double t_sec = num_s + rem_s;
      std::cout << sprockit::printf("%16s: %16.5f s\n", keys[idx].c_str(), t_sec);
    }
  }
}

void
FTQOutput::stopOutputGroup()
{
  if (aggregate_){
    dump(aggregateCalendars_, out_, true, "");
  } else {
    bool includeHeaders = true;
    for (auto& pair : individualCalendars_){
      dump(pair.second, out_, includeHeaders, pair.first);
      includeHeaders = false;
    }
  }
  out_.close();
}


#endif

}

