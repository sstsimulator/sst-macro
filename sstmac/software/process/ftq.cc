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

#include <sstmac/software/process/ftq.h>
#include <sstmac/software/process/thread.h>
#include <sstmac/backends/common/parallel_runtime.h>
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
namespace sw {

static const char* matplotlib_histogram_text_header =
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
  sprockit::abort("FTQCalendar::registerOutputFields: should never be called");
}

void
FTQCalendar::outputStatisticData(StatisticFieldsOutput *output, bool endOfSimFlag)
{
  sprockit::abort("FTQCalendar::outputStatisticData: should never be called");
}

FTQOutput::FTQOutput(SST::Params& params) :
  sstmac::StatisticOutput(params),
  events_used_(0),
  max_tick_(0),
  num_epochs_(0),
  ticks_per_epoch_(0)
{
  if (params.contains("num_epochs")){
    num_epochs_ = params.find<long>("num_epochs");
  } else if (params.contains("epoch_length")) {
    SST::UnitAlgebra length = params.find<SST::UnitAlgebra>("epoch_length");
    sstmac::TimeDelta time(length.toDouble());
    ticks_per_epoch_ = time.ticks();
  } else {
    spkt_abort_printf("must specify either num_epochs or epoch_length for FTQOutput");
  }
}

void
FTQOutput::startOutputGroup(StatisticGroup *grp)
{
  active_group_ = grp->name;
}

void
FTQOutput::output(StatisticBase *statistic, bool endOfSimFlag)
{
  FTQCalendar* calendar = dynamic_cast<FTQCalendar*>(statistic);
  if (!calendar){
    spkt_abort_printf("FTQOutput can only be used with FTQCalendar statistic");
  }
  if (!calendar->empty()){
    events_used_ = events_used_ | calendar->eventsUsed();
    max_tick_ = std::max(max_tick_, calendar->maxTick());
    calendars_.push_back(calendar);
  }
}

void
FTQOutput::stopOutputGroup()
{
  if (num_epochs_){
    //specified as fixed number of epochs
    ticks_per_epoch_ = max_tick_ / num_epochs_;
    if (max_tick_ % num_epochs_){
      //if there's a remainer, add another epoch
      num_epochs_++;
    }
  } else if (ticks_per_epoch_){
    //specified not as fixed number of epochs
    //but as a specific length for the epoch
    num_epochs_ = max_tick_ / ticks_per_epoch_;
    if (max_tick_ % ticks_per_epoch_){
      //if remainder, add another epoch
      num_epochs_++;
    }
  }

  //not all events are used - map them into a dense mapping
  //for only the events that are actually counted
  int dense_index[64];
  int sparse_index[64];
  uint64_t event_totals[64];
  int num_event_types = 0;
  for (int i=0; i < 64; ++i){
    uint64_t mask = uint64_t(1)<<i;
    if (mask & events_used_){
      dense_index[i] = num_event_types;
      sparse_index[num_event_types] = i;
      num_event_types++;
    }
    event_totals[i] = 0;
  }


  EpochList epochs(num_event_types, num_epochs_);
  for (FTQCalendar* calendar : calendars_){
    for (auto& ev : calendar->events()){
      int event_id_remapped = dense_index[ev.type];

      event_totals[event_id_remapped] += ev.length;
      uint64_t event_stop = ev.start + ev.length;
      uint64_t start_epoch = ev.start / ticks_per_epoch_;
      uint64_t stop_epoch = event_stop / ticks_per_epoch_;

      if (start_epoch == stop_epoch){
        epochs(event_id_remapped,start_epoch) += ev.length;
      } else {
        uint64_t first_time = (start_epoch+1)*ticks_per_epoch_ - ev.start;
        epochs(event_id_remapped,start_epoch) += first_time;
        uint64_t last_time = event_stop - stop_epoch*ticks_per_epoch_;
        epochs(event_id_remapped,stop_epoch) += last_time;
        for (uint64_t ep=start_epoch+1; ep < stop_epoch; ++ep){
          epochs(event_id_remapped,ep) += ticks_per_epoch_;
        }
      }
    }
  }

  std::string dat_fname = sprockit::printf("%s.dat", active_group_.c_str());
  std::ofstream dat_out(dat_fname.c_str());
  //print the first line header
  dat_out << sprockit::printf("%12s", "Epoch(us)");

  //sort the categories
  std::map<std::string, int> sorted_keys;
  for (int i=0; i < num_event_types; ++i){
    int index = sparse_index[i];
    sorted_keys[FTQTag::name(index)] = i;
  }

  for (auto& pair : sorted_keys){
    dat_out << sprockit::printf(" %12s", pair.first.c_str());
  }

  dat_out << sprockit::printf(" %12s", "Total");

  std::stringstream sstr;
  sstr << "\n";
  TimeDelta one_ms(1e-3);
  int64_t ticks_ms = one_ms.ticks();
  for (uint64_t ep=0; ep < num_epochs_; ++ep) {
    //figure out how many us
    double num_ms = double(ep * ticks_per_epoch_) / (double) ticks_ms;
    sstr << sprockit::printf("%12.4f", num_ms);
    uint64_t total_ticks = 0;
    for (int i=0; i < num_event_types; ++i) {
      uint64_t total_ev_ticks = epochs(i,ep);
      sstr << sprockit::printf(" %12llu", total_ev_ticks);
      total_ticks += total_ev_ticks;
    }
    sstr << sprockit::printf(" %12llu", total_ticks);
    sstr << "\n";
  }
  dat_out << sstr.str();
  dat_out.close();

  std::string plt_fname = sprockit::printf("%s.py", active_group_.c_str());
  std::ofstream plt_out(plt_fname.c_str());
  plt_out << matplotlib_histogram_text_header << active_group_ << matplotlib_histogram_text_footer;
  plt_out.close();

  TimeDelta stamp_sec(1.0);
  uint64_t ticks_s = stamp_sec.ticks();
  std::cout << sprockit::printf("Aggregate time stats for %s: \n", active_group_.c_str());
  for (auto& pair : sorted_keys){
    int idx = pair.second;
    double num_s = event_totals[idx] / ticks_s;
    uint64_t remainder = event_totals[idx] - ticks_s*num_s;
    double rem_s = double(remainder) / double(ticks_s);
    double t_sec = num_s + rem_s;
    std::cout << sprockit::printf("%16s: %16.5f s\n", pair.first.c_str(), t_sec);
  }
}
#endif

// ftq_scope member functions
FTQScope::FTQScope(Thread* thr, const FTQTag& tag) :
  prev_tag_(thr->tag()), thr_(thr)
{
  if (tag.level() >= thr->tag().level()){
    thr->setTag(tag);
  }
}

FTQScope::~FTQScope()
{
  thr_->setTag(prev_tag_);
}

}
}
