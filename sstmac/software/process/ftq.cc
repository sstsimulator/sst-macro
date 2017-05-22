/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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
#include <sstmac/common/thread_lock.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/delete.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords("epoch");

namespace sstmac {
namespace sw {

spkt_unordered_map<int, app_ftq_calendar*> ftq_calendar::calendars_;
const long app_ftq_calendar::allocation_num_epochs = 10000;

ftq_calendar::ftq_calendar(sprockit::sim_parameters* params) :
  num_ticks_epoch_(0),
  stat_collector(params)
{
  num_ticks_epoch_ = timestamp(params->get_time_param("epoch")).ticks_int64();
}

ftq_calendar::~ftq_calendar()
{
  sprockit::delete_vals(calendars_);
  calendars_.clear();
}

void
ftq_calendar::init(long nticks_per_epoch)
{
  num_ticks_epoch_ = nticks_per_epoch;
}

void
ftq_calendar::register_app(int aid, const std::string& appname)
{
  static thread_lock lock;
  lock.lock();
  app_ftq_calendar*& cal = calendars_[aid];
  if (!cal){
    cal = new app_ftq_calendar(aid, appname, num_ticks_epoch_);
  }
  lock.unlock();
}

void
ftq_calendar::clear()
{
}

void
app_ftq_calendar::global_reduce(parallel_runtime* rt)
{

  //make a big buffer
  long my_num_epochs = max_epoch_;
  long max_num_epochs = rt->global_max(my_num_epochs);
  int num_keys = key::num_categories();
  long buffer_length = max_num_epochs * long(num_keys);

  long long* reduce_buffer = new long long[buffer_length];
  ::memset(reduce_buffer, 0, buffer_length * sizeof(long long));
  long long* bufptr = reduce_buffer;
  for (long i=0; i < my_num_epochs; ++i){
   ftq_epoch& my_epoch = epochs_[i];
   for (int k=0; k < num_keys; ++k, ++bufptr){
     *bufptr = my_epoch.event_time(k);
   }
  }

  int root = 0;
  rt->global_sum(reduce_buffer, buffer_length, root);

  rt->global_sum(aggregate_.totals_, num_keys, root);

  if (rt->me() != root){
    delete[] reduce_buffer;
    return;
  }

  //now loop back through
  allocate_epochs(max_num_epochs);

  bufptr = reduce_buffer;
  for (long i=0; i < max_num_epochs; ++i){
   ftq_epoch& my_epoch = epochs_[i];
   for (int k=0; k < num_keys; ++k, ++bufptr){
     my_epoch.set_event_time(k, *bufptr);
   }
  }

  delete[] reduce_buffer;

  max_epoch_ = max_num_epochs;
}

void
ftq_calendar::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;

  spkt_unordered_map<int, app_ftq_calendar*>::iterator it, end = calendars_.end();
  for (it=calendars_.begin(); it != end; ++it){
    app_ftq_calendar* cal = it->second;
    cal->global_reduce(rt);
  }
}

void
app_ftq_calendar::reduce(app_ftq_calendar* cal)
{
  /**
  int num_keys = key::num_categories();
  int num_epochs = cal->epochs_.size();

  allocate_epochs(num_epochs); //make sure we have enough

  for (int e=0; e < num_epochs; ++e){
    ftq_epoch& my_epoch = epochs_[e];
    ftq_epoch& his_epoch = cal->epochs_[e];
    for (int k=0; k < num_keys; ++k){
      my_epoch.collect(k, his_epoch.event_time(k));
    }
  }

  for (int k=0; k < num_keys; ++k){
    totals_[k] += cal->totals_[k];
  }
  */
}

void
ftq_calendar::reduce(stat_collector* coll)
{
  //nothing to do for now... we are using statics
  /**
  ftq_calendar* other = safe_cast(ftq_calendar, coll);
  spkt_unordered_map<int, app_ftq_calendar*>::iterator it, end = other->calendars_.end();
  for (it=other->calendars_.begin(); it!= end; ++it){
    int appnum = it->first;
    calendars_[appnum]->reduce(it->second);
  }
  */
}

void
ftq_calendar::dump_local_data()
{
}

void
ftq_calendar::dump_global_data()
{
  spkt_unordered_map<int, app_ftq_calendar*>::iterator it, end = calendars_.end();
  for (it=calendars_.begin(); it != end; ++it) {
    it->second->dump(fileroot_);
  }
}

void
ftq_calendar::simulation_finished(timestamp t_end)
{

}

void
ftq_calendar::collect(int event_typeid, int aid, int tid, long ticks_begin,
                      long num_ticks)
{
  static thread_lock lock;
  lock.lock();
  calendars_[aid]->collect(event_typeid, tid, ticks_begin, num_ticks);
  lock.unlock();
}

app_ftq_calendar*
ftq_calendar::get_calendar(int aid) const
{
  auto it = calendars_.find(aid);
  if (it == calendars_.end()) {
    spkt_throw_printf(sprockit::value_error, "no FTQ calendar found for app %d", aid);
  }
  return it->second;
}

app_ftq_calendar::app_ftq_calendar(int aid,
                                   const std::string& appname,
                                   long nticks_epoch)
  : epochs_(0),
    aid_(aid),
    max_tid_(0),
    max_epoch_(0),
    max_epoch_allocated_(0),
    num_ticks_epoch_(nticks_epoch),
    appname_(appname)
{
  int num_categories = key::num_categories();
  aggregate_.totals_ = new long long[num_categories];
  for (int i=0; i < num_categories; ++i) {
    aggregate_.totals_[i] = 0;
  }
}

app_ftq_calendar::~app_ftq_calendar()
{
  sprockit::delete_all(buffers_);
  delete[] aggregate_.totals_;
}

void
app_ftq_calendar::allocate_epochs(long max_epoch)
{
  while (max_epoch >= max_epoch_allocated_) {
    long epoch_start = max_epoch_allocated_;
    max_epoch_allocated_ += allocation_num_epochs;
    epochs_.resize(max_epoch_allocated_);

    int num_categories = key::num_categories();
    long long* buffer = new long long[num_categories * allocation_num_epochs];
    long long* buffer_ptr = buffer;
    for (long epoch=epoch_start; epoch < max_epoch_allocated_;
         buffer_ptr += num_categories, ++epoch) {
      epochs_[epoch].init(num_categories, buffer_ptr);
    }
    buffers_.push_back(buffer);
  }
}

void
app_ftq_calendar::collect(int event_typeid, int tid, long ticks_begin,
                          long num_ticks)
{
  /** aggregate for all time intervals and all threads for given event type */
  aggregate_.totals_[event_typeid] += num_ticks;
  long ticks_end = ticks_begin + num_ticks;
  long max_epoch = ticks_end / num_ticks_epoch_ +
                   1; //just always assume a remainder
  allocate_epochs(max_epoch);

  long first_epoch = ticks_begin / num_ticks_epoch_;
  long nticks_first_epoch = num_ticks_epoch_ - ticks_begin % num_ticks_epoch_;
  // this might not go until the end of the epoch
  nticks_first_epoch = std::min(nticks_first_epoch, num_ticks);
  /** aggregate for all threads for given time interval and event type */
  epochs_[first_epoch].collect(event_typeid, nticks_first_epoch);
  num_ticks -= nticks_first_epoch;
  //t_epoch[epoch].collect(event_typeid, nticks_next_epoch);
  for (long epoch=first_epoch+1; epoch <= max_epoch; ++epoch) {
    long nticks_this_epoch = std::min(num_ticks, num_ticks_epoch_);
    epochs_[epoch].collect(event_typeid, nticks_this_epoch);
    //t_epoch[epoch].collect(event_typeid, nticks_next_epoch);
    num_ticks -= nticks_this_epoch;
  }

  if (max_epoch > max_epoch_) {
    max_epoch_ = max_epoch;
  }

}


static const char* gnuplot_histogram_header =
  "set terminal postscript enhanced\n"
  "set border 3 front linetype -1 linewidth 1.000\n"
  "set boxwidth 0.75 absolute\n"
  "set style fill solid 1.00 border lt -1\n"
  "set grid nopolar\n"
  "set grid noxtics nomxtics ytics nomytics noztics nomztics nox2tics nomx2tics noy2tics nomy2tics nocbtics nomcbtics\n"
  "set grid layerdefault   linetype 0 linewidth 1.000,  linetype 0 linewidth 1.000\n"
  "set key outside right top vertical Left reverse noenhanced autotitles columnhead nobox\n"
  "set key invert samplen 4 spacing 1.5 width 0 height 0 font \"Arial,28\" \n"
  "set size 0.95,0.95\n"
  "set style histogram rowstacked title  offset character 0, 0, 0\n"
  "set style data histograms\n"
  "set style fill solid noborder\n"
  "set xtics border in scale 0,0 nomirror rotate by -45  offset character 0, 0, 0 autojustify\n"
  "set xtics norangelimit font \",8\"\n"
  "set xtics ()\n"
  "set xtics font \"Arial,24\" \n"
  "set noytics\n"
  "set title \"Application Activity Over Time\" font \"Arial,28\" \n"
  "set ylabel \"% of total\" font \"Arial,28\" \n"
  "set xlabel \"Time(ms)\" font \"Arial,28\" offset 0,-1\n"
  "set yrange [0.00000 : 100.000] noreverse nowriteback\n"
  "set boxwidth 1\n";

void
app_ftq_calendar::dumpi_gnuplot_histogram(const std::string& fileroot, int num_categories)
{
  int last_data_column = num_categories + 1;
  int totals_column = num_categories + 2;
  std::string fname = sprockit::printf("%s_app%d.p", fileroot.c_str(), aid_);
  std::ofstream out(fname.c_str());
  out << gnuplot_histogram_header;
  out << sprockit::printf("set xrange[0.0:%d.0]\n", max_epoch_);
  out << "plot 'ftq_app1.dat' using (100.*$2/$" << totals_column <<
      ") title column(2) lc 2 lw -1 lt -1";
  if (num_categories == 1) {
    return;
  }

  out << ", \\";
  out << "\n";

  out << sprockit::printf("   for [i=3:%d] '' using (100.*column(i)/column(%d)) title column(i) lc i lw -1 lt -1",
                   last_data_column, totals_column);
  out.close();
}

void
app_ftq_calendar::dump(const std::string& fileroot)
{
  int num_categories = key::num_categories();
  std::string fname = sprockit::printf("%s_app%d.dat", fileroot.c_str(), aid_);
  std::ofstream out(fname.c_str());
  //print the first line header
  out << sprockit::printf("%12s", "Epoch(us)");

  //sort the categories
  std::map<std::string, int> sorted_keys;
  for (int i=0; i < num_categories; ++i){
    sorted_keys[key::name(i)] = i;  
  }

  int nonzero_categories[num_categories];
  int num_nonzero_cats = 0;
  std::map<std::string,int>::iterator it, end = sorted_keys.end();
  for (it=sorted_keys.begin(); it != end; ++it){
    int i = it->second;
    std::string name = it->first;
    if (aggregate_.totals_[i] > 0) {
      out << sprockit::printf(" %12s", name.c_str());
      nonzero_categories[num_nonzero_cats] = i;
      ++num_nonzero_cats;
    }
  }

  out << sprockit::printf(" %12s", "Total");

  std::stringstream sstr;
  sstr << "\n";
  timestamp one_ms(1e-3);
  int64_t ticks_ms = one_ms.ticks_int64();
  for (int ep=0; ep < max_epoch_; ++ep) {
    //figure out how many us
    double num_ms = double(ep * num_ticks_epoch_) / (double) ticks_ms;
    sstr << sprockit::printf("%12.4f", num_ms);
    long total_ticks = 0;
    for (int i=0; i < num_nonzero_cats; ++i) {
      int cat_id = nonzero_categories[i];
      long num_ticks = epochs_[ep].event_time(cat_id);
      sstr << sprockit::printf(" %12ld", num_ticks);
      total_ticks += num_ticks;
    }
    sstr << sprockit::printf(" %12ld", total_ticks);
    sstr << "\n";
  }
  out << sstr.str();
  out.close();

  dumpi_gnuplot_histogram(fileroot, num_nonzero_cats);




  timestamp stamp_sec = timestamp(1);
  int64_t ticks_s = stamp_sec.ticks_int64();
  std::cout << sprockit::printf("Average time stats for application %s: \n",
                                     appname_.c_str());
  /** print the stat totals */
  int ntasks = max_tid_ + 1;

  end = sorted_keys.end();
  for (it=sorted_keys.begin(); it != end; ++it){
    int idx = it->second;
    std::string name = it->first;
    double av_per_app = (double) aggregate_.totals_[idx] / ntasks;
    double t_sec = av_per_app / (double) ticks_s;
    std::cout << sprockit::printf("%16s: %16.8f s\n", name.c_str(), t_sec);
  }
}

ftq_epoch::ftq_epoch()
  : totals_(nullptr)
{
}

ftq_epoch::~ftq_epoch()
{
}

void
ftq_epoch::init(int num_events, long long *buffer)
{
  totals_ = buffer;
  for (int i=0; i < num_events; ++i) {
    totals_[i] = 0;
  }
}


}
}