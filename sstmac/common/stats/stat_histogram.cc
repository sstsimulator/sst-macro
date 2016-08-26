#include <sstmac/backends/common/parallel_runtime.h>
#include <sstmac/common/stats/stat_histogram.h>
#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <math.h>

namespace sstmac {

SpktRegister("histogram", stat_collector, stat_histogram);
SpktRegister("time_histogram", stat_collector, stat_time_histogram);

stat_histogram::stat_histogram() :
    bin_size_(0),
    max_bin_(-1),
    is_log_(false)
{
}

void
stat_histogram::global_reduce(parallel_runtime *rt)
{
  if (rt->nproc() == 1)
    return;

  int root = 0;
  int my_num_bins = counts_.size();
  int num_bins = rt->global_max(my_num_bins);
  counts_.resize(num_bins);
  rt->global_sum(&counts_[0], num_bins, root);
}

void
stat_histogram::reduce(stat_collector *coll)
{
  stat_histogram* other = safe_cast(stat_histogram, coll);

  int max_num = std::max(counts_.size(), other->counts_.size());
  if (max_num > counts_.size()){
    counts_.resize(max_num);
  }

  int num_bins = other->counts_.size();
  for (int i=0; i < num_bins; ++i){
    counts_[i] += other->counts_[i];
  }
}

void
stat_histogram::dump(const std::string& froot)
{
  std::string data_file = froot + ".dat";
  std::fstream data_str;
  check_open(data_str, data_file);
  data_str << "Bin Count\n";
  for (int i=0; i < counts_.size(); ++i){
    double size  = (i+0.5) * bin_size_;
    data_str << sprockit::printf("%12.8f", size) << " " << counts_[i] << "\n";
  }
  data_str.close();

  std::string gnuplot_file = froot + ".p";
  std::fstream gnuplot_str;
  check_open(gnuplot_str, gnuplot_file);

  gnuplot_str << "reset\n";
  gnuplot_str << "set term png truecolor\n";
  gnuplot_str << "set output \"" << froot << ".png\"\n";
  gnuplot_str << "set xlabel \"" << (is_log_ ? "log(Size)" : "Size") << "\"\n";
  gnuplot_str << "set ylabel \"Count\"\n";
  gnuplot_str << "set boxwidth 0.95 relative\n";
  gnuplot_str << "set style fill transparent solid 0.5 noborder\n";
  gnuplot_str << "plot \"" << data_file << "\"" << " u 1:2 w boxes notitle\n";
  gnuplot_str.close();
}

void
stat_histogram::dump_global_data()
{
  dump(fileroot_);
}

void
stat_histogram::dump_local_data()
{
  std::string fname = sprockit::printf("%s.%d", fileroot_.c_str(), id_);
  dump(fname);
}

void
stat_histogram::clear()
{
}

void
stat_histogram::collect(double value)
{
  collect(value, 1);
}

void
stat_histogram::clone_into(stat_histogram* hist) const {
  hist->bin_size_ = bin_size_;
  hist->is_log_ = is_log_;
  hist->counts_.reserve(counts_.size());
  stat_collector::clone_into(hist);
}

void
stat_histogram::collect(double value, int64_t count)
{
  value = is_log_ ? log10(value) : value;
  long bin = value / bin_size_;
  if (bin > max_bin_){
    max_bin_ = bin;
    counts_.resize(max_bin_+1);
  }
  counts_[bin] += count;
}

void
stat_histogram::init_factory_params(sprockit::sim_parameters *params)
{
  stat_collector::init_factory_params(params);
  bin_size_ = params->get_quantity("bin_size");
  int num_bins_guess = params->get_optional_int_param("num_bins", 20);
  is_log_ = params->get_optional_bool_param("logarithmic", false);
  counts_.reserve(num_bins_guess);
}

void
stat_histogram::simulation_finished(timestamp end)
{
}

void
stat_time_histogram::record(timestamp t, int64_t num)
{
  stat_histogram::collect(t.sec(), num);
}




}
