#ifndef sstmac_software_libraries_API_H
#define sstmac_software_libraries_API_H

#include <sprockit/factories/factory.h>
#include <sstmac/software/libraries/library.h>
#include <sstmac/software/process/key.h>
#include <sstmac/common/sst_event_fwd.h>

#include <sstmac/software/libraries/compute/lib_compute_time.h>

# if  (defined(__MACH__) && defined(__APPLE__))
#   define _MAC
#endif

# if defined(_MAC)
#    include <mach/mach_time.h>
# else
#    include <time.h>
# endif

#if defined(_MAC)
typedef uint64_t my_timer_t;
typedef double timer_c;
#else
typedef double my_timer_t;
typedef timespec timer_c;
#endif

namespace sstmac {
namespace sw {

//==============================================================================
// Timer
// A quick class to do benchmarking.
// Example: Timer t;  t.tic();  SomeSlowOp(); t.toc("Some Message");

class Timer
{
 public:
  Timer();

  inline void
  tic();
  inline void
  toc();

  void
  reset();
  double
  getTime();

 private:
  my_timer_t start;
  double duration;
  timer_c ts;
  double conv_factor;
  double elapsed_time;
};

class api :
  public library
{
 public:
  api(sprockit::sim_parameters* params,
      const char* prefix,
      software_id sid,
      operating_system* os,
      const key::category& ty) :
    api(params, prefix, sid, os)
  {
    key_cat_ = ty;
  }

  api(sprockit::sim_parameters *params,
      const std::string& libname,
      software_id sid,
      operating_system *os) :
    library(libname, sid, os),
    startcount_(0),
    endcount_(0)
  {
    init(params);
  }

  api(sprockit::sim_parameters* params,
      const char* prefix,
      software_id sid,
      operating_system* os) :
    api(params, standard_lib_name(prefix, sid), sid, os)
  {
  }

  virtual ~api();

  virtual void
  init(){}

  virtual void
  finalize(){}

  timestamp
  now() const;

  void
  schedule(timestamp t, event_queue_entry* ev);

  void
  schedule_delay(timestamp t, event_queue_entry* ev);

  //these can be used for direct execution compute modeling
  virtual void
  start_api_call();

  virtual void
  end_api_call();

 protected:
  bool hostcompute_;
  Timer* timer_;
  long startcount_;
  long endcount_;
  lib_compute_time* compute_;

 private:
  void init(sprockit::sim_parameters *params);
};

void api_lock();
void api_unlock();

#define ImplementAPI(x) \
 public: \
  static const char* api_name;

#define RegisterAPI(name, child_cls) \
  SpktRegister(name, sstmac::sw::api, child_cls); \
  const char* child_cls::api_name = name

DeclareFactory(api,software_id,operating_system*);

}
}

#endif // API_H

