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
  virtual public sprockit::factory_type,
  public library
{
 public:
  api(const key::category& ty) :
    library() {
    startcount_ = 0;
    endcount_ = 0;
    key_cat_ = ty;
  }

  api() {
    startcount_ = 0;
    endcount_ = 0;
  }

  virtual
  ~api() {
    if (hostcompute_) {
      delete timer_;
    }
  }

  virtual void
  init_os(operating_system* os);

  virtual void
  init_param1(const software_id& sid);

  timestamp
  now() const;

  void
  schedule(timestamp t, event* ev);

  void
  schedule_delay(timestamp t, event* ev);

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

  //these can be used for direct execution compute modeling
  virtual void
  start_api_call();

  virtual void
  end_api_call();

 protected:
  static bool hostcompute_;
  Timer* timer_;
  long startcount_;
  long endcount_;
  lib_compute_time* compute_;
  software_id sid_;

};
DeclareFactory1InitParam(api,software_id)
;

}
}

#endif // API_H

