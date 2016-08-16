#ifndef PERF_COUNTER_H
#define PERF_COUNTER_H

#include <sprockit/factories/factory.h>
#include <sstmac/software/libraries/compute/compute_event_fwd.h>

namespace sstmac {
namespace sw {

class perf_counter
{
 public:
  virtual ~perf_counter() = default;
};

class perf_counter_model :
 public sprockit::factory_type
{
 public:
  virtual ~perf_counter_model(){}

  virtual compute_event*
  get_next_event() = 0;

  virtual perf_counter*
  register_variable(void* ptr) = 0;

  virtual void
  remove_variable(void* ptr) = 0;

};

DeclareFactory(perf_counter_model)

template <class T>
class perf_counter_impl :
 public perf_counter
{
 public:
  T& counters() {
    return counters_;
  }

 private:
  T counters_;
};

}
}

#endif // PERF_COUNTER_H
