#include <sumi/thread_safe_set.h>

namespace sumi
{

void
set_difference(
    const thread_safe_set<int>& base,
    const thread_safe_set<int>& subtract,
    thread_safe_set<int>& result)
{
  typedef thread_safe_set<int>::const_iterator it_t;

  it_t base_end = base.start_iteration();
  it_t subtract_end = subtract.start_iteration();
  it_t result_end = result.start_iteration();

  std::set_difference(base.begin(), base_end,
    subtract.begin(), subtract_end,
    std::inserter(result.set_, result_end));

  result.end_iteration();
  subtract.end_iteration();
  base.end_iteration();
}

void
set_difference(
    const thread_safe_set<int>& base,
    const thread_safe_set<int>& subtract,
    std::set<int>& result)
{
  typedef thread_safe_set<int>::const_iterator it_t;

  it_t base_end = base.start_iteration();
  it_t subtract_end = subtract.start_iteration();

  std::set_difference(base.begin(), base_end,
    subtract.begin(), subtract_end,
    std::inserter(result, result.end()));

  subtract.end_iteration();
  base.end_iteration();
}

}
