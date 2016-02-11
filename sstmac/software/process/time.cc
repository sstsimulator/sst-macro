#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/time.h>

using sstmac::sw::operating_system;

extern "C"
int SSTMAC_gettimeofday(struct timeval* tv, struct timezone* tz)
{
  operating_system* os = operating_system::current_os();
  double total = os->now().sec();
  int secs = int(total);
  int usecs = int((total-secs)*1e6);
  tv->tv_sec = secs;
  tv->tv_usec = usecs;
  return 0;
}

//make sure this doesn't get overwritten
#undef gettimeofday
extern "C"
double sstmac_wall_time()
{
  timeval t_st;
  gettimeofday(&t_st, 0);
  double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return t;
}
