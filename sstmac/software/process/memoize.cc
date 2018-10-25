#include <sstmac/software/process/memoize.h>
#include <sstmac/software/process/operating_system.h>


namespace sstmac {

Memoization::Memoization(const char* name, const char* model)
{
  sstmac::sw::operating_system::add_memoization(name, model);
}

}


