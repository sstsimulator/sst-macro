#include <sstmac/software/process/memoize.h>
#include <sstmac/software/process/operating_system.h>


namespace sstmac {

Memoization::Memoization(const char* name, const char* model)
{
  sstmac::sw::OperatingSystem::addMemoization(name, model);
}

}


