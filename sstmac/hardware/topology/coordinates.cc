#include <sstmac/hardware/topology/coordinates.h>

#include <sstream>

namespace sstmac {
namespace hw {

coordinates::coordinates()
{
}

coordinates::coordinates(int nelem)
  : std::vector<int>(nelem, 0)
{
}

coordinates::coordinates(const std::vector<int> &vec)
  : std::vector<int>(vec)
{
}

std::string
coordinates::to_string() const
{
  std::stringstream sstr;
  sstr << "[";
  for (int i=0; i < size(); ++i) {
    sstr << " " << at(i);
  }
  sstr << " ]";
  return sstr.str();
}

}
}


