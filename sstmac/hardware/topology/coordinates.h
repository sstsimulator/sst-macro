#ifndef COORDINATES_H
#define COORDINATES_H

#include <vector>
#include <string>

namespace sstmac {
namespace hw {

class coordinates :
  public std::vector<int>
{
 public:
  coordinates(const std::vector<int>& vec);

  coordinates();

  coordinates(int nelem);

  std::string
  to_string() const;

};

}
}

#endif // COORDINATES_H

