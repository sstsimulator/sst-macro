
namespace ns {
 namespace ns2 {
  int x = 10;
 }
}

namespace ns {
int x = 10;
}

void fxn()
{
  ns::x += 5;
  ns::ns2::x -= 3;
}

