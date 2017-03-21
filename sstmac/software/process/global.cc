#include <sstmac/software/process/global.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/thread.h>

int sstmac_global_stacksize = 0;

namespace sstmac {

int GlobalVariable::stackOffset = 0;
char GlobalVariable::globalInits[SSTMAC_MAX_GLOBALS];

GlobalVariable::GlobalVariable(int &offset, const int size, const void *initData)
{
  const_cast<int&>(offset) = stackOffset;

  int rem = size % 4;
  int offsetIncrement = rem ? (size + (4-rem)) : size; //align on 32-bits

  if (initData){
    void* initStart = (char*)globalInits + stackOffset;
    ::memcpy(initStart, initData, size);
  }

  stackOffset += offsetIncrement;
}

namespace sw {

}

}
