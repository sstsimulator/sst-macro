#include <sstmac/software/ami/ami.h>
#include <sprockit/errors.h>
#include <sstmac/common/messages/sst_message.h>

namespace sstmac {
namespace ami {


#define enumcase(x) case x: return #x

const char*
tostr(CAT cat)
{
  switch(cat) {
      enumcase(COMPUTE);
      enumcase(COMM);
      enumcase(DISP);
      enumcase(QUERY);
      enumcase(HW);
  }
  spkt_throw_printf(sprockit::illformed_error,
                    "Invalid AMI_CAT %d received", cat);
}

const char*
tostr(COMM_FUNC func)
{
  switch(func) {
    enumcase(COMM_SEND);
  }
  spkt_throw_printf(sprockit::illformed_error,
                    "Invalid AMI_COMM_FUNC %d received", func);
}

const char*
tostr(SERVICE_FUNC func)
{
  switch(func) {
    enumcase(COMP_DISKACCESS);
  }
  spkt_throw_printf(sprockit::illformed_error,
                    "Invalid AMI_COMP_FUNC %d received", func);
  return 0;
}

const char*
tostr(COMP_FUNC func)
{
  switch(func) {
    enumcase(COMP_TIME);
    enumcase(COMP_INSTR);
    enumcase(COMP_EIGER);
  }
  spkt_throw_printf(sprockit::illformed_error,
                    "Invalid AMI_COMP_FUNC %d received", func);
  return 0;
}

}
}

