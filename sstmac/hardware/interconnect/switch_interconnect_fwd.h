#ifndef Switch_INTERCONNECT_FWD_H
#define Switch_INTERCONNECT_FWD_H

namespace sstmac {
namespace hw {

#if SSTMAC_INTEGRATED_SST_CORE
class sst_switch_interconnect;
typedef sst_switch_interconnect switch_interconnect;
#else
class macro_switch_interconnect;
typedef macro_switch_interconnect switch_interconnect;
#endif

}
}

#endif // INTERCONNECT_FWD_H

