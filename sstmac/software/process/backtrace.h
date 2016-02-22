#ifndef sstmac_software_process_backtrace_h
#define sstmac_software_process_backtrace_h

#include <sstmac/common/sstmac_config.h>
#include <sstmac/software/process/graphviz.h>

#if SSTMAC_HAVE_GRAPHVIZ
#define SSTMACBacktrace GraphVizAppendBacktrace
#else
#define SSTMACBacktrace GraphVizDoNothing
#endif

#endif
