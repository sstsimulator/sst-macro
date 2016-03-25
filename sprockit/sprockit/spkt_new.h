#ifndef sprockit_common_custom_new_h
#define sprockit_common_custom_new_h
#ifdef __cplusplus

#include <cstring>
#include <new>
#include <sprockit/sim_parameters_fwd.h>

namespace sprockit {

void sprockit_init_cxx_heap(sim_parameters* params);

void sprockit_finalize_cxx_heap();

void sprockit_init_allocator_chunks();

}


#endif
#endif


