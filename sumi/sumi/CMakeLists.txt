set (sumi_api_HEADERS
lockable.h
thread_lock.h
thread_safe_int.h
thread_safe_list.h
thread_safe_ptr_type.h
thread_safe_refcount_ptr.h
thread_safe_set.h
)

set (sumi_api_SOURCES 
thread_lock.cc
thread_safe_set.cc
)

if (NOT NO_TRANSPORT)

set (sumi_api_HEADERS
${sumi_api_HEADERS}
active_msg_transport.h
allgather.h
allreduce.h
bcast.h 
collective.h
collective_actor.h
collective_message.h
collective_message_fwd.h
comm_functions.h
dense_rank_map.h
domain.h
communicator_fwd.h
dynamic_tree_vote.h
lockable.h
message.h
monitor.h
options.h
partner_timeout.h
ping.h
rdma.h
rdma_mdata.h
timeout.h
transport.h
transport_fwd.h
)

set (sumi_api_SOURCES 
${sumi_api_SOURCES}
active_msg_transport.cc
allgather.cc
allreduce.cc
bcast.cc
collective.cc
collective_actor.cc
collective_message.cc
dense_rank_map.cc
domain.cc
dynamic_tree_vote.cc
message.cc
monitor.cc
partner_timeout.cc
ping.cc
rdma.cc
transport.cc
)

endif()

if (CRAPPY_OLD_CMAKE)
add_library(sumi_api SHARED ${sumi_api_SOURCES} ${sumi_api_HEADERS})

target_link_libraries( sumi_api sumi_gni )
else()
add_library(core OBJECT ${sumi_api_SOURCES} ${sumi_api_HEADERS} )

install (FILES ${sumi_api_HEADERS} ${CMAKE_BINARY_DIR}/sumi/config.h DESTINATION include/sumi)

set(extra_targets $<TARGET_OBJECTS:core>)
set(extra_libs sprockit)

if (NOT NO_TRANSPORT)
set(extra_targets ${extra_targets} $<TARGET_OBJECTS:fake>)
endif()

if (SST)
set(extra_targets ${extra_targets} $<TARGET_OBJECTS:sst>)
set(extra_libs ${extra_libs} sstmac)
endif()

if (MPI)
set(extra_targets ${extra_targets} $<TARGET_OBJECTS:mpi>)
endif()

if (GNI)
set(extra_targets ${extra_targets} $<TARGET_OBJECTS:gni>)
endif()

add_library(sumi_api SHARED ${extra_targets})

target_link_libraries( sumi_api ${extra_libs} )
endif()

install (TARGETS sumi_api DESTINATION lib)
