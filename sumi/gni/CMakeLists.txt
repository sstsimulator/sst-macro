set (gni_HEADERS
gni_transport.h
rdma.h
)

set (gni_SOURCES 
gni_transport.cc
cdm.cc
cq.cc
ep.cc
finalize.cc
gather.cc
init.cc
mem.cc
progress.cc
rdma_poll.cc
rdma_get.cc
rdma_post.cc
rdma_put.cc
smsg.cc
smsg_poll.cc
smsg_recv.cc
smsg_send.cc
util.cc
)

if (FAKEGNI)
set (gni_SOURCES "${gni_SOURCES}" fakegni.cc)
endif()

include_directories( "${CMAKE_SOURCE_DIR}" )

if (CRAPPY_OLD_CMAKE)
add_library( sumi_gni SHARED ${gni_SOURCES} ${gni_HEADERS} )
else()
add_library( gni OBJECT ${gni_SOURCES} ${gni_HEADERS} )
endif()

install (FILES rdma.h DESTINATION include/gni)

