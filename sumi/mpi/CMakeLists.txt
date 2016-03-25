set (mpi_HEADERS
mpi_transport.h
rdma.h
)

set (mpi_SOURCES 
mpi_transport.cc
)

include_directories( "${CMAKE_SOURCE_DIR}" )
add_library( mpi OBJECT ${mpi_SOURCES} ${mpi_HEADERS} )


install (FILES rdma.h DESTINATION include/mpi)
