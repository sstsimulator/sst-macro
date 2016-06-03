cmake_minimum_required (VERSION 2.6)
#cmake_minimum_required (VERSION 2.8.8)
# FOR CMAKE VERSIONS 2.6 TO 2.8.7, MUST USE CRAPPY_OLD_CMAKE

project (SumiAPI)
# The version number.
set (SumiAPI_VERSION_MAJOR 1)
set (SumiAPI_VERSION_MINOR 0)

include_directories( "${CMAKE_SOURCE_DIR}" ${CMAKE_BINARY_DIR} )

ENABLE_TESTING()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR})
include(TestAgainstReference)

option(SPINLOCK "whether spin locks are supported" Off)
option(SST "whether to compile SST transport" Off)
option(MPI "whether to compile MPI transport" Off)
option(GNI "whether to compile GNI transport" Off)
option(NO_TRANSPORT "whether to skip compiling transport layer" Off)

if (NOT SST AND NOT MPI AND NOT GNI AND NOT NO_TRANSPORT)
message(FATAL_ERROR "You must choose SST, MPI, GNI, or NO_TRANSPORT")
endif()

add_subdirectory(sumi)

if (NOT NO_TRANSPORT)
add_subdirectory(fake)
add_subdirectory(benchmark)
endif()

add_subdirectory(test)

if (SPINLOCK)
set (sumi_use_spinlock 1)
else()
set (sumi_use_spinlock 0)
endif()

if (NO_TRANSPORT)
set (rdma_header_file_include "")
set (default_transport "nothing")
endif()

if (SST)
set (rdma_header_file_include "<sst/rdma.h>")
set (default_transport "sst")
add_subdirectory(sst)
endif()

if (MPI)
add_subdirectory(mpi)
set (rdma_header_file_include "<mpi/rdma.h>")
set (default_transport "mpi")
endif()

if (GNI)
add_subdirectory(gni)
set (rdma_header_file_include "<gni/rdma.h>")
set (default_transport "gni")
endif()

configure_file(${CMAKE_SOURCE_DIR}/config.h.in ${CMAKE_BINARY_DIR}/sumi/config.h)


