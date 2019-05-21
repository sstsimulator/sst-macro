cmake_minimum_required (VERSION 2.6)
project (test_cmake)

include(CheckSymbolExists)

try_run(MPI_RUN MPI_COMPILE ${CMAKE_CURRENT_BINARY_DIR}/mpi-bin ${CMAKE_CURRENT_SOURCE_DIR}/mpi-src/mpi.cc)
if (MPI_RUN STREQUAL "FAILED_TO_RUN")
  message(FATAL_ERROR "mpi required")
endif ()

try_run(PTHREAD_RUN PTHREAD_COMPILE ${CMAKE_CURRENT_BINARY_DIR}/pthread-bin ${CMAKE_CURRENT_SOURCE_DIR}/pthread-src/pthread.cc)
if (PTHREAD_RUN STREQUAL "FAILED_TO_RUN")
  message(FATAL_ERROR "pthreads required")
endif ()

add_library(compute compute.cc)
add_executable (test_cmake main.cc)
target_link_libraries (test_cmake compute)
