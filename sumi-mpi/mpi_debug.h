#ifndef MPI_DEBUG_H
#define MPI_DEBUG_H

#include <sprockit/debug.h>

DeclareDebugSlot(mpi)
DeclareDebugSlot(mpi_request)
DeclareDebugSlot(mpi_server)
DeclareDebugSlot(mpi_queue)
DeclareDebugSlot(mpi_pt2pt)
DeclareDebugSlot(mpi_collective)

// VA_ARGS[0] = format_str
#define mpi_debug(rank, flags, ...) \
  debug_printf(flags, "MPI Rank %4d: %s", int(rank), sprockit::printf(__VA_ARGS__).c_str())

#endif // MPI_DEBUG_H
