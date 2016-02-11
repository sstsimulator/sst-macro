#include <sstmac/libraries/mpi/mpi_debug.h>

RegisterDebugSlot(mpi,
    "print all the basic operations that occur on each rank - only API calls are logged, not any implementation details")
RegisterDebugSlot(mpi_server,
    "print all the basic operations that occur on each MPI server "
    "- each physical node shares a single MPI server across co-located MPI ranks")
RegisterDebugSlot(mpi_request,
    "print all basic information regarding the creation/completion of MPI_Requests")
RegisterDebugSlot(mpi_queue,
    "print details about the operations that occur inside the mpi queue that manages ingoing/outgoing messages"
    " - there is a unique queue object for each MPI rank")
RegisterDebugSlot(mpi_pt2pt,
    "print information about MPI point-to-point calls as well as implementation details")
RegisterDebugSlot(mpi_collective,
    "print information about MPI collective calls as well as implementation details")
