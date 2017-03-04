/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include "callbase.h"
#include "otf2_trace_replay.h"

/******************************************************************************
 *  Classes that derive from CallBase
 */

// TIME_WRAPPER() compacts redundant time-keeping steps
#if 0
#define TIME_WRAPPER(...) \
	cout << "START " << GetStart() << endl; \
	cout << "END " << GetEnd() << endl; \
    app->start_mpi(GetStart()); \
    __VA_ARGS__; \
    app->end_mpi(GetEnd());
#else
#define TIME_WRAPPER(...) \
    app->start_mpi(GetStart()); \
    __VA_ARGS__; \
    app->end_mpi(GetEnd());
#endif

void MpiInitCall::Trigger() {
    TIME_WRAPPER(app->get_mpi()->do_init(const_cast<int*>(&argc), const_cast<char***>(&argv)));
}

/*
 Problem; There is no MPI wait callback, only a generic MPI_Wait enter event.
 The trace replay needs to associate MPI wait/waitall with request handles. The
 solution is to grab the last Wait event in the queue and assign the every
 incoming request.

ENTER                                      1     3121396012661832  Region: "MPI_Irecv" <179>
MPI_IRECV_REQUEST                          1     3121396012692014  Request: 1
LEAVE                                      1     3121396012750548  Region: "MPI_Irecv" <179>
ENTER                                      1     3121396012754602  Region: "MPI_Isend" <188>
MPI_ISEND                                  1     3121396012759130  Receiver: 0 ("Master thread" <0>), Communicator: "MPI_COMM_WORLD" <0>, Tag: 123, Length: 40, Request: 2
LEAVE                                      1     3121396012780256  Region: "MPI_Isend" <188>
ENTER                                      1     3121396012789141  Region: "MPI_Wait" <233>
MPI_IRECV                                  1     3121396012811793  Sender: 0 ("Master thread" <0>), Communicator: "MPI_COMM_WORLD" <0>, Tag: 123, Length: 40, Request: 1
LEAVE                                      1     3121396012835188  Region: "MPI_Wait" <233>
ENTER                                      1     3121396012836552  Region: "MPI_Wait" <233>
MPI_ISEND_COMPLETE                         1     3121396012841752  Request: 2

*/
//void MpiIsendCall::Trigger() {
//    //throw std::runtime_error("MPI requests have not been implemented");
//    MPI_Request req; // not stored
//    TIME_WRAPPER(app->get_mpi()->isend(NULL, count, MPI_CHAR, dest, tag, comm, &req));
//    app->add_request(request, req);
//}
//
//void MpiIrecvCall::Trigger() {
//    //throw std::runtime_error("MPI requests have not been implemented");
//    MPI_Request req; // not stored
//    TIME_WRAPPER(app->get_mpi()->irecv(NULL, count, MPI_CHAR, source, tag, comm, &req));
//    app->add_request(request, req);
//}

void MpiSendCall::Trigger() {
	app->start_mpi(GetStart());
    TIME_WRAPPER(app->get_mpi()->send(NULL, count, MPI_CHAR, dest, tag, comm));
}

void MpiRecvCall::Trigger() {
    TIME_WRAPPER(app->get_mpi()->recv(NULL, count, MPI_CHAR, source, tag, comm, MPI_STATUS_IGNORE));
}

void MpiFinalizeCall::Trigger() {
    TIME_WRAPPER(app->get_mpi()->do_finalize())
}
