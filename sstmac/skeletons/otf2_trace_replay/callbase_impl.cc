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
//#if 0
//#define TIME_WRAPPER(...) \
//	cout << "START " << GetStart() << endl; \
//	cout << "END " << GetEnd() << endl; \
//    app->start_mpi(GetStart()); \
//    __VA_ARGS__; \
//    app->end_mpi(GetEnd());
//#else
//#define TIME_WRAPPER(...) \
//    app->start_mpi(GetStart()); \
//    __VA_ARGS__; \
//    app->end_mpi(GetEnd());
//#endif
//
//void MpiInitCall::Trigger() {
//    TIME_WRAPPER(app->get_mpi()->do_init(const_cast<int*>(&argc), const_cast<char***>(&argv)));
//}
//
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
//
//void MpiWaitCall::Trigger() {
//	MPI_Status s;
//	MPI_Request req = app->get_request(request);
//
//	TIME_WRAPPER(app->get_mpi()->wait(&req, &s));
//}
//
//void MpiSendCall::Trigger() {
//	app->start_mpi(GetStart());
//    TIME_WRAPPER(app->get_mpi()->send(NULL, count, MPI_CHAR, dest, tag, comm));
//}
//
//void MpiRecvCall::Trigger() {
//    TIME_WRAPPER(app->get_mpi()->recv(NULL, count, MPI_CHAR, source, tag, comm, MPI_STATUS_IGNORE));
//}
//
//void MpiFinalizeCall::Trigger() {
//    TIME_WRAPPER(app->get_mpi()->do_finalize())
//}
