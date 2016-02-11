/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009-2011 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/libraries/mpi/sstmac_mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <sstmac/common/timestamp.h>
#include <sprockit/errors.h>

extern "C" void
mpi_errhandler_set_(int &comm, int &handle, int &ierr)
{

}

extern "C" void
mpi_error_string_(int &ierr, char* string, int &len, int &ierror)
{

}

extern "C" void
mpi_init_(int &err)
{
  err = MPI_Init(0, 0);
}

extern "C" void
mpi_init_thread_(int &err)
{
  err = MPI_Init(0, 0);
}

extern "C" void
mpi_finalize_(int &err)
{
  err = MPI_Finalize();
}

extern "C" void
mpi_initialized_(int &flag, int &err)
{
  err = MPI_Initialized(&flag);
}

extern "C" int
mpi_comm_world_()
{
  return (int) MPI_COMM_WORLD;
}

extern "C" void
mpi_abort_(int &comm, int &code, int &err)
{
  printf("MPI_Abort was called with code %i", code);
  exit(1);
}

extern "C" void
mpi_comm_rank_(int &comm, int &rank, int &err)
{
  err = MPI_Comm_rank(comm, &rank);
}

extern "C" void
mpi_comm_size_(int &comm, int &size, int &err)
{
  err = MPI_Comm_size(comm, &size);
}

extern "C" void
mpi_attr_get1_(int &comm, int &keyval, int& attr_value, int& flag, int&err)
{
  err = MPI_Attr_get(comm, keyval, &attr_value, &flag);
}

extern "C" void
mpi_attr_get2_(int &comm, int &keyval, int& attr_value, bool& flag, int&err)
{
  int f;
  err = MPI_Attr_get(comm, keyval, &attr_value, &f);
  flag = f;
}

// ----------------------------------------------------------------------------------
// --   MPI_Send
// -----------------------------------------------------------------------------------

extern "C" void
mpi_send0_( int &count, int &datatype, int &dest, int &tag, int &comm,
            int &err)
{
  MPI_Print("MPI_Send: no buffer");

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(NULL, count, d, dest, tag, c);
}

extern "C" void
mpi_send1_(int &buf, int &count, int &datatype, int &dest, int &tag, int &comm,
           int &err)
{
  MPI_Print("MPI_Send: integer buffer");
  if (count > 1) {
    printf("fortran mpi_send: buffers not yet supported \n");
    exit(1);
  }
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(&buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send2_(int *buf, int &count, int &datatype, int &dest, int &tag, int &comm,
           int &err)
{
  MPI_Print("MPI_Send: integer array buffer");

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send3_(float &buf, int &count, int &datatype, int &dest, int &tag,
           int &comm, int &err)
{
  MPI_Print("MPI_Send: real buffer");

  if (count > 1) {
    printf("fortran mpi_send: buffers not yet supported \n");
    exit(1);
  }
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(&buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send4_(float *buf, int &count, int &datatype, int &dest, int &tag,
           int &comm, int &err)
{
  MPI_Print("MPI_Send: real array buffer");

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send5_(double &buf, int &count, int &datatype, int &dest, int &tag,
           int &comm, int &err)
{
  MPI_Print("MPI_Send: double buffer");
  if (count > 1) {
    printf("fortran mpi_send: buffers not yet supported \n");
    exit(1);
  }

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(&buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send6_(double *buf, int &count, int &datatype, int &dest, int &tag,
           int &comm, int &err)
{
  MPI_Print("MPI_Send: double array buffer");

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(buf, count, d, dest, tag, c);
}

extern "C" void
mpi_send7_(int *buf, int &count, int &datatype, int &dest, int &tag, int &comm,
           int &err)
{

  throw sprockit::spkt_error(
    "sstmac_mpi_f: mpi_send7 -- multi-dimensional array not safe yet");
  MPI_Print("MPI_Send: integer multi-dimensional array buffer");

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Send(buf, count, d, dest, tag, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Recv
// -----------------------------------------------------------------------------------

extern "C" void
mpi_recv0_( int &count, int &datatype, int &source, int &tag,
            int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: no buffer");

  MPI_Status cstat;



  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(NULL, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv1_(int &buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: integer buffer");

  MPI_Status cstat;

  if (count > 1) {
    printf("fortran mpi_recv: buffers not yet supported \n");
    exit(1);
  }

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(&buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv2_(int *buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: integer array buffer");

  MPI_Status cstat;

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv3_(float &buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: real buffer");

  MPI_Status cstat;

  if (count > 1) {
    printf("fortran mpi_recv: buffers not yet supported \n");
    exit(1);
  }
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(&buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv4_(float *buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: real array buffer");

  MPI_Status cstat;

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv5_(double &buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: double buffer");

  MPI_Status cstat;

  if (count > 1) {
    printf("fortran mpi_recv: buffers not yet supported \n");
    exit(1);
  }
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(&buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_recv6_(double *buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  MPI_Print("MPI_Recv: double array buffer");

  MPI_Status cstat;

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" int
mpi_recv7_(int *buf, int &count, int &datatype, int &source, int &tag,
           int &comm, int *status, int &err)
{
  throw sprockit::spkt_error(
    "sstmac_mpi_f: mpi_recv7 -- multi-dimensional array not safe yet");
  MPI_Print("MPI_Recv: integer multi-dimensional array buffer");

  MPI_Status cstat;

  MPI_Comm c = comm;
  MPI_Datatype d = datatype;

  err = MPI_Recv(buf, count, d, source, tag, c, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

// ----------------------------------------------------------------------------------
// --   MPI_Wait
// -----------------------------------------------------------------------------------

extern "C" void
mpi_wait1_(int &request, int *status, int &err)
{
  MPI_Status cstat;
  MPI_Request req = request;
  err = MPI_Wait(&req, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;

}

extern "C" void
mpi_wait2_(MPI_Request &request, int *status, int &err)
{
  MPI_Status cstat;

  err = MPI_Wait(&request, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;

}

// ----------------------------------------------------------------------------------
// --   MPI_Waitany
// -----------------------------------------------------------------------------------


extern "C" void
mpi_waitany1_(int &count, int* array_of_requests, int &index, int *status,
              int& err)
{
  MPI_Status cstat;
  MPI_Request arr[count];
  for (int i = 0; i < count; i++) {
    arr[i] = (MPI_Request) array_of_requests[i]; //convert to MPI_Request (long)
  }
  int ind;

  err = MPI_Waitany(count, arr, &ind, &cstat);
  index = ind + 1;
  for (int i = 0; i < count; i++) {
    array_of_requests[i] = arr[i];
  }
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_waitany2_(int &count, MPI_Request* array_of_requests, int &index,
              int *status, int& err)
{
  MPI_Status cstat;
  int ind;
  err = MPI_Waitany(count, array_of_requests, &ind, &cstat);
  index = ind + 1;
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

// ----------------------------------------------------------------------------------
// --   MPI_Waitsome
// -----------------------------------------------------------------------------------


extern "C" void
mpi_waitsome_(int &incount, int array_of_requests[], int &outcount,
              int array_of_indices[], int* array_of_statuses[], int &err)
{
  MPI_Status *cstat;

  MPI_Request arr[incount];
  for (int i = 0; i < incount; i++) {
    arr[i] = array_of_requests[i]; //convert to MPI_Request (long)
  }

  err = MPI_Waitsome(incount, arr, &outcount, array_of_indices, cstat);
  for (int i = 0; i < outcount; i++) {
    array_of_statuses[i][0] = cstat[i].MPI_SOURCE;
    array_of_statuses[i][1] = cstat[i].MPI_TAG;
    array_of_statuses[i][2] = cstat[i].MPI_ERROR;
    array_of_statuses[i][3] = cstat[i].BYTES_RECV;
  }
}

// ----------------------------------------------------------------------------------
// --   MPI_Get_count
// -----------------------------------------------------------------------------------


extern "C" void
mpi_get_count_(int *status, int &datatype, int &count, int &err)
{
  MPI_Status cstat;
  err = MPI_Get_count(&cstat, datatype, &count);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

// ----------------------------------------------------------------------------------
// --   MPI_Test
// -----------------------------------------------------------------------------------

extern "C" void
mpi_test_(int &request, int &flag, int *status, int &err)
{
  MPI_Status cstat;
  MPI_Request req = request;
  err = MPI_Test(&req, &flag, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

// ----------------------------------------------------------------------------------
// --   MPI_Cancel
// -----------------------------------------------------------------------------------


extern "C" void
mpi_cancel_(int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Cancel(&req);
}

// ----------------------------------------------------------------------------------
// --   MPI_Request_free
// -----------------------------------------------------------------------------------


extern "C" void
mpi_request_free_(int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Request_free(&req);
}

// ----------------------------------------------------------------------------------
// --   MPI_Comm_dup
// -----------------------------------------------------------------------------------


extern "C" void
mpi_comm_dup_(int &comm, int &newcomm, int &err)
{
  MPI_Comm res;
  err = MPI_Comm_dup(comm, &res);
  newcomm = res;
}

// ----------------------------------------------------------------------------------
// --   MPI_Comm_split
// -----------------------------------------------------------------------------------

extern "C" void
mpi_comm_split_(int &comm, int &color, int &key, int &newcomm, int err)
{
  MPI_Comm res;
  err = MPI_Comm_split(comm, color, key, &res);
  newcomm = res;
}
// ----------------------------------------------------------------------------------
// --   MPI_Comm_free
// -----------------------------------------------------------------------------------


extern "C" void
mpi_comm_free_(int &comm, int &err)
{
  MPI_Comm res = comm;
  err = MPI_Comm_free(&res);
}

// ----------------------------------------------------------------------------------
// --   MPI_Comm_create
// -----------------------------------------------------------------------------------


extern "C" void
mpi_comm_create_(int &comm, int &group, int &newcomm, int &err)
{
  MPI_Comm res;
  MPI_Group g = group;
  err = MPI_Comm_create(comm, g, &res);
  newcomm = res;
}

// ----------------------------------------------------------------------------------
// --   MPI_Comm_group
// -----------------------------------------------------------------------------------


extern "C" void
mpi_comm_group_(int &comm, int &group, int err)
{
  MPI_Group g = group;
  err = MPI_Comm_group(comm, &g);
  group = g;
}

// ----------------------------------------------------------------------------------
// --   MPI_Group_incl
// -----------------------------------------------------------------------------------


extern "C" void
mpi_group_incl_(int &group, int &n, int *ranks, int &newgroup, int &err)
{
  MPI_Group g = group;
  MPI_Group g2 = newgroup;
  err = MPI_Group_incl(g, n, ranks, &g2);
  newgroup = g2;
}

// ----------------------------------------------------------------------------------
// --   MPI_Group_free
// -----------------------------------------------------------------------------------


extern "C" void
mpi_group_free_(int &group, int &err)
{
  MPI_Group g = group;
  err = MPI_Group_free(&g);
}

// ----------------------------------------------------------------------------------
// --   MPI_Sendrecv
// -----------------------------------------------------------------------------------
extern "C" void
mpi_sendrecv0_( int &sendcount, int &sendtype, int &dest,
                int &sendtag,  int &recvcount, int &recvtype, int &source,
                int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(NULL, sendcount, sendtype, dest, sendtag, NULL,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv1_(int &sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, int &recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(&sendbuf, sendcount, sendtype, dest, sendtag, &recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv2_(int *sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, int *recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv3_(float &sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, float &recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(&sendbuf, sendcount, sendtype, dest, sendtag, &recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv4_(float *sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, float *recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv5_(double &sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, double &recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(&sendbuf, sendcount, sendtype, dest, sendtag, &recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

extern "C" void
mpi_sendrecv6_(double *sendbuf, int &sendcount, int &sendtype, int &dest,
               int &sendtag, double *recvbuf, int &recvcount, int &recvtype, int &source,
               int &recvtag, int &comm, int *status, int &err)
{
  MPI_Status cstat;
  err = MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf,
                     recvcount, recvtype, source, recvtag, comm, &cstat);
  status[0] = cstat.MPI_SOURCE;
  status[1] = cstat.MPI_TAG;
  status[2] = cstat.MPI_ERROR;
  status[3] = cstat.BYTES_RECV;
}

// ----------------------------------------------------------------------------------
// --   MPI_Isend
// -----------------------------------------------------------------------------------
extern "C" void
mpi_isend0_( int &count, int &datatype, int &dest, int &tag,
             int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(NULL, count, d, dest, tag, c, &req);
  request = req;
}

extern "C" void
mpi_isend1_(int &buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(&buf, count, d, dest, tag, c, &req);
  request = req;
}

extern "C" void
mpi_isend2_(int *buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(buf, count, d, dest, tag, c, &req);
  request = req;
}

extern "C" void
mpi_isend3_(float &buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(&buf, count, d, dest, tag, c, &req);
  request = req;
}

extern "C" void
mpi_isend4_(float *buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(buf, count, d, dest, tag, c, &req);
  request = req;
}
extern "C" void
mpi_isend5_(double &buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(&buf, count, d, dest, tag, c, &req);
  request = req;
}

extern "C" void
mpi_isend6_(double *buf, int &count, int &datatype, int &dest, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Isend(buf, count, d, dest, tag, c, &req);
  request = req;
}

// ----------------------------------------------------------------------------------
// --   MPI_Irecv
// -----------------------------------------------------------------------------------
extern "C" void
mpi_irecv0_( int &count, int &datatype, int &source, int &tag,
             int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(NULL, count, d, source, tag, c, &req);
  request = req;
}

extern "C" void
mpi_irecv1_(int &buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(&buf, count, d, source, tag, c, &req);
  request = req;
}

extern "C" void
mpi_irecv2_(int *buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(buf, count, d, source, tag, c, &req);
  request = req;
}

extern "C" void
mpi_irecv3_(float &buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(&buf, count, d, source, tag, c, &req);
  request = req;
}
extern "C" void
mpi_irecv4_(float *buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(buf, count, d, source, tag, c, &req);
  request = req;
}
extern "C" void
mpi_irecv5_(double &buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(&buf, count, d, source, tag, c, &req);
  request = req;
}
extern "C" void
mpi_irecv6_(double *buf, int &count, int &datatype, int &source, int &tag,
            int &comm, int &request, int &err)
{
  MPI_Request req;
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Irecv(buf, count, d, source, tag, c, &req);
  request = req;
}

// ----------------------------------------------------------------------------------
// --   MPI_Recover_payload
// -----------------------------------------------------------------------------------


extern "C" void
mpi_recover_payload1_(int &buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(&buf, req);
}

extern "C" void
mpi_recover_payload2_(int *buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(buf, req);
}

extern "C" void
mpi_recover_payload3_(float &buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(&buf, req);
}

extern "C" void
mpi_recover_payload4_(float *buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(buf, req);
}

extern "C" void
mpi_recover_payload5_(double &buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(&buf, req);
}

extern "C" void
mpi_recover_payload6_(double *buf, int &request, int &err)
{
  MPI_Request req = request;
  err = MPI_Recover_Payload(buf, req);
}

// ----------------------------------------------------------------------------------
// --   MPI_Allreduce
// -----------------------------------------------------------------------------------
extern "C" void
mpi_allreduce0_( int &count, int &datatype, int &op,
                 int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(NULL, NULL, count, d, op, c);
}

extern "C" void
mpi_allreduce1_(int &sendbuf, int &recvbuf, int &count, int &datatype, int &op,
                int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce2_(int *sendbuf, int *recvbuf, int &count, int &datatype, int &op,
                int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(sendbuf, recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce3_(float &sendbuf, float &recvbuf, int &count, int &datatype,
                int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce4_(float *sendbuf, float *recvbuf, int &count, int &datatype,
                int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(sendbuf, recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce5_(double &sendbuf, double &recvbuf, int &count, int &datatype,
                int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce6_(double *sendbuf, double *recvbuf, int &count, int &datatype,
                int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(sendbuf, recvbuf, count, d, op, c);
}


extern "C" void
mpi_allreduce7_(long &sendbuf, long &recvbuf, int &count, int &datatype,
                int &op,
                int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_allreduce8_(long *sendbuf, long *recvbuf, int &count, int &datatype,
                int &op,
                int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Allreduce(sendbuf, recvbuf, count, d, op, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Scan
// -----------------------------------------------------------------------------------
extern "C" void
mpi_scan0_( int &count, int &datatype, int &op,
            int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(NULL, NULL, count, d, op, c);
}

extern "C" void
mpi_scan1_(int &sendbuf, int &recvbuf, int &count, int &datatype, int &op,
           int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan2_(int *sendbuf, int *recvbuf, int &count, int &datatype, int &op,
           int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(sendbuf, recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan3_(float &sendbuf, float &recvbuf, int &count, int &datatype,
           int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan4_(float *sendbuf, float *recvbuf, int &count, int &datatype,
           int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(sendbuf, recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan5_(double &sendbuf, double &recvbuf, int &count, int &datatype,
           int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan6_(double *sendbuf, double *recvbuf, int &count, int &datatype,
           int &op, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(sendbuf, recvbuf, count, d, op, c);
}


extern "C" void
mpi_scan7_(long &sendbuf, long &recvbuf, int &count, int &datatype, int &op,
           int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(&sendbuf, &recvbuf, count, d, op, c);
}

extern "C" void
mpi_scan8_(long *sendbuf, long *recvbuf, int &count, int &datatype, int &op,
           int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Scan(sendbuf, recvbuf, count, d, op, c);
}



// ----------------------------------------------------------------------------------
// --   MPI_Reduce
// -----------------------------------------------------------------------------------
extern "C" void
mpi_reduce0_(int &count, int &datatype, int &op,
             int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(NULL, NULL, count, d, op, root, c);
}

extern "C" void
mpi_reduce1_(int &sendbuf, int &recvbuf, int &count, int &datatype, int &op,
             int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(&sendbuf, &recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce2_(int *sendbuf, int *recvbuf, int &count, int &datatype, int &op,
             int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(sendbuf, recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce3_(float &sendbuf, float &recvbuf, int &count, int &datatype,
             int &op, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(&sendbuf, &recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce4_(float *sendbuf, float *recvbuf, int &count, int &datatype,
             int &op, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(sendbuf, recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce5_(double &sendbuf, double &recvbuf, int &count, int &datatype,
             int &op, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(&sendbuf, &recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce6_(double *sendbuf, double *recvbuf, int &count, int &datatype,
             int &op, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(sendbuf, recvbuf, count, d, op, root, c);
}

extern "C" void
mpi_reduce92_(long *sendbuf, long *recvbuf, int &count, int &datatype, int &op,
              int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Reduce(sendbuf, recvbuf, count, d, op, root, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Barrier
// -----------------------------------------------------------------------------------


extern "C" void
mpi_barrier0_(int &comm, int &err)
{
  MPI_Comm c = comm;

  err = MPI_Barrier(c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Bcast
// -----------------------------------------------------------------------------------
extern "C" void
mpi_bcast0_( int &count, int &datatype, int &root, int &comm, int &err)
{
  MPI_Print("MPIf_Bcast - no buffer");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(NULL, count, d, root, c);
}

extern "C" void
mpi_bcast1_(int &buf, int &count, int &datatype, int &root, int &comm, int &err)
{
  MPI_Print("MPIf_Bcast - integer");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(&buf, count, d, root, c);
}

extern "C" void
mpi_bcast100_(bool &buf, int &count, int &datatype, int &root, int &comm,
              int &err)
{
  MPI_Print("MPIf_Bcast - bool");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  int ret;
  err = MPI_Bcast(&ret, count, d, root, c);
  buf = ret;
}

extern "C" void
mpi_bcast2_(int *buf, int &count, int &datatype, int &root, int &comm, int &err)
{
  MPI_Print("MPIf_Bcast - integer array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast3_(float &buf, int &count, int &datatype, int &root, int &comm,
            int &err)
{
  MPI_Print("MPIf_Bcast - float");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(&buf, count, d, root, c);
}

extern "C" void
mpi_bcast4_(float *buf, int &count, int &datatype, int &root, int &comm,
            int &err)
{
  MPI_Print("MPIf_Bcast - float array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast5_(double &buf, int &count, int &datatype, int &root, int &comm,
            int &err)
{
  MPI_Print("MPIf_Bcast - double");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(&buf, count, d, root, c);
}

extern "C" void
mpi_bcast6_(double *buf, int &count, int &datatype, int &root, int &comm,
            int &err)
{
  MPI_Print("MPIf_Bcast - double array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast7_(int *buf, int &count, int &datatype, int &root, int &comm, int &err)
{
  MPI_Print("MPIf_Bcast - integer 2-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast8_(int *buf, int &count, int &datatype, int &root, int &comm, int &err)
{
  MPI_Print("MPIf_Bcast - integer 3-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast9_(float *buf, int &count, int &datatype, int &root, int &comm,
            int &err)
{
  MPI_Print("MPIf_Bcast - float 2-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast10_(float *buf, int &count, int &datatype, int &root, int &comm,
             int &err)
{
  MPI_Print("MPIf_Bcast - float 3-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast11_(double *buf, int &count, int &datatype, int &root, int &comm,
             int &err)
{
  MPI_Print("MPIf_Bcast - double 2-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

extern "C" void
mpi_bcast12_(double *buf, int &count, int &datatype, int &root, int &comm,
             int &err)
{
  MPI_Print("MPIf_Bcast - double 3-dim array");
  MPI_Comm c = comm;
  MPI_Datatype d = datatype;
  err = MPI_Bcast(buf, count, d, root, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Gather
// -----------------------------------------------------------------------------------
extern "C" void
mpi_gather0_( int &sendcnt, int &sendtype,
              int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(NULL, sendcnt, sd, NULL, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather1_(int &sendbuf, int &sendcnt, int &sendtype, int &recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(&sendbuf, sendcnt, sd, &recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather2_(int *sendbuf, int &sendcnt, int &sendtype, int *recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(sendbuf, sendcnt, sd, recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather3_(float &sendbuf, int &sendcnt, int &sendtype, float &recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(&sendbuf, sendcnt, sd, &recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather4_(float *sendbuf, int &sendcnt, int &sendtype, float *recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(sendbuf, sendcnt, sd, recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather5_(double &sendbuf, int &sendcnt, int &sendtype, double &recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(&sendbuf, sendcnt, sd, &recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather6_(double *sendbuf, int &sendcnt, int &sendtype, double *recvbuf,
             int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(sendbuf, sendcnt, sd, recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather37_(double &sendbuf, int &sendcnt, int &sendtype, double *recvbuf,
              int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(&sendbuf, sendcnt, sd, recvbuf, recvcnt, rd, root, c);
}

extern "C" void
mpi_gather38_(double *sendbuf, int &sendcnt, int &sendtype, double &recvbuf,
              int &recvcnt, int &recvtype, int &root, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Gather(sendbuf, sendcnt, sd, &recvbuf, recvcnt, rd, root, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Allgather
// -----------------------------------------------------------------------------------
extern "C" void
mpi_allgather0_( int &sendcount, int &sendtype,
                 int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(NULL, sendcount, sd, NULL, recvcount, rd, c);
}

extern "C" void
mpi_allgather1_(int &sendbuf, int &sendcount, int &sendtype, int &recvbuf,
                int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_allgather2_(int *sendbuf, int &sendcount, int &sendtype, int *recvbuf,
                int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_allgather3_(float &sendbuf, int &sendcount, int &sendtype, float &recvbuf,
                int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_allgather4_(float *sendbuf, int &sendcount, int &sendtype, float *recvbuf,
                int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_allgather5_(double &sendbuf, int &sendcount, int &sendtype,
                double &recvbuf, int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_allgather6_(double *sendbuf, int &sendcount, int &sendtype,
                double *recvbuf, int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}
// ----------------------------------------------------------------------------------
// --   MPI_Scatter
// -----------------------------------------------------------------------------------
extern "C" void
mpi_scatter0_( int &sendcount, int &sendtype,
               int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(NULL, sendcount, sd, NULL, recvcount, rd, c);
}

extern "C" void
mpi_scatter1_(int &sendbuf, int &sendcount, int &sendtype, int &recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_scatter2_(int *sendbuf, int &sendcount, int &sendtype, int *recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_scatter3_(float &sendbuf, int &sendcount, int &sendtype, float &recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_scatter4_(float *sendbuf, int &sendcount, int &sendtype, float *recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_scatter5_(double &sendbuf, int &sendcount, int &sendtype, double &recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_scatter6_(double *sendbuf, int &sendcount, int &sendtype, double *recvbuf,
              int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}
// ----------------------------------------------------------------------------------
// --   MPI_Alltoall
// -----------------------------------------------------------------------------------
extern "C" void
mpi_alltoall0_( int &sendcount, int &sendtype,
                int &recvcount, int &recvtype, int &comm, int &err)
{

  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(NULL, sendcount, sd, NULL, recvcount, rd, c);
}

extern "C" void
mpi_alltoall1_(int &sendbuf, int &sendcount, int &sendtype, int &recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{

  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_alltoall2_(int *sendbuf, int &sendcount, int &sendtype, int *recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{

  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;

  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_alltoall3_(float &sendbuf, int &sendcount, int &sendtype, float &recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_alltoall4_(float *sendbuf, int &sendcount, int &sendtype, float *recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_alltoall5_(double &sendbuf, int &sendcount, int &sendtype, double &recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(&sendbuf, sendcount, sd, &recvbuf, recvcount, rd, c);
}

extern "C" void
mpi_alltoall6_(double *sendbuf, int &sendcount, int &sendtype, double *recvbuf,
               int &recvcount, int &recvtype, int &comm, int &err)
{
  MPI_Comm c = comm;
  MPI_Datatype sd = sendtype;
  MPI_Datatype rd = recvtype;
  err = MPI_Allgather(sendbuf, sendcount, sd, recvbuf, recvcount, rd, c);
}

// ----------------------------------------------------------------------------------
// --   MPI_Wtime
// -----------------------------------------------------------------------------------

extern "C" double
mpi_wtime_()
{
  return MPI_Wtime();
}

// ----------------------------------------------------------------------------------
// --   MPI_Wtick
// -----------------------------------------------------------------------------------

extern "C" double
mpi_wtick_()
{
  return (double) sstmac::timestamp::tick_interval_int64() / (double) 1e12;
}

//----------------------------------------------------------------
// --- MPI Cart functions
//----------------------------------------------------------------
extern "C" void
mpi_cart_create_(int comm, int ndims, int *dims, int *periods, int reorder,
                 int *outcomm, int ierr)
{
  MPI_Comm c = comm;
  MPI_Comm outc;
  ierr = MPI_Cart_create(c, ndims, dims, periods, reorder, &outc);
  *outcomm = outc;
}

extern "C" void
mpi_cart_get(MPI_Comm comm, int maxdims, int *dims, int *periods, int *coords,
             int ierr)
{
  MPI_Comm c = comm;

  ierr = MPI_Cart_get(c, maxdims, dims, periods, coords);
}

extern "C" void
mpi_cart_rank(MPI_Comm comm, int *coords, int *rank, int ierr)
{
  MPI_Comm c = comm;

  ierr = MPI_Cart_rank(c, coords, rank);
}

extern "C" void
mpi_cart_shift(MPI_Comm comm, int direction, int displ, int *source, int *dest,
               int ierr)
{
  MPI_Comm c = comm;

  ierr = MPI_Cart_shift(c, direction, displ, source, dest);
}

extern "C" void
mpi_cart_coords(MPI_Comm comm, int rank, int maxdims, int *coords, int ierr)
{
  MPI_Comm c = comm;

  ierr = MPI_Cart_coords(c, rank, maxdims, coords);
}

