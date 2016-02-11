!
!  This file is part of SST/macroscale: 
!               The macroscale architecture simulator from the SST suite.
!  Copyright (c) 2009-2011 Sandia Corporation.
!  This software is distributed under the BSD License.
!  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
!  the U.S. Government retains certain rights in this software.
!  For more information, see the LICENSE file in the top 
!  SST/macroscale directory.
!

!module MPI
! These are hard-mapped to sstmac_mpi.h
integer, parameter :: MPI_INTEGER = 5, MPI_INTEGER4 = 4, MPI_INTEGER8 = 5, &
  MPI_REAL = 6, MPI_REAL4 = 6, MPI_REAL8 = 7, MPI_DOUBLE_PRECISION = 7
integer, parameter :: MPI_STATUS_SIZE = 4
!end module MPI_TYPES

integer, parameter :: MPI_MAX = 1, MPI_MIN = 2, MPI_SUM = 3, MPI_PROD = 4, MPI_LAND = 5, MPI_BAND = 6
integer, parameter :: MPI_LOR = 7, MPI_BOR = 8, MPI_LXOR = 9, MPI_BXOR = 10, MPI_MINLOC = 11, MPI_MAXLOC = 12
integer, parameter :: MPI_COMM_WORLD = 1140850688, MPI_COMM_SELF = 1140850689 

integer, parameter :: MPI_MAX_ERROR_STRING=512
integer, parameter :: MPI_ADDRESS_KIND = 4

integer, parameter ::  MPI_COMM_NULL     = 67108864
integer, parameter ::  MPI_OP_NULL      =  402653184
integer, parameter ::  MPI_GROUP_NULL    = 134217728
integer, parameter ::  MPI_DATATYPE_NULL = 201326592
integer, parameter ::  MPI_REQUEST_NULL  = 738197504

integer, parameter :: MPI_SUCCESS = 0, MPI_ERROR = 1

integer, parameter :: MPI_ERRORS_ARE_FATAL = 1


integer, parameter :: MPI_BYTE = 2


interface
	subroutine MPI_Errhandler_set(comm, handle, ierr)
	integer :: comm, handle, ierr
	end subroutine MPI_Errhandler_set
end interface

interface
	subroutine MPI_Error_string(ierr, string, len, ierror)
	integer :: ierr, len, ierror
	character*(*) :: string
	end subroutine MPI_Error_string
end interface

INTERFACE 
   SUBROUTINE MPI_Init(err)
     INTEGER, INTENT(OUT) :: err
   END SUBROUTINE MPI_Init
END INTERFACE

interface
	subroutine MPI_Initialized(flag, err)
	integer :: flag, err
	end subroutine
end interface

INTERFACE
   SUBROUTINE MPI_Finalize(err)
     INTEGER, INTENT(OUT) :: err
   END SUBROUTINE MPI_Finalize
END INTERFACE

interface
	subroutine MPI_Abort(comm, code, err)
		integer :: comm
		integer :: code, err
	end subroutine MPI_Abort
end interface

interface
	subroutine MPI_Comm_Rank(comm, rank, err)
		integer :: comm, rank
		integer :: err
	end subroutine MPI_Comm_Rank
end interface

interface
	subroutine MPI_Comm_Size(comm, size, err)
		integer :: comm, size, err
	end subroutine MPI_Comm_Size
end interface

interface MPI_Attr_get
	subroutine MPI_Attr_get1(comm, keyval, attr_value, flag, err)
	integer :: comm, keyval, attr_value, flag, err
	end subroutine

	subroutine MPI_Attr_get2(comm, keyval, attr_value, flag, err)
	integer :: comm, keyval, attr_value, err
	logical :: flag
	end subroutine
end interface MPI_Attr_get


interface MPI_Send

 subroutine MPI_Send0( cnt, datatype, dest, tag, comm, err)
	integer :: cnt, dest, tag, comm, err
	integer :: datatype
  end subroutine MPI_Send0

  subroutine MPI_Send1(buf, cnt, datatype, dest, tag, comm, err)
	integer :: buf
	integer :: cnt, dest, tag, comm, err
	integer :: datatype
  end subroutine MPI_Send1

  subroutine MPI_Send2(buf, cnt, datatype, dest, tag, comm, err)
	integer :: cnt, dest, tag, comm, err
	integer, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Send2

 subroutine MPI_Send3(buf, cnt, datatype, dest, tag, comm, err)
	real :: buf
	integer :: cnt, dest, tag, comm, err
	integer :: datatype
  end subroutine MPI_Send3

  subroutine MPI_Send4(buf, cnt, datatype, dest, tag, comm, err)
	integer :: cnt, dest, tag, comm, err
	real, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Send4

 subroutine MPI_Send5(buf, cnt, datatype, dest, tag, comm, err)
	double precision :: buf
	integer :: cnt, dest, tag, comm, err
	integer :: datatype
  end subroutine MPI_Send5

  subroutine MPI_Send6(buf, cnt, datatype, dest, tag, comm, err)
	integer :: cnt, dest, tag, comm, err
	double precision, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Send6

   subroutine MPI_Send7(buf, cnt, datatype, dest, tag, comm, err)
	integer :: cnt, dest, tag, comm, err
	integer, dimension(:,:) :: buf
	integer :: datatype
  end subroutine MPI_Send7

end interface MPI_Send

interface MPI_Recv

	subroutine MPI_Recv0(cnt, datatype, src, tag, comm, status, err)
	integer :: cnt, src, tag, comm, err
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv0

  subroutine MPI_Recv1(buf, cnt, datatype, src, tag, comm, status, err)
	integer :: buf
	integer :: cnt, src, tag, comm, err
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv1

  subroutine MPI_Recv2(buf, cnt, datatype, src, tag, comm, status, err)
	integer :: cnt, src, tag, comm, err
	integer:: buf(cnt)
	integer :: datatype
	integer, dimension(:) :: status
 end subroutine MPI_Recv2

subroutine MPI_Recv3(buf, cnt, datatype, src, tag, comm, status, err)
	real :: buf
	integer :: cnt, src, tag, comm, err
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv3

  subroutine MPI_Recv4(buf, cnt, datatype, src, tag, comm, status, err)
	integer :: cnt, src, tag, comm, err
	real, dimension(cnt) :: buf
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv4

  subroutine MPI_Recv5(buf, cnt, datatype, src, tag, comm, status, err)
	double precision :: buf
	integer :: cnt, src, tag, comm, err
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv5

  subroutine MPI_Recv6(buf, cnt, datatype, src, tag, comm, status, err)
	integer :: cnt, src, tag, comm, err
	double precision, dimension(cnt) :: buf
	integer :: datatype
	integer, dimension(:) :: status
  end subroutine MPI_Recv6

  subroutine MPI_Recv7(buf, cnt, datatype, src, tag, comm, status, err)
	integer :: cnt, src, tag, comm, err
	integer, dimension(:,:) :: buf
	integer :: datatype
	integer, dimension(:) :: status
 end subroutine MPI_Recv7

end interface MPI_Recv

INTERFACE MPI_Wait
	SUBROUTINE MPI_Wait1(request, status, err)
	  integer, INTENT(IN) :: request
	  integer, dimension(:) :: status
	  INTEGER, INTENT(OUT) :: err
	END SUBROUTINE MPI_Wait1

	SUBROUTINE MPI_Wait2(request, status, err)
	  integer(KIND=8), INTENT(IN) :: request
	  integer, dimension(:) :: status
	  INTEGER, INTENT(OUT) :: err
	END SUBROUTINE MPI_Wait2
END INTERFACE MPI_Wait

INTERFACE MPI_Waitany
	SUBROUTINE MPI_Waitany1(cnt, requests, idx, status, err)
	  integer :: cnt, idx
	  integer, dimension(cnt) :: requests
	  integer, dimension(:) :: status
	  INTEGER, INTENT(OUT) :: err
	END SUBROUTINE MPI_Waitany1

	SUBROUTINE MPI_Waitany2(cnt, requests, idx, status, err)
	  integer :: cnt, idx
	  integer(KIND=8), dimension(cnt) :: requests
	  integer, dimension(:) :: status
	  INTEGER, INTENT(OUT) :: err
	END SUBROUTINE MPI_Waitany2
END INTERFACE MPI_Waitany

INTERFACE
	SUBROUTINE MPI_Waitsome(incnt, requests, outcnt, indices, statuses, err)
	  integer :: incnt, outcnt, err
	  integer, dimension(incnt) :: requests
	  integer, dimension(:) :: indices
	  integer, dimension(:,:) :: statuses
	END SUBROUTINE MPI_Waitsome
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Get_count(status, datatype, cnt, err)
	  integer :: cnt, err, datatype
	  integer, dimension(:) :: status
	END SUBROUTINE MPI_Get_count
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Test(request, flag, status, err)
	  integer :: request, flag, err
	  integer, dimension(:) :: status
	END SUBROUTINE MPI_Test
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Cancel(request, err)
	  integer :: request, err
	END SUBROUTINE MPI_Cancel
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Request_free(request, err)
	  integer :: request, err
	END SUBROUTINE MPI_Request_free
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Comm_dup(comm, newcomm, err)
	  integer :: comm, newcomm, err
	END SUBROUTINE MPI_Comm_dup
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Comm_split(comm, color, key, newcomm, err)
	  integer :: comm, color, key, newcomm, err
	END SUBROUTINE MPI_Comm_split
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Comm_free(comm, err)
	  integer :: comm, err
	END SUBROUTINE MPI_Comm_free
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Comm_create(comm, group, newcomm, err)
	  integer :: comm, group, newcomm, err
	END SUBROUTINE MPI_Comm_create
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Comm_group(comm, group, err)
	  integer :: comm, group, err
	END SUBROUTINE MPI_Comm_group
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Group_incl(group, n, ranks, newgroup, err)
	  integer :: group, n, newgroup, err
	  integer, dimension(:) :: ranks
	END SUBROUTINE MPI_Group_incl
END INTERFACE

INTERFACE
	SUBROUTINE MPI_Group_free(group, err)
	  integer :: group, err
	END SUBROUTINE MPI_Group_free
END INTERFACE

interface MPI_Sendrecv
  subroutine MPI_Sendrecv0(sendcnt, senddatatype, dest, sendtag, &
         recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv0

  subroutine MPI_Sendrecv1(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	integer :: sendbuf, recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv1

  subroutine MPI_Sendrecv2(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	integer, dimension(sendcnt) :: sendbuf
	integer, dimension(recvcnt) :: recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv2

  subroutine MPI_Sendrecv3(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	real :: sendbuf, recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv3

   subroutine MPI_Sendrecv4(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	real, dimension(sendcnt) :: sendbuf
	real, dimension(recvcnt) :: recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv4

  subroutine MPI_Sendrecv5(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	double precision :: sendbuf, recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv5

   subroutine MPI_Sendrecv6(sendbuf, sendcnt, senddatatype, dest, sendtag, &
        recvbuf, recvcnt, recvtype, source, recvtag, comm, status, err)
	integer :: sendcnt, senddatatype, dest, sendtag, recvcnt, recvtype, source, recvtag, comm, err
	double precision, dimension(sendcnt) :: sendbuf
	double precision, dimension(recvcnt) :: recvbuf
	integer, dimension(:) :: status
  end subroutine MPI_Sendrecv6

end interface MPI_Sendrecv

interface MPI_Isend
subroutine MPI_Isend0( cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	integer :: datatype
  end subroutine MPI_Isend0

  subroutine MPI_Isend1(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	integer :: buf
	integer :: datatype
  end subroutine MPI_Isend1

  subroutine MPI_Isend2(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	integer, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Isend2

  subroutine MPI_Isend3(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	real :: buf
	integer :: datatype
  end subroutine MPI_Isend3

  subroutine MPI_Isend4(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	real, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Isend4

  subroutine MPI_Isend5(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	double precision :: buf
	integer :: datatype
  end subroutine MPI_Isend5

  subroutine MPI_Isend6(buf, cnt, datatype, dest, tag, comm, request, err)
	integer :: cnt, dest, tag, comm, err, request
	double precision, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Isend6


end interface MPI_Isend

interface MPI_Irecv
subroutine MPI_Irecv0(cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	integer :: datatype
  end subroutine MPI_Irecv0

  subroutine MPI_Irecv1(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	integer :: buf
	integer :: datatype
  end subroutine MPI_Irecv1

  subroutine MPI_Irecv2(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	integer, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Irecv2

  subroutine MPI_Irecv3(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	real :: buf
	integer :: datatype
  end subroutine MPI_Irecv3

  subroutine MPI_Irecv4(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	real, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Irecv4

  subroutine MPI_Irecv5(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	double precision :: buf
	integer :: datatype
  end subroutine MPI_Irecv5

  subroutine MPI_Irecv6(buf, cnt, datatype, source, tag, comm, request, err)
	integer :: cnt, source, tag, comm, err, request
	double precision, dimension(cnt) :: buf
	integer :: datatype
  end subroutine MPI_Irecv6

end interface MPI_Irecv


INTERFACE MPI_Recover_payload
	SUBROUTINE MPI_Recover_payload1(buf, request, err)
	  integer :: request, err
	  integer :: buf
	END SUBROUTINE MPI_Recover_payload1

	SUBROUTINE MPI_Recover_payload2(buf, cnt, request, err)
	  integer :: request, err, cnt
	  integer, dimension(cnt) :: buf
	END SUBROUTINE MPI_Recover_payload2

	SUBROUTINE MPI_Recover_payload3(buf, request, err)
	  integer :: request, err
	  real :: buf
	END SUBROUTINE MPI_Recover_payload3

	SUBROUTINE MPI_Recover_payload4(buf, cnt, request, err)
	  integer :: request, err, cnt
	  real, dimension(cnt) :: buf
	END SUBROUTINE MPI_Recover_payload4

	SUBROUTINE MPI_Recover_payload5(buf, request, err)
	  integer :: request, err
	  double precision :: buf
	END SUBROUTINE MPI_Recover_payload5

	SUBROUTINE MPI_Recover_payload6(buf, cnt, request, err)
	  integer :: request, err, cnt
	  double precision :: buf
	END SUBROUTINE MPI_Recover_payload6
END INTERFACE MPI_Recover_payload


interface MPI_Allreduce
  subroutine MPI_Allreduce0(cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
  end subroutine MPI_Allreduce0

  subroutine MPI_Allreduce1(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer :: sendbuf, recvbuf
  end subroutine MPI_Allreduce1

  subroutine MPI_Allreduce2(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Allreduce2

  subroutine MPI_Allreduce3(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	real :: sendbuf, recvbuf
  end subroutine MPI_Allreduce3

  subroutine MPI_Allreduce4(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	real, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Allreduce4

  subroutine MPI_Allreduce5(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	double precision :: sendbuf, recvbuf
  end subroutine MPI_Allreduce5

  subroutine MPI_Allreduce6(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	double precision, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Allreduce6

  subroutine MPI_Allreduce7(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer(KIND=8) :: sendbuf, recvbuf
  end subroutine MPI_Allreduce7

   subroutine MPI_Allreduce8(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer(KIND=8), dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Allreduce8

end interface MPI_Allreduce

interface MPI_Scan
  subroutine MPI_Scan0(cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
  end subroutine MPI_Scan0

  subroutine MPI_Scan1(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer :: sendbuf, recvbuf
  end subroutine MPI_Scan1

  subroutine MPI_Scan2(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Scan2

  subroutine MPI_Scan3(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	real :: sendbuf, recvbuf
  end subroutine MPI_Scan3

  subroutine MPI_Scan4(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	real, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Scan4

  subroutine MPI_Scan5(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	double precision :: sendbuf, recvbuf
  end subroutine MPI_Scan5

  subroutine MPI_Scan6(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	double precision, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Scan6

  subroutine MPI_Scan7(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer(KIND=8) :: sendbuf, recvbuf
  end subroutine MPI_Scan7

   subroutine MPI_Scan8(sendbuf, recvbuf, cnt, datatype, op, comm, err)
	integer :: cnt, datatype, op, comm, err
	integer(KIND=8), dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Scan8

end interface MPI_Scan


interface MPI_Reduce
  subroutine MPI_Reduce0(cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
  end subroutine MPI_Reduce0

  subroutine MPI_Reduce1(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	integer :: sendbuf, recvbuf
  end subroutine MPI_Reduce1

  subroutine MPI_Reduce2(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	integer, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Reduce2

  subroutine MPI_Reduce3(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	real :: sendbuf, recvbuf
  end subroutine MPI_Reduce3

  subroutine MPI_Reduce4(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	real, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Reduce4

  subroutine MPI_Reduce5(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	double precision :: sendbuf, recvbuf
  end subroutine MPI_Reduce5

  subroutine MPI_Reduce6(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	double precision, dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Reduce6

  subroutine MPI_Reduce92(sendbuf, recvbuf, cnt, datatype, op, root, comm, err)
	integer :: cnt, datatype, op, root, comm, err
	integer(KIND=8), dimension(cnt) :: sendbuf, recvbuf
  end subroutine MPI_Reduce92

end interface MPI_Reduce

INTERFACE MPI_Barrier
	SUBROUTINE MPI_Barrier0(comm, err)
	  integer :: comm, err
	END SUBROUTINE MPI_Barrier0
END INTERFACE MPI_Barrier


INTERFACE MPI_Bcast

	SUBROUTINE MPI_Bcast0( cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	END SUBROUTINE MPI_Bcast0

	SUBROUTINE MPI_Bcast1(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  integer :: buf
	END SUBROUTINE MPI_Bcast1

	SUBROUTINE MPI_Bcast100(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  logical :: buf
	END SUBROUTINE MPI_Bcast100

	SUBROUTINE MPI_Bcast2(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  integer, dimension(cnt) :: buf
	END SUBROUTINE MPI_Bcast2

	SUBROUTINE MPI_Bcast3(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  real :: buf
	END SUBROUTINE MPI_Bcast3

	SUBROUTINE MPI_Bcast4(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  real, dimension(cnt) :: buf
	END SUBROUTINE MPI_Bcast4

	SUBROUTINE MPI_Bcast5(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  double precision :: buf
	END SUBROUTINE MPI_Bcast5

	SUBROUTINE MPI_Bcast6(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  double precision, dimension(cnt) :: buf
	END SUBROUTINE MPI_Bcast6

	SUBROUTINE MPI_Bcast7(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  integer, dimension(:,:) :: buf
	END SUBROUTINE MPI_Bcast7

	SUBROUTINE MPI_Bcast8(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  integer, dimension(:,:,:) :: buf
	END SUBROUTINE MPI_Bcast8

	SUBROUTINE MPI_Bcast9(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  real, dimension(:,:) :: buf
	END SUBROUTINE MPI_Bcast9

	SUBROUTINE MPI_Bcast10(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  real, dimension(:,:,:) :: buf
	END SUBROUTINE MPI_Bcast10

	SUBROUTINE MPI_Bcast11(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  double precision, dimension(:,:) :: buf
	END SUBROUTINE MPI_Bcast11

	SUBROUTINE MPI_Bcast12(buf, cnt, datatype, root, comm, err)
	  integer :: cnt, datatype, root, comm, err
	  double precision, dimension(:,:,:) :: buf
	END SUBROUTINE MPI_Bcast12

END INTERFACE MPI_Bcast

INTERFACE MPI_Gather
	SUBROUTINE MPI_Gather0(sendcnt, sendtype, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	END SUBROUTINE MPI_Gather0

	SUBROUTINE MPI_Gather1(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  integer :: sendbuf
	  integer :: recvbuf
	END SUBROUTINE MPI_Gather1

	SUBROUTINE MPI_Gather2(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  integer, dimension(sendcnt) :: sendbuf
	  integer, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Gather2

	SUBROUTINE MPI_Gather3(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  real :: sendbuf
	  real :: recvbuf
	END SUBROUTINE MPI_Gather3

	SUBROUTINE MPI_Gather4(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  real, dimension(sendcnt) :: sendbuf
	  real, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Gather4

	SUBROUTINE MPI_Gather5(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  double precision :: sendbuf
	  double precision :: recvbuf
	END SUBROUTINE MPI_Gather5

	SUBROUTINE MPI_Gather6(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  double precision, dimension(sendcnt) :: sendbuf
	  double precision, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Gather6

	SUBROUTINE MPI_Gather37(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  double precision :: sendbuf
	  double precision, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Gather37

	SUBROUTINE MPI_Gather38(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, root, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, root, comm, err
	  double precision, dimension(sendcnt) :: sendbuf
	  double precision :: recvbuf
	END SUBROUTINE MPI_Gather38



END INTERFACE MPI_Gather

INTERFACE MPI_Allgather
	SUBROUTINE MPI_Allgather0( sendcnt, sendtype,  recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err

	END SUBROUTINE MPI_Allgather0

	SUBROUTINE MPI_Allgather1(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer :: sendbuf
	  integer :: recvbuf
	END SUBROUTINE MPI_Allgather1

	SUBROUTINE MPI_Allgather2(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer, dimension(sendcnt) :: sendbuf
	  integer, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Allgather2

	SUBROUTINE MPI_Allgather3(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real :: sendbuf
	  real :: recvbuf
	END SUBROUTINE MPI_Allgather3

	SUBROUTINE MPI_Allgather4(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real, dimension(sendcnt) :: sendbuf
	  real, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Allgather4

	SUBROUTINE MPI_Allgather5(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision :: sendbuf
	  double precision :: recvbuf
	END SUBROUTINE MPI_Allgather5

	SUBROUTINE MPI_Allgather6(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision, dimension(sendcnt) :: sendbuf
	  double precision, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Allgather6
END INTERFACE MPI_Allgather

INTERFACE MPI_Scatter
	SUBROUTINE MPI_Scatter0( sendcnt, sendtype,  recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	END SUBROUTINE MPI_Scatter0

	SUBROUTINE MPI_Scatter1(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer :: sendbuf
	  integer :: recvbuf
	END SUBROUTINE MPI_Scatter1

	SUBROUTINE MPI_Scatter2(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer, dimension(sendcnt) :: sendbuf
	  integer, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Scatter2

	SUBROUTINE MPI_Scatter3(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real :: sendbuf
	  real :: recvbuf
	END SUBROUTINE MPI_Scatter3

	SUBROUTINE MPI_Scatter4(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real, dimension(sendcnt) :: sendbuf
	  real, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Scatter4

	SUBROUTINE MPI_Scatter5(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision :: sendbuf
	  double precision :: recvbuf
	END SUBROUTINE MPI_Scatter5

	SUBROUTINE MPI_Scatter6(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision, dimension(sendcnt) :: sendbuf
	  double precision, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Scatter6
END INTERFACE MPI_Scatter

INTERFACE MPI_Alltoall
	SUBROUTINE MPI_Alltoall0( sendcnt, sendtype,  recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err

	END SUBROUTINE MPI_Alltoall0

	SUBROUTINE MPI_Alltoall1(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer :: sendbuf
	  integer :: recvbuf
	END SUBROUTINE MPI_Alltoall1

	SUBROUTINE MPI_Alltoall2(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  integer, dimension(sendcnt) :: sendbuf
	  integer, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Alltoall2

	SUBROUTINE MPI_Alltoall3(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real :: sendbuf
	  real :: recvbuf
	END SUBROUTINE MPI_Alltoall3

	SUBROUTINE MPI_Alltoall4(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  real, dimension(sendcnt) :: sendbuf
	  real, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Alltoall4

	SUBROUTINE MPI_Alltoall5(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision :: sendbuf
	  double precision :: recvbuf
	END SUBROUTINE MPI_Alltoall5

	SUBROUTINE MPI_Alltoall6(sendbuf, sendcnt, sendtype, recvbuf, recvcnt, recvtype, comm, err)
	  integer :: sendcnt, sendtype, recvcnt, recvtype, comm, err
	  double precision, dimension(sendcnt) :: sendbuf
	  double precision, dimension(recvcnt) :: recvbuf
	END SUBROUTINE MPI_Alltoall6
END INTERFACE MPI_Alltoall


interface
	 double precision function MPI_Wtime()
	end function MPI_Wtime
end interface

interface
	 double precision function MPI_Wtick()
	end function MPI_Wtick
end interface


interface
	subroutine MPI_Cart_create(comm, ndims, dims, periods, reorder, outcomm, ierr)
		integer :: comm, ndims, reorder, outcomm, ierr
		integer, dimension(ndims) :: dims, periods
	end subroutine MPI_Cart_create
end interface

interface
	subroutine MPI_Cart_get(comm, maxdims, dims, periods, coords, ierr)
		integer :: comm, maxdims, ierr
		integer, dimension(maxdims) :: dims, periods, coords
	end subroutine MPI_Cart_get
end interface

interface
	subroutine MPI_Cart_shift(comm, direction, displ, source, dest, ierr)
		integer :: comm, direction, displ, source, dest, ierr
	end subroutine MPI_Cart_shift
end interface

interface
	subroutine MPI_Cart_rank(comm, coords, rank, ierr)
		integer :: comm, rank, ierr
		integer, dimension(:) :: coords
	end subroutine MPI_Cart_rank
end interface


interface
	subroutine MPI_Cart_coords(comm, rank, maxdims, coords, ierr)
		integer :: comm, rank, maxdims, ierr
		integer, dimension(maxdims) :: coords
	end subroutine MPI_Cart_coords
end interface
