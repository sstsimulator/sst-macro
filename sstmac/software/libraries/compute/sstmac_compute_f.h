!
!  This file is part of SST/macroscale:
!               The macroscale architecture simulator from the SST suite.
!  Copyright (c) 2009 Sandia Corporation.
!  This software is distributed under the BSD License.
!  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
!  the U.S. Government retains certain rights in this software.
!  For more information, see the LICENSE file in the top
!  SST/macroscale directory.
!

!module SSTMAC_compute

interface
	subroutine SSTMAC_compute(seconds)
		double precision :: seconds
	end subroutine SSTMAC_compute
end interface

interface
        subroutine SSTMAC_compute_block_read(bytes)
                integer :: bytes
        end subroutine SSTMAC_compute_block_read
end interface

interface SSTMAC_compute_loop
	subroutine SSTMAC_compute_loop_0(from, to, numlines)
		integer :: from, to, numlines
	end subroutine

	subroutine SSTMAC_compute_loop_1(from, to, numlines)
		integer :: from, to
		real :: numlines
	end subroutine
end interface

interface SSTMAC_compute_loop2
	subroutine SSTMAC_compute_loop2_0(from1, to1, from2, to2, numlines)
		integer :: from1, to1, from2, to2, numlines
	end subroutine

	subroutine SSTMAC_compute_loop2_1(from1, to1, from2, to2, numlines)
		integer :: from1, to1, from2, to2
		real :: numlines
	end subroutine
end interface

interface SSTMAC_compute_loop3
	subroutine SSTMAC_compute_loop3_0(from1, to1, from2, to2, from3, to3, numlines)
		integer :: from1, to1, from2, to2, from3, to3, numlines
	end subroutine

	subroutine SSTMAC_compute_loop3_1(from1, to1, from2, to2, from3, to3, numlines)
		integer :: from1, to1, from2, to2, from3, to3
		real :: numlines
	end subroutine
end interface

interface SSTMAC_compute_loop4
	subroutine SSTMAC_compute_loop4_0(from1, to1, from2, to2, from3, to3, from4, to4, numlines)
		integer :: from1, to1, from2, to2, from3, to3, from4, to4, numlines
	end subroutine

	subroutine SSTMAC_compute_loop4_1(from1, to1, from2, to2, from3, to3, from4, to4, numlines)
		integer :: from1, to1, from2, to2, from3, to3, from4, to4
		real :: numlines
	end subroutine
end interface
