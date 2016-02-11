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

  subroutine test_compute_f90()

    include "sstmac/software/libraries/compute/sstmac_compute_f.h"

    double precision :: mytime
    integer :: myblocks
 
    mytime = 0.1234
    myblocks = 1024

    call sstmac_compute(mytime)
    call sstmac_compute_block_read(myblocks)

  end
