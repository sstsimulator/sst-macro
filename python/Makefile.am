#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

python_includedir=$(includedir)/python
EXTRA_DIST= \
  jobScheduler.py \
  merlin.py \
  emberMacro.py \
  emberLoadInfo.py \
  emberDefaultParams.py 

nobase_python_include_HEADERS = \
    default.py 

uninstall-local:
	test -f $(includedir)/sstmac/pysst/default.pyc ; if [[ $$? -eq 0 ]] ; then rm $(includedir)/sstmac/pysst/*.pyc; fi




