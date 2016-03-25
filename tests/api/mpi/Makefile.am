#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2011 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#
include $(top_srcdir)/Makefile.common

AM_CPPFLAGS += -I$(top_srcdir)/tests/mpi/include -Wp,-w -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi \
  -I$(srcdir) -I$(top_builddir)/pth -I$(top_srcdir)/pth \
  -I$(top_builddir)/tests/api/mpi -I$(top_srcdir)/tests/api/mpi \
  -I$(top_builddir)/tests/api/mpi/include -I$(top_srcdir)/tests/api/mpi/include -DHAVE_MPI_WIN_CREATE

EXTRA_DIST = testmpi.h include


if INTEGRATED_SST_CORE
lib_LTLIBRARIES = libsstmac_api_mpi_test.la
libsstmac_api_mpi_test_la_SOURCES = testmpi.cc testmpi_eli.cc util/mtest.cc
libsstmac_api_mpi_test_la_LDFLAGS = -module -shared $(PTH_LDFLAGS)
else
check_PROGRAMS = testexec
testexec_SOURCES = exec.cc
testexec_LDADD = \
  $(top_builddir)/sstmac/install/libsstmac.la \
  $(top_builddir)/sstmac/main/libsstmac_main.la \
  $(top_builddir)/sprockit/sprockit/libsprockit.la 
if EXTERNAL_BOOST
  testexec_LDADD += $(BOOST_LDFLAGS)
endif
if USE_MPIPARALLEL
testexec_LDADD += $(top_builddir)/sstmac/backends/mpi/libsstmac_mpi_runtime.la
endif
endif

# basically just libsstmac_api_mpi_test_la_SOURCES, but compiling is slow
TEST_SOURCE_FILES = \
  attr/attr2type.cc \
  attr/attrend.cc \
  attr/attrend2.cc \
  attr/attrerr.cc \
  attr/attrerrcomm.cc \
  attr/attrerrtype.cc \
  attr/attric.cc \
  attr/attrorder.cc \
  attr/attrordercomm.cc \
  attr/attrordertype.cc \
  attr/attrt.cc \
  attr/baseattr2.cc \
  attr/baseattrcomm.cc \
  attr/fkeyval.cc \
  attr/fkeyvalcomm.cc \
  attr/fkeyvaltype.cc \
  attr/keyval_double_free.cc \
  coll/allgather2.cc \
  coll/allgather3.cc \
  coll/allgatherv2.cc \
  coll/allgatherv3.cc \
  coll/allgatherv4.cc \
  coll/allred.cc \
  coll/allred2.cc \
  coll/allred3.cc \
  coll/allred4.cc \
  coll/allred5.cc \
  coll/allred6.cc \
  coll/allredmany.cc \
  coll/alltoall1.cc \
  coll/alltoallv.cc \
  coll/alltoallv0.cc \
  coll/alltoallw1.cc \
  coll/alltoallw2.cc \
  coll/alltoallw_zeros.cc \
  coll/bcast2.cc \
  coll/bcast3.cc \
  coll/bcasttest.cc \
  coll/bcastzerotype.cc \
  coll/coll10.cc \
  coll/coll11.cc \
  coll/coll12.cc \
  coll/coll13.cc \
  coll/coll2.cc \
  coll/coll3.cc \
  coll/coll4.cc \
  coll/coll5.cc \
  coll/coll6.cc \
  coll/coll7.cc \
  coll/coll8.cc \
  coll/coll9.cc \
  coll/exscan.cc \
  coll/exscan2.cc \
  coll/gather.cc \
  coll/gather2.cc \
  coll/iallred.cc \
  coll/icallgather.cc \
  coll/icallgatherv.cc \
  coll/icallreduce.cc \
  coll/icalltoall.cc \
  coll/icalltoallv.cc \
  coll/icalltoallw.cc \
  coll/icbarrier.cc \
  coll/icbcast.cc \
  coll/icgather.cc \
  coll/icgatherv.cc \
  coll/icreduce.cc \
  coll/icscatter.cc \
  coll/icscatterv.cc \
  coll/longuser.cc \
  coll/nonblocking.cc \
  coll/nonblocking2.cc \
  coll/nonblocking3.cc \
  coll/op_commutative.cc \
  coll/opband.cc \
  coll/opbor.cc \
  coll/opbxor.cc \
  coll/opland.cc \
  coll/oplor.cc \
  coll/oplxor.cc \
  coll/opmax.cc \
  coll/opmaxloc.cc \
  coll/opmin.cc \
  coll/opminloc.cc \
  coll/opprod.cc \
  coll/opsum.cc \
  coll/red3.cc \
  coll/red4.cc \
  coll/red_scat_block.cc \
  coll/red_scat_block2.cc \
  coll/redscat.cc \
  coll/redscat2.cc \
  coll/redscat3.cc \
  coll/redscatbkinter.cc \
  coll/redscatblk3.cc \
  coll/redscatinter.cc \
  coll/reduce.cc \
  coll/reduce_local.cc \
  coll/scantst.cc \
  coll/scatter2.cc \
  coll/scatter3.cc \
  coll/scattern.cc \
  coll/scatterv.cc \
  comm/cmfree.cc \
  comm/cmsplit.cc \
  comm/cmsplit2.cc \
  comm/cmsplit_type.cc \
  comm/comm_create_group.cc \
  comm/comm_group_half.cc \
  comm/comm_group_rand.cc \
  comm/comm_idup.cc \
  comm/commcreate1.cc \
  comm/commname.cc \
  comm/ctxalloc.cc \
  comm/ctxsplit.cc \
  comm/dup.cc \
  comm/dupic.cc \
  comm/ic1.cc \
  comm/ic2.cc \
  comm/iccreate.cc \
  comm/icgroup.cc \
  comm/icm.cc \
  comm/icsplit.cc \
  comm/probe-intercomm.cc \
  datatype/blockindexed-misc.cc \
  datatype/blockindexed-zero-count.cc \
  datatype/contents.cc \
  datatype/contig-zero-count.cc \
  datatype/contigstruct.cc \
  datatype/darray-pack.cc \
  datatype/gaddress.cc \
  datatype/get-elements-pairtype.cc \
  datatype/get-elements.cc \
  datatype/getpartelm.cc \
  datatype/hindexed-zeros.cc \
  datatype/hindexed_block.cc \
  datatype/hindexed_block_contents.cc \
  datatype/indexed-misc.cc \
  datatype/lbub.cc \
  datatype/localpack.cc \
  datatype/longdouble.cc \
  datatype/lots-of-types.cc \
  datatype/pairtype-pack.cc \
  datatype/pairtype-size-extent.cc \
  datatype/segtest.cc \
  datatype/simple-commit.cc \
  datatype/simple-pack-external.cc \
  datatype/simple-pack.cc \
  datatype/simple-resized.cc \
  datatype/simple-size-extent.cc \
  datatype/sizedtypes.cc \
  datatype/slice-pack-external.cc \
  datatype/slice-pack.cc \
  datatype/struct-derived-zeros.cc \
  datatype/struct-empty-el.cc \
  datatype/struct-ezhov.cc \
  datatype/struct-no-real-types.cc \
  datatype/struct-pack.cc \
  datatype/struct-verydeep.cc \
  datatype/struct-zero-count.cc \
  datatype/subarray-pack.cc \
  datatype/subarray.cc \
  datatype/tfree.cc \
  datatype/tmatchsize.cc \
  datatype/transpose-pack.cc \
  datatype/tresized.cc \
  datatype/tresized2.cc \
  datatype/triangular-pack.cc \
  datatype/typecommit.cc \
  datatype/typefree.cc \
  datatype/typelb.cc \
  datatype/typename.cc \
  datatype/unpack.cc \
  datatype/unusual-noncontigs.cc \
  datatype/zero-blklen-vector.cc \
  datatype/zeroblks.cc \
  datatype/zeroparms.cc \
  errhan/adderr.cc \
  errhan/commcall.cc \
  errhan/errcode.cc \
  errhan/errfatal.cc \
  errhan/errmsg.cc \
  errhan/errring.cc \
  errhan/errstring.cc \
  errhan/predef_eh.cc \
  group/glpid.cc \
  group/groupcreate.cc \
  group/groupnullincl.cc \
  group/grouptest.cc \
  group/grouptest2.cc \
  group/gtranks.cc \
  group/gtranksperf.cc \
  info/infodel.cc \
  info/infodup.cc \
  info/infomany.cc \
  info/infomany2.cc \
  info/infoorder.cc \
  info/infotest.cc \
  info/infovallen.cc \
  init/attrself.cc \
  init/exitst1.cc \
  init/exitst2.cc \
  init/exitst3.cc \
  init/finalized.cc \
  init/initstat.cc \
  init/timeout.cc \
  init/version.cc \
  pt2pt/anyall.cc \
  pt2pt/bottom.cc \
  pt2pt/bsend1.cc \
  pt2pt/bsend2.cc \
  pt2pt/bsend3.cc \
  pt2pt/bsend4.cc \
  pt2pt/bsend5.cc \
  pt2pt/bsendalign.cc \
  pt2pt/bsendfrag.cc \
  pt2pt/bsendpending.cc \
  pt2pt/cancelrecv.cc \
  pt2pt/eagerdt.cc \
  pt2pt/greq1.cc \
  pt2pt/icsend.cc \
  pt2pt/inactivereq.cc \
  pt2pt/isendself.cc \
  pt2pt/isendselfprobe.cc \
  pt2pt/large_message.cc \
  pt2pt/mprobe.cc \
  pt2pt/pingping.cc \
  pt2pt/probe-unexp.cc \
  pt2pt/probenull.cc \
  pt2pt/pscancel.cc \
  pt2pt/rcancel.cc \
  pt2pt/rqfreeb.cc \
  pt2pt/rqstatus.cc \
  pt2pt/scancel.cc \
  pt2pt/scancel2.cc \
  pt2pt/sendall.cc \
  pt2pt/sendflood.cc \
  pt2pt/sendrecv1.cc \
  pt2pt/sendrecv2.cc \
  pt2pt/sendrecv3.cc \
  pt2pt/sendself.cc \
  pt2pt/waitany-null.cc \
  pt2pt/waittestnull.cc \
  topo/cartcreates.cc \
  topo/cartmap1.cc \
  topo/cartshift1.cc \
  topo/cartsuball.cc \
  topo/cartzero.cc \
  topo/dgraph_unwgt.cc \
  topo/dims1.cc \
  topo/dims2.cc \
  topo/distgraph1.cc \
  topo/graphcr.cc \
  topo/graphcr2.cc \
  topo/graphmap1.cc \
  topo/neighb_coll.cc \
  topo/topodup.cc \
  topo/topotest.cc \
  rma/accfence1.cc \
  rma/accfence2_am.cc \
  rma/accfence2.cc \
  rma/allocmem.cc \
  rma/attrorderwin.cc \
  rma/baseattrwin.cc \
  rma/contig_displ.cc \
  rma/fetchandadd_am.cc \
  rma/fetchandadd_tree_am.cc \
  rma/fetchandadd_tree.cc \
  rma/fetchandadd.cc \
  rma/fkeyvalwin.cc \
  rma/getfence1.cc \
  rma/getgroup.cc \
  rma/ircpi.cc \
  rma/lockcontention.cc \
  rma/locknull.cc \
  rma/mixedsync.cc \
  rma/nullpscw.cc \
  rma/putfence1.cc \
  rma/putfidx.cc \
  rma/putpscw1.cc \
  rma/selfrma.cc \
  rma/test1_am.cc \
  rma/test1.cc \
  rma/test2_am.cc \
  rma/test2.cc \
  rma/test3_am.cc \
  rma/test3.cc \
  rma/test4_am.cc \
  rma/test4.cc \
  rma/test5_am.cc \
  rma/test5.cc \
  rma/transpose1.cc \
  rma/transpose2.cc \
  rma/transpose3.cc \
  rma/transpose4.cc \
  rma/transpose5.cc \
  rma/transpose6.cc \
  rma/transpose7.cc \
  rma/wincall.cc \
  rma/winname.cc \
  rma/wintest.cc 
EXTRA_DIST += $(TEST_SOURCE_FILES)



