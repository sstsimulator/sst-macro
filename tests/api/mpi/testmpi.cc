/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/replacements/mpi.h>
#include <sstmac/replacements/sys/time.h>
#include <sstmac/util.h>
#include <sstmac/skeleton.h>
#include <sstmac/common/runtime.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

using namespace sstmac;

double get_time()
{
  timeval t_st;
  gettimeofday(&t_st, 0);
  double t = t_st.tv_sec + 1e-6 * t_st.tv_usec;
  return t;
}

enum TEST_MODE
{

  //ATTR_ATTR2TYPE = 0,
  //ATTR_ATTREND = 1,
  ATTR_ATTREND2 = 2,
  //ATTR_ATTRERR = 3,
  //ATTR_ATTRERRCOMM = 4,
  //ATTR_ATTRERRTYPE = 5,
  //ATTR_ATTRIC = 6,
  //ATTR_ATTRORDER = 7,
  //ATTR_ATTRORDERCOMM = 8,
  //ATTR_ATTRORDERTYPE = 9,
  //ATTR_ATTRT = 10,
  //ATTR_BASEATTR2 = 11,
  //ATTR_BASEATTRCOMM = 12,
  //ATTR_FKEYVAL = 13,
  //ATTR_FKEYVALCOMM = 14,
  //ATTR_FKEYVALTYPE = 15,
  ATTR_KEYVAL_DOUBLE_FREE = 16,
  COLL_ALLGATHER2 = 17,
  COLL_ALLGATHER3 = 18,
  COLL_ALLGATHERV2 = 19,
  COLL_ALLGATHERV3 = 20,
  COLL_ALLGATHERV4 = 21,
  COLL_ALLRED = 22,
  COLL_ALLRED2 = 23,
  COLL_ALLRED5 = 26,
  COLL_ALLRED6 = 27,
  COLL_ALLREDMANY = 28,
  COLL_ALLTOALL1 = 29,
  COLL_ALLTOALLV = 30,
  COLL_ALLTOALLV0 = 31,
  //COLL_ALLTOALLW1 = 32,
  //COLL_ALLTOALLW2 = 33,
  //COLL_ALLTOALLW_ZEROS = 34,
  COLL_BCAST2 = 35, //this guy takes a while, eats up a lot of memory too
  COLL_BCAST3 = 36,
  COLL_BCASTTEST = 37,
  COLL_BCASTZEROTYPE = 38,
  COLL_COLL12 = 41,
  COLL_COLL13 = 42,
  COLL_COLL2 = 43,
  COLL_COLL3 = 44,
  COLL_COLL4 = 45,
  COLL_COLL5 = 46,
  COLL_COLL6 = 47,
  COLL_COLL7 = 48,
  COLL_COLL8 = 49,
  COLL_COLL9 = 50,
  //COLL_EXSCAN = 51,
  //COLL_EXSCAN2 = 52,
  COLL_GATHER = 53,
  COLL_GATHER2 = 54,
  COLL_IALLRED = 55,
  //COLL_ICALLGATHER = 56,
  //COLL_ICALLGATHERV = 57,
  //COLL_ICALLREDUCE = 58,
  //COLL_ICALLTOALL = 59,
  //COLL_ICALLTOALLV = 60,
  //COLL_ICALLTOALLW = 61,
  //COLL_ICBARRIER = 62,
  //COLL_ICBCAST = 63,
  //COLL_ICGATHER = 64,
  //COLL_ICGATHERV = 65,
  //COLL_ICREDUCE = 66,
  //COLL_ICSCATTER = 67,
  //COLL_ICSCATTERV = 68,
  //COLL_LONGUSER = 69,
  //COLL_OP_COMMUTATIVE = 73,
  COLL_OPBAND = 74,
  COLL_OPBOR = 75,
  COLL_OPBXOR = 76,
  COLL_OPLAND = 77,
  COLL_OPLOR = 78,
  COLL_OPLXOR = 79,
  COLL_OPMAX = 80,
  COLL_OPMAXLOC = 81,
  COLL_OPMIN = 82,
  COLL_OPMINLOC = 83,
  COLL_OPPROD = 84,
  COLL_OPSUM = 85,
  //COLL_RED_SCAT_BLOCK = 88,
  //COLL_RED_SCAT_BLOCK2 = 89,
  COLL_REDSCAT = 90,
  COLL_REDSCAT3 = 92,
  //COLL_REDSCATBKINTER = 93,
  //COLL_REDSCATBLK3 = 94,
  //COLL_REDSCATINTER = 95,
  COLL_REDUCE = 96,
  COLL_REDUCE_LOCAL = 97,
  COLL_SCATTER2 = 99,
  COLL_SCATTER3 = 100,
  COLL_SCATTERN = 101,
  //COLL_SCATTERV = 102,
  COMM_CMFREE = 103,
  COMM_CMSPLIT = 104,
  //COMM_CMSPLIT2 = 105,
  //COMM_CMSPLIT_TYPE = 106,
  //COMM_COMM_CREATE_GROUP = 107,
  //COMM_COMM_GROUP_HALF = 108,
  //COMM_COMM_GROUP_RAND = 109,
  COMM_COMM_IDUP = 110,
  //COMM_COMMCREATE1 = 111,
  //COMM_COMMNAME = 112,
  COMM_CTXALLOC = 113,
  COMM_CTXSPLIT = 114,
  COMM_DUP = 115,
  //COMM_DUPIC = 116,
  //COMM_IC1 = 117,
  //COMM_IC2 = 118,
  //COMM_ICCREATE = 119,
  //COMM_ICGROUP = 120,
  //COMM_ICM = 121,
  //COMM_ICSPLIT = 122,
  //COMM_PROBE_INTERCOMM = 123,
  //DATATYPE_BLOCKINDEXED_MISC = 124,
  //DATATYPE_BLOCKINDEXED_ZERO_COUNT = 125,
  //DATATYPE_CONTENTS = 126,
  //DATATYPE_CONTIG_ZERO_COUNT = 127,
  //DATATYPE_CONTIGSTRUCT = 128,
  //DATATYPE_DARRAY_PACK = 129,
  //DATATYPE_GADDRESS = 130,
  //DATATYPE_GET_ELEMENTS_PAIRTYPE = 131,
  //DATATYPE_GET_ELEMENTS = 132,
  //DATATYPE_GETPARTELM = 133,
  //DATATYPE_HINDEXED_ZEROS = 134,
  //DATATYPE_HINDEXED_BLOCK = 135,
  //DATATYPE_HINDEXED_BLOCK_CONTENTS = 136,
  //DATATYPE_INDEXED_MISC = 137,
  //DATATYPE_LBUB = 138,
  //DATATYPE_LOCALPACK = 139,
  //DATATYPE_LONGDOUBLE = 140,
  //DATATYPE_LOTS_OF_TYPES = 141,
  //DATATYPE_PAIRTYPE_PACK = 142,
  //DATATYPE_PAIRTYPE_SIZE_EXTENT = 143,
  //  DATATYPE_SEGTEST = 144,
  //DATATYPE_SIMPLE_COMMIT = 145,
  //DATATYPE_SIMPLE_PACK_EXTERNAL = 146,
  //DATATYPE_SIMPLE_PACK = 147,
  //DATATYPE_SIMPLE_RESIZED = 148,
  //DATATYPE_SIMPLE_SIZE_EXTENT = 149,
  //DATATYPE_SIZEDTYPES = 150,
  //DATATYPE_SLICE_PACK_EXTERNAL = 151,
  //DATATYPE_SLICE_PACK = 152,
  //DATATYPE_STRUCT_DERIVED_ZEROS = 153,
  //DATATYPE_STRUCT_EMPTY_EL = 154,
  //DATATYPE_STRUCT_EZHOV = 155,
  //DATATYPE_STRUCT_NO_REAL_TYPES = 156,
  //DATATYPE_STRUCT_PACK = 157,
  //DATATYPE_STRUCT_VERYDEEP = 158,
  //DATATYPE_STRUCT_ZERO_COUNT = 159,
  //DATATYPE_SUBARRAY_PACK = 160,
  //DATATYPE_SUBARRAY = 161,
  //DATATYPE_TFREE = 162,
  //DATATYPE_TMATCHSIZE = 163,
  //DATATYPE_TRANSPOSE_PACK = 164,
  //DATATYPE_TRESIZED = 165,
 //DATATYPE_TRESIZED2 = 166,
  //DATATYPE_TRIANGULAR_PACK = 167,
  //DATATYPE_TYPECOMMIT = 168,
  //DATATYPE_TYPEFREE = 169,
  //DATATYPE_TYPELB = 170,
  //DATATYPE_TYPENAME = 171,
  //DATATYPE_UNPACK = 172,
  //DATATYPE_UNUSUAL_NONCONTIGS = 173,
  //DATATYPE_ZERO_BLKLEN_VECTOR = 174,
  //DATATYPE_ZEROBLKS = 175,
  //DATATYPE_ZEROPARMS = 176,
  /*** ERRHAN_ADDERR = 177,
  ERRHAN_COMMCALL = 178,
  ERRHAN_ERRCODE = 179,
  ERRHAN_ERRFATAL = 180,
  //  ERRHAN_ERRMSG = 181,
  //  ERRHAN_ERRRING = 182,
  //  ERRHAN_ERRSTRING = 183,
  //  ERRHAN_PREDEF_EH = 184,
  // GROUP_GLPID = 185,
  GROUP_GROUPCREATE = 186,
  GROUP_GROUPNULLINCL = 187,
  GROUP_GROUPTEST = 188,
  GROUP_GROUPTEST2 = 189,
  GROUP_GTRANKS = 190,
  //GROUP_GTRANKSPERF = 191,
  INFO_INFODEL = 192,
  INFO_INFODUP = 193,
  INFO_INFOMANY = 194,
  INFO_INFOMANY2 = 195,
  INFO_INFOORDER = 196,
  INFO_INFOTEST = 197,
  INFO_INFOVALLEN = 198,
  INIT_ATTRSELF = 199,
  INIT_EXITST1 = 200,
  INIT_EXITST2 = 201,
  INIT_EXITST3 = 202,
  INIT_FINALIZED = 203,
  INIT_INITSTAT = 204,
  // INIT_TIMEOUT = 205,
  INIT_VERSION = 206, */
  PT2PT_ANYALL = 207,
  PT2PT_BOTTOM = 208,
  PT2PT_BSEND1 = 209,
  PT2PT_BSEND2 = 210,
  //PT2PT_BSEND3 = 211,
  PT2PT_BSEND4 = 212,
  //PT2PT_BSEND5 = 213,
  PT2PT_BSENDALIGN = 214,
  PT2PT_BSENDFRAG = 215,
  PT2PT_BSENDPENDING = 216,
  //PT2PT_CANCELRECV = 217,
  //PT2PT_EAGERDT = 218,
  //PT2PT_GREQ1 = 219,
  //PT2PT_ICSEND = 220,
  //PT2PT_INACTIVEREQ = 221,
  PT2PT_ISENDSELF = 222,
  //  PT2PT_ISENDSELFPROBE = 223,
  PT2PT_LARGE_MESSAGE = 224,
  PT2PT_MPROBE = 225,
  PT2PT_PINGPING = 226,
  PT2PT_PROBE_UNEXP = 227,
  PT2PT_PROBENULL = 228,
  //PT2PT_PSCANCEL = 229,
  //PT2PT_RCANCEL = 230,
  //PT2PT_RQFREEB = 231,
  //PT2PT_RQSTATUS = 232,
  //PT2PT_SCANCEL = 233,
  //PT2PT_SCANCEL2 = 234,
  PT2PT_SENDALL = 235,
  PT2PT_SENDFLOOD = 236,
  PT2PT_SENDRECV1 = 237,
  PT2PT_SENDRECV2 = 238,
  PT2PT_SENDRECV3 = 239,
  PT2PT_SENDSELF = 240,
  PT2PT_WAITANY_NULL = 241,
  PT2PT_WAITTESTNULL = 242,
  /*** TOPO_CARTCREATES = 243,
  TOPO_CARTMAP1 = 244,
  TOPO_CARTSHIFT1 = 245,
  TOPO_CARTSUBALL = 246,
  TOPO_CARTZERO = 247,
  TOPO_DGRAPH_UNWGT = 248,
  TOPO_DIMS1 = 249,
  TOPO_DIMS2 = 250,
  TOPO_DISTGRAPH1 = 251,
  TOPO_GRAPHCR = 252,
  TOPO_GRAPHCR2 = 253,
  TOPO_GRAPHMAP1 = 254,
  TOPO_NEIGHB_COLL = 255,
  TOPO_TOPODUP = 256,
  TOPO_TOPOTEST = 257,
  RMA_ACCFENCE1 = 258,
  RMA_ACCFENCE2_AM = 259,
  RMA_ACCFENCE2 = 260,
  RMA_ALLOCMEM = 261,
  RMA_ATTRORDERWIN = 262,
  RMA_BASEATTRWIN = 263,
  RMA_CONTIG_DISPL = 264,
  RMA_FETCHANDADD_AM = 265,
  RMA_FETCHANDADD_TREE_AM = 266,
  RMA_FETCHANDADD_TREE = 267,
  RMA_FETCHANDADD = 268,
  RMA_FKEYVALWIN = 269,
  RMA_GETFENCE1 = 270,
  RMA_GETGROUP = 271,
  RMA_IRCPI = 272,
  RMA_LOCKCONTENTION = 273,
  RMA_LOCKNULL = 274,
  RMA_MIXEDSYNC = 275,
  RMA_NULLPSCW = 276,
  RMA_PUTFENCE1 = 277,
  RMA_PUTFIDX = 278,
  RMA_PUTPSCW1 = 279,
  RMA_SELFRMA = 280,
  RMA_TEST1_AM = 281,
  RMA_TEST1 = 282,
  RMA_TEST2_AM = 283,
  RMA_TEST2 = 284,
  RMA_TEST3_AM = 285,
  RMA_TEST3 = 286,
  RMA_TEST4_AM = 287,
  RMA_TEST4 = 288,
  RMA_TEST5_AM = 289,
  RMA_TEST5 = 290,
  RMA_TRANSPOSE1 = 291,
  RMA_TRANSPOSE2 = 292,
  RMA_TRANSPOSE3 = 293,
  RMA_TRANSPOSE4 = 294,
  RMA_TRANSPOSE5 = 295,
  RMA_TRANSPOSE6 = 296,
  RMA_TRANSPOSE7 = 297,
  RMA_WINCALL = 298,
  RMA_WINNAME = 299,
  RMA_WINTEST = 300, */
  TEST_MODE_END = 301

};

//-------- attr ---------//
/*** TODO
#include "attr/attr2type.cc"
#include "attr/attrend.cc"
#include "attr/attrerr.cc"
#include "attr/attrerrcomm.cc"
#include "attr/attrerrtype.cc"
#include "attr/attric.cc"
#include "attr/attrorder.cc"
#include "attr/attrordercomm.cc"
#include "attr/attrordertype.cc"
#include "attr/attrt.cc"
#include "attr/baseattr2.cc"
#include "attr/baseattrcomm.cc"
#include "attr/fkeyval.cc"
#include "attr/fkeyvalcomm.cc"
#include "attr/fkeyvaltype.cc"
#include "attr/keyval_double_free.cc"
*/

#include "attr/attrend2.cc"











//----------- coll ----------------//
#include "coll/allgather2.cc"
#include "coll/allgather3.cc"
#include "coll/allgatherv2.cc"
#include "coll/allgatherv3.cc"
#include "coll/allgatherv4.cc"
#include "coll/allred.cc"
#include "coll/allred2.cc"
#include "coll/allred5.cc"
#include "coll/allred6.cc"
#include "coll/allredmany.cc"
#include "coll/alltoall1.cc"
#include "coll/alltoallv.cc"
#include "coll/alltoallv0.cc"
//TODO #include "coll/alltoallw1.cc"
//TODO #include "coll/alltoallw2.cc"
//TODO #include "coll/alltoallw_zeros.cc"
#include "coll/bcast2.cc"
#include "coll/bcast3.cc"
#include "coll/bcasttest.cc"
#include "coll/bcastzerotype.cc"
#include "coll/coll12.cc"
#include "coll/coll13.cc"
#include "coll/coll2.cc"
#include "coll/coll3.cc"
#include "coll/coll4.cc"
#include "coll/coll5.cc"
#include "coll/coll6.cc"
#include "coll/coll7.cc"
#include "coll/coll8.cc"
#include "coll/coll9.cc"
//#include "coll/exscan.cc"
//#include "coll/exscan2.cc"
#include "coll/gather.cc"
#include "coll/gather2.cc"
#include "coll/iallred.cc"
//#include "coll/icallgather.cc"
//#include "coll/icallgatherv.cc"
//#include "coll/icallreduce.cc"
//#include "coll/icalltoall.cc"
//#include "coll/icalltoallv.cc"
//#include "coll/icalltoallw.cc"
//#include "coll/icbarrier.cc"
//#include "coll/icbcast.cc"
//#include "coll/icgather.cc"
//#include "coll/icgatherv.cc"
//#include "coll/icreduce.cc"
//#include "coll/icscatter.cc"
//#include "coll/icscatterv.cc"
//#include "coll/longuser.cc"
//#include "coll/op_commutative.cc"
#include "coll/opband.cc"
#include "coll/opbor.cc"
#include "coll/opbxor.cc"
#include "coll/opland.cc"
#include "coll/oplor.cc"
#include "coll/oplxor.cc"
#include "coll/opmax.cc"
#include "coll/opmaxloc.cc"
#include "coll/opmin.cc"
#include "coll/opminloc.cc"
#include "coll/opprod.cc"
#include "coll/opsum.cc"
#include "coll/red_scat_block.cc"
#include "coll/redscat.cc"
#include "coll/redscat3.cc"
//#include "coll/redscatbkinter.cc"
//#include "coll/redscatblk3.cc"
//#include "coll/redscatinter.cc"
#include "coll/reduce.cc"
#include "coll/scatter2.cc"
#include "coll/scatter3.cc"
#include "coll/scattern.cc"
//#include "coll/scatterv.cc"

//-------- comm -------------------//
#include "comm/cmfree.cc"
#include "comm/cmsplit.cc"
//group size and group translate ranks #include "comm/cmsplit2.cc"
//group excl and group free #include "comm/cmsplit_type.cc"
//group excl and group free #include "comm/comm_create_group.cc"
//group range incl and group free #include "comm/comm_group_half.cc"
//error class and group free #include "comm/comm_group_rand.cc"
#include "comm/comm_idup.cc"
//group_range_incl #include "comm/commcreate1.cc"
//comm get name comm set name #include "comm/commname.cc"
#include "comm/ctxalloc.cc"
#include "comm/ctxsplit.cc"
#include "comm/dup.cc"
//intercomm #include "comm/dupic.cc"
//intercomm stuff #include "comm/ic1.cc"
//intercomm stuff #include "comm/ic2.cc"
//intercomm stuff #include "comm/iccreate.cc"
//intercomm stuff #include "comm/icgroup.cc"
//intercomm stuff #include "comm/icm.cc"
//intercomm stuff #include "comm/icsplit.cc"
//intercomm stuff #include "comm/probe-intercomm.cc"

// ---------------- datatype ---------------- //
//type create indexed block #include "datatype/blockindexed-misc.cc"
//type create indexed block #include "datatype/blockindexed-zero-count.cc"
//type create indexed block #include "datatype/contents.cc"
#include "datatype/contig-zero-count.cc"
//type create darray, pack size #include "datatype/contigstruct.cc"
//type create darray, pack size #include "datatype/darray-pack.cc"
//get_address #include "datatype/gaddress.cc"
//get_elements #include "datatype/get-elements-pairtype.cc"
//get_elements #include "datatype/get-elements.cc"
//pack, get_elements, #include "datatype/getpartelm.cc"
//get count #include "datatype/hindexed-zeros.cc"
// #include "datatype/hindexed_block.cc"
// #include "datatype/hindexed_block_contents.cc"
// #include "datatype/indexed-misc.cc"
// #include "datatype/lbub.cc"
// #include "datatype/localpack.cc"
// #include "datatype/longdouble.cc"
// #include "datatype/lots-of-types.cc"
// #include "datatype/pairtype-pack.cc"
// #include "datatype/pairtype-size-extent.cc"
//#include "datatype/segtest.cc"
// #include "datatype/simple-commit.cc"
// #include "datatype/simple-pack-external.cc"
// #include "datatype/simple-pack.cc"
// #include "datatype/simple-resized.cc"
// #include "datatype/simple-size-extent.cc"
// #include "datatype/sizedtypes.cc"
// #include "datatype/slice-pack-external.cc"
// #include "datatype/slice-pack.cc"
// #include "datatype/struct-derived-zeros.cc"
// #include "datatype/struct-empty-el.cc"
// #include "datatype/struct-ezhov.cc"
// #include "datatype/struct-no-real-types.cc"
// #include "datatype/struct-pack.cc"
// #include "datatype/struct-verydeep.cc"
// #include "datatype/struct-zero-count.cc"
// #include "datatype/subarray-pack.cc"
// #include "datatype/subarray.cc"
// #include "datatype/tfree.cc"
// #include "datatype/tmatchsize.cc"
// #include "datatype/transpose-pack.cc"
// #include "datatype/tresized.cc"
// #include "datatype/tresized2.cc"
// #include "datatype/triangular-pack.cc"
// #include "datatype/typecommit.cc"
// #include "datatype/typefree.cc"
// #include "datatype/typelb.cc"
// #include "datatype/typename.cc"
// #include "datatype/unpack.cc"
// #include "datatype/unusual-noncontigs.cc"
// #include "datatype/zero-blklen-vector.cc"
// #include "datatype/zeroblks.cc"
// #include "datatype/zeroparms.cc"

// ------------------ errhan ------------- //
// #include "errhan/adderr.cc"
// #include "errhan/commcall.cc"
//#include "errhan/errcode.cc"
//#include "errhan/errfatal.cc"
//#include "errhan/errmsg.cc"
//#include "errhan/errring.cc"
//#include "errhan/errstring.cc"
//#include "errhan/predef_eh.cc"

// --------------- group --------------- //
//#include "group/glpid.cc"
//group size #include "group/groupcreate.cc"
//group compare #include "group/groupnullincl.cc"
//group size, group rank #include "group/grouptest.cc"
//group size, group rank #include "group/grouptest2.cc"
//group translate ranks #include "group/gtranks.cc"
//group translate ranks #include "group/gtranksperf.cc"

// --------------- info -------------- //
//#include "info/infodel.cc"
//#include "info/infodup.cc"
//#include "info/infomany.cc"
//#include "info/infomany2.cc"
//#include "info/infoorder.cc"
//#include "info/infotest.cc"
//#include "info/infovallen.cc"

// --------------- init ----------- //
//#include "init/attrself.cc"
//#include "init/exitst1.cc"
//#include "init/exitst2.cc"
//#include "init/exitst3.cc"
//#include "init/finalized.cc"
//#include "init/initstat.cc"
//#include "init/timeout.cc"
//#include "init/version.cc"

// -------------- pt2pt ----------- //
#include "pt2pt/anyall.cc"
#include "pt2pt/bottom.cc"
#include "pt2pt/bsend1.cc"
#include "pt2pt/bsend2.cc"
#include "pt2pt/bsend3.cc"
#include "pt2pt/bsend4.cc"
//intercomm #include "pt2pt/bsend5.cc"
#include "pt2pt/bsendalign.cc"
#include "pt2pt/bsendfrag.cc"
#include "pt2pt/bsendpending.cc"
//init/cancel #include "pt2pt/cancelrecv.cc"
//type create indexed block #include "pt2pt/eagerdt.cc"
//cancel #include "pt2pt/greq1.cc"
//comm remote size #include "pt2pt/icsend.cc"
//send init #include "pt2pt/inactivereq.cc"
#include "pt2pt/isendself.cc"
#include "pt2pt/isendselfprobe.cc"
#include "pt2pt/large_message.cc"
#include "pt2pt/mprobe.cc"
#include "pt2pt/pingping.cc"
#include "pt2pt/probe-unexp.cc"
#include "pt2pt/probenull.cc"
//send init #include "pt2pt/pscancel.cc"
//cancel #include "pt2pt/rcancel.cc"
//Request_free, Request_get_status #include "pt2pt/rqfreeb.cc"
//Request_free, Request_get_status  #include "pt2pt/rqstatus.cc"
//cancel #include "pt2pt/scancel.cc"
//cancel #include "pt2pt/scancel2.cc"
#include "pt2pt/sendall.cc"
#include "pt2pt/sendflood.cc"
#include "pt2pt/sendrecv1.cc"
#include "pt2pt/sendrecv2.cc"
#include "pt2pt/sendrecv3.cc"
#include "pt2pt/sendself.cc"
#include "pt2pt/waitany-null.cc"
#include "pt2pt/waittestnull.cc"

/*** no topo
// --------------- topo ------------ //
#include "topo/cartcreates.cc"
#include "topo/cartmap1.cc"
#include "topo/cartshift1.cc"
#include "topo/cartsuball.cc"
#include "topo/cartzero.cc"
#include "topo/dgraph_unwgt.cc"
#include "topo/dims1.cc"
#include "topo/dims2.cc"
#include "topo/distgraph1.cc"
#include "topo/graphcr.cc"
#include "topo/graphcr2.cc"
#include "topo/graphmap1.cc"
#include "topo/neighb_coll.cc"
#include "topo/topodup.cc"
#include "topo/topotest.cc"
*/

// -------------- RMA ------------ //
/*** no RMA
#include "rma/accfence1.cc"
#include "rma/accfence2_am.cc"
#include "rma/accfence2.cc"
#include "rma/allocmem.cc"
#include "rma/attrorderwin.cc"
#include "rma/baseattrwin.cc"
#include "rma/contig_displ.cc"
#include "rma/fetchandadd_am.cc"
#include "rma/fetchandadd_tree_am.cc"
#include "rma/fetchandadd_tree.cc"
#include "rma/fetchandadd.cc"
#include "rma/fkeyvalwin.cc"
#include "rma/getfence1.cc"
#include "rma/getgroup.cc"
#include "rma/ircpi.cc"
#include "rma/lockcontention.cc"
#include "rma/locknull.cc"
#include "rma/mixedsync.cc"
#include "rma/nullpscw.cc"
#include "rma/putfence1.cc"
#include "rma/putfidx.cc"
#include "rma/putpscw1.cc"
#include "rma/selfrma.cc"
#include "rma/test1_am.cc"
#include "rma/test1.cc"
#include "rma/test2_am.cc"
#include "rma/test2.cc"
#include "rma/test3_am.cc"
#include "rma/test3.cc"
#include "rma/test4_am.cc"
#include "rma/test4.cc"
#include "rma/test5_am.cc"
#include "rma/test5.cc"
#include "rma/transpose1.cc"
#include "rma/transpose2.cc"
#include "rma/transpose3.cc"
#include "rma/transpose4.cc"
#include "rma/transpose5.cc"
#include "rma/transpose6.cc"
#include "rma/transpose7.cc"
#include "rma/wincall.cc"
#include "rma/winname.cc"
#include "rma/wintest.cc"
*/

int testmode_ = -1;

const char* valid_keywords[] = {
"testsuite_testmode",
"testsuite_numtests" };

sprockit::StaticKeywordRegister reg(sizeof(valid_keywords) / sizeof(const char*), valid_keywords);

#define sstmac_app_name apitest

int USER_MAIN(int argc, char *argv[])
{
  sprockit::sim_parameters* params = get_params();
  testmode_ = params->get_int_param("testsuite_testmode");

  sstmac::runtime::add_deadlock_check(
    sstmac::new_deadlock_check(sumi::sstmac_mpi(), &sumi::transport::deadlock_check));
  sstmac::runtime::enter_deadlock_region();

  double t_start = get_time();

  switch (testmode_)
  {
  /** case ATTR_ATTR2TYPE:
    attr2type::attr2type(argc, argv);
    break; */
  /** case ATTR_ATTREND:
    attrend::attrend(argc, argv);
    break; */
  case ATTR_ATTREND2:
    attrend2::attrend2(argc, argv);
    break;
  /*** case ATTR_ATTRERR:
    attrerr::attrerr(argc, argv);
    break;
  case ATTR_ATTRERRCOMM:
    attrerrcomm::attrerrcomm(argc, argv);
    break;
  case ATTR_ATTRERRTYPE:
    attrerrtype::attrerrtype(argc, argv);
    break;
  case ATTR_ATTRIC:
    attric::attric(argc, argv);
    break;
  case ATTR_ATTRORDER:
    attrorder::attrorder(argc, argv);
    break;
  case ATTR_ATTRORDERCOMM:
    attrordercomm::attrordercomm(argc, argv);
    break;
  case ATTR_ATTRORDERTYPE:
    attrordertype::attrordertype(argc, argv);
    break;
  case ATTR_ATTRT:
    attrt::attrt(argc, argv);
    break;
  case ATTR_BASEATTR2:
    baseattr2::baseattr2(argc, argv);
    break;
  case ATTR_BASEATTRCOMM:
    baseattrcomm::baseattrcomm(argc, argv);
    break;
  case ATTR_FKEYVAL:
    fkeyval::fkeyval(argc, argv);
    break;
  case ATTR_FKEYVALCOMM:
    fkeyvalcomm::fkeyvalcomm(argc, argv);
    break;
  case ATTR_FKEYVALTYPE:
    fkeyvaltype::fkeyvaltype(argc, argv);
    break;
  case ATTR_KEYVAL_DOUBLE_FREE:
    keyval_double_free::keyval_double_free(argc, argv);
    break; */
  case COLL_ALLGATHER2:
    allgather2::allgather2(argc, argv);
    break;
  case COLL_ALLGATHER3:
    allgather3::allgather3(argc, argv);
    break;
  case COLL_ALLGATHERV2:
    allgatherv2::allgatherv2(argc, argv);
    break;
  case COLL_ALLGATHERV3:
    allgatherv3::allgatherv3(argc, argv);
    break;
  case COLL_ALLGATHERV4:
    allgatherv4::allgatherv4(argc, argv);
    break;
  case COLL_ALLRED:
    allred::allred(argc, argv);
    break;
  case COLL_ALLRED2:
    allred2::allred2(argc, argv);
    break;
  case COLL_ALLRED5:
    allred5::allred5(argc, argv);
    break;
  case COLL_ALLRED6:
    allred6::allred6(argc, argv);
    break;
  case COLL_ALLREDMANY:
    allredmany::allredmany(argc, argv);
    break;
  case COLL_ALLTOALL1:
    alltoall1::alltoall1(argc, argv);
    break;
  case COLL_ALLTOALLV:
    alltoallv::alltoallv(argc, argv);
    break;
  case COLL_ALLTOALLV0:
    alltoallv0::alltoallv0(argc, argv);
    break;
  /*** case COLL_ALLTOALLW1:
    alltoallw1::alltoallw1(argc, argv);
    break;
  case COLL_ALLTOALLW2:
    alltoallw2::alltoallw2(argc, argv);
    break;
  case COLL_ALLTOALLW_ZEROS:
    alltoallw_zeros::alltoallw_zeros(argc, argv);
    break; */
  case COLL_BCAST2:
    bcast2::bcast2(argc, argv);
    break;
  case COLL_BCAST3:
    bcast3::bcast3(argc, argv);
    break;
  case COLL_BCASTTEST:
    bcasttest::bcasttest(argc, argv);
    break;
  case COLL_BCASTZEROTYPE:
    bcastzerotype::bcastzerotype(argc, argv);
    break;
  /*** case COLL_COLL10:
    coll10::coll10(argc, argv);
    break;
  case COLL_COLL11:
    coll11::coll11(argc, argv);
    break; */
  case COLL_COLL12:
    coll12::coll12(argc, argv);
    break;
  case COLL_COLL13:
    coll13::coll13(argc, argv);
    break;
  case COLL_COLL2:
    coll2::coll2(argc, argv);
    break;
  case COLL_COLL3:
    coll3::coll3(argc, argv);
    break;
  case COLL_COLL4:
    coll4::coll4(argc, argv);
    break;
  case COLL_COLL5:
    coll5::coll5(argc, argv);
    break;
  case COLL_COLL6:
    coll6::coll6(argc, argv);
    break;
  case COLL_COLL7:
    coll7::coll7(argc, argv);
    break;
  case COLL_COLL8:
    coll8::coll8(argc, argv);
    break;
  case COLL_COLL9:
    coll9::coll9(argc, argv);
    break;
  /***
  case COLL_EXSCAN:
    exscan::exscan(argc, argv);
    break;
  case COLL_EXSCAN2:
    exscan2::exscan2(argc, argv);
    break; */
  case COLL_GATHER:
    gather::gather(argc, argv);
    break;
  case COLL_GATHER2:
    gather2::gather2(argc, argv);
    break;
  case COLL_IALLRED:
    iallred::iallred(argc, argv);
    break;
  /*** case COLL_ICALLGATHER:
    icallgather::icallgather(argc, argv);
    break;
  case COLL_ICALLGATHERV:
    icallgatherv::icallgatherv(argc, argv);
    break;
  case COLL_ICALLREDUCE:
    icallreduce::icallreduce(argc, argv);
    break;
  case COLL_ICALLTOALL:
    icalltoall::icalltoall(argc, argv);
    break;
  case COLL_ICALLTOALLV:
    icalltoallv::icalltoallv(argc, argv);
    break;
  case COLL_ICALLTOALLW:
    icalltoallw::icalltoallw(argc, argv);
    break;
  case COLL_ICBARRIER:
    icbarrier::icbarrier(argc, argv);
    break;
  case COLL_ICBCAST:
    icbcast::icbcast(argc, argv);
    break;
  case COLL_ICGATHER:
    icgather::icgather(argc, argv);
    break;
  case COLL_ICGATHERV:
    icgatherv::icgatherv(argc, argv);
    break;
  case COLL_ICREDUCE:
    icreduce::icreduce(argc, argv);
    break;
  case COLL_ICSCATTER:
    icscatter::icscatter(argc, argv);
    break;
  case COLL_ICSCATTERV:
    icscatterv::icscatterv(argc, argv);
    break; */
  case COLL_OPBAND:
    opband::opband(argc, argv);
    break;
  case COLL_OPBOR:
    opbor::opbor(argc, argv);
    break;
  case COLL_OPBXOR:
    opbxor::opbxor(argc, argv);
    break;
  case COLL_OPLAND:
    opland::opland(argc, argv);
    break;
  case COLL_OPLOR:
    oplor::oplor(argc, argv);
    break;
  case COLL_OPLXOR:
    oplxor::oplxor(argc, argv);
    break;
  case COLL_OPMAX:
    opmax::opmax(argc, argv);
    break;
  case COLL_OPMAXLOC:
    opmaxloc::opmaxloc(argc, argv);
    break;
  case COLL_OPMIN:
    opmin::opmin(argc, argv);
    break;
  case COLL_OPMINLOC:
    opminloc::opminloc(argc, argv);
    break;
  case COLL_OPPROD:
    opprod::opprod(argc, argv);
    break;
  case COLL_OPSUM:
    opsum::opsum(argc, argv);
    break;
  /*** case COLL_RED_SCAT_BLOCK:
    red_scat_block::red_scat_block(argc, argv);
    break;
  case COLL_REDSCAT:
    redscat::redscat(argc, argv);
    break;
  case COLL_REDSCAT3:
    redscat3::redscat3(argc, argv);
    break; */
  /*** case COLL_REDSCATBKINTER:
    redscatbkinter::redscatbkinter(argc, argv);
    break;
  case COLL_REDSCATBLK3:
    redscatblk3::redscatblk3(argc, argv);
    break;
  case COLL_REDSCATINTER:
    redscatinter::redscatinter(argc, argv);
    break; */
  case COLL_REDUCE:
    reduce::reduce(argc, argv);
    break;
  case COLL_SCATTER2:
    scatter2::scatter2(argc, argv);
    break;
  case COLL_SCATTER3:
    scatter3::scatter3(argc, argv);
    break;
  case COLL_SCATTERN:
    scattern::scattern(argc, argv);
    break;
  /*** case COLL_SCATTERV:
    scatterv::scatterv(argc, argv);
    break; */
  case COMM_CMFREE:
    cmfree::cmfree(argc, argv);
    break;
  case COMM_CMSPLIT:
    cmsplit::cmsplit(argc, argv);
    break;
  /*** case COMM_CMSPLIT2:
    cmsplit2::cmsplit2(argc, argv);
    break;
  case COMM_CMSPLIT_TYPE:
    cmsplit_type::cmsplit_type(argc, argv);
    break;
  case COMM_COMM_CREATE_GROUP:
    comm_create_group::comm_create_group(argc, argv);
    break;
  case COMM_COMM_GROUP_HALF:
    comm_group_half::comm_group_half(argc, argv);
    break;
  case COMM_COMM_GROUP_RAND:
    comm_group_rand::comm_group_rand(argc, argv);
    break; */
  case COMM_COMM_IDUP:
    comm_idup::comm_idup(argc, argv);
    break;
  /*** case COMM_COMMCREATE1:
    commcreate1::commcreate1(argc, argv);
    break;
  case COMM_COMMNAME:
    commname::commname(argc, argv);
    break; */
  case COMM_CTXALLOC:
    // ctxalloc::ctxalloc(argc, argv);       //------------should be fixed, takes up a lot of memory
    break;
  case COMM_CTXSPLIT:
    //ctxsplit::ctxsplit(argc, argv);       //------------should be fixed, takes up a lot of memory
    break;
  case COMM_DUP:
    duptest::duptest(argc, argv);
    break;
  /***   case COMM_DUPIC:
    dupic::dupic(argc, argv);
    break;
  case COMM_IC1:
    ic1::ic1(argc, argv);
    break;
  case COMM_IC2:
    ic2::ic2(argc, argv);
    break;
  case COMM_ICCREATE:
    iccreate::iccreate(argc, argv);
    break;
  case COMM_ICGROUP:
    icgroup::icgroup(argc, argv);
    break;
  case COMM_ICM:
    icm::icm(argc, argv);
    break;
  case COMM_ICSPLIT:
    icsplit::icsplit(argc, argv);
    break;
  case COMM_PROBE_INTERCOMM:
    probe_intercomm::probe_intercomm(argc, argv);
    break;
  case DATATYPE_BLOCKINDEXED_MISC:
    blockindexed_misc::blockindexed_misc(argc, argv);
    break;
  case DATATYPE_BLOCKINDEXED_ZERO_COUNT:
    blockindexed_zero_count::blockindexed_zero_count(argc, argv);
    break;
  case DATATYPE_CONTENTS:
    contents::contents(argc, argv);
    break;
  case DATATYPE_CONTIG_ZERO_COUNT:
    contig_zero_count::contig_zero_count(argc, argv);
    break;
  case DATATYPE_CONTIGSTRUCT:
    contigstruct::contigstruct(argc, argv);
    break;
  case DATATYPE_DARRAY_PACK:
    darray_pack::darray_pack(argc, argv);
    break;
  case DATATYPE_GADDRESS:
    gaddress::gaddress(argc, argv);
    break;
  case DATATYPE_GET_ELEMENTS_PAIRTYPE:
    get_elements_pairtype::get_elements_pairtype(argc, argv);
    break;
  case DATATYPE_GET_ELEMENTS:
    get_elements::get_elements(argc, argv);
    break;
  case DATATYPE_GETPARTELM:
    getpartelm::getpartelm(argc, argv);
    break;
  case DATATYPE_HINDEXED_ZEROS:
    hindexed_zeros::hindexed_zeros(argc, argv);
    break;
  case DATATYPE_HINDEXED_BLOCK:
    hindexed_block::hindexed_block(argc, argv);
    break;
  case DATATYPE_HINDEXED_BLOCK_CONTENTS:
    hindexed_block_contents::hindexed_block_contents(argc, argv);
    break;
  case DATATYPE_INDEXED_MISC:
    indexed_misc::indexed_misc(argc, argv);
    break;
  case DATATYPE_LBUB:
    lbub::lbub(argc, argv);
    break;
  case DATATYPE_LOCALPACK:
    localpack::localpack(argc, argv);
    break;
  case DATATYPE_LONGDOUBLE:
    longdouble::longdouble(argc, argv);
    break;
  case DATATYPE_LOTS_OF_TYPES:
    lots_of_types::lots_of_types(argc, argv);
    break;
  case DATATYPE_PAIRTYPE_PACK:
    pairtype_pack::pairtype_pack(argc, argv);
    break;
  case DATATYPE_PAIRTYPE_SIZE_EXTENT:
    pairtype_size_extent::pairtype_size_extent(argc, argv);
    break;
    // case DATATYPE_SEGTEST: segtest::segtest(argc, argv); break;
  case DATATYPE_SIMPLE_COMMIT:
    simple_commit::simple_commit(argc, argv);
    break;
  case DATATYPE_SIMPLE_PACK_EXTERNAL:
    simple_pack_external::simple_pack_external(argc, argv);
    break;
  case DATATYPE_SIMPLE_PACK:
    simple_pack::simple_pack(argc, argv);
    break;
  case DATATYPE_SIMPLE_RESIZED:
    simple_resized::simple_resized(argc, argv);
    break;
  case DATATYPE_SIMPLE_SIZE_EXTENT:
    simple_size_extent::simple_size_extent(argc, argv);
    break;
  case DATATYPE_SIZEDTYPES:
    sizedtypes::sizedtypes(argc, argv);
    break;
  case DATATYPE_SLICE_PACK_EXTERNAL:
    slice_pack_external::slice_pack_external(argc, argv);
    break;
  case DATATYPE_SLICE_PACK:
    slice_pack::slice_pack(argc, argv);
    break;
  case DATATYPE_STRUCT_DERIVED_ZEROS:
    struct_derived_zeros::struct_derived_zeros(argc, argv);
    break;
  case DATATYPE_STRUCT_EMPTY_EL:
    struct_empty_el::struct_empty_el(argc, argv);
    break;
  case DATATYPE_STRUCT_EZHOV:
    struct_ezhov::struct_ezhov(argc, argv);
    break;
  case DATATYPE_STRUCT_NO_REAL_TYPES:
    struct_no_real_types::struct_no_real_types(argc, argv);
    break;
  case DATATYPE_STRUCT_PACK:
    struct_pack::struct_pack(argc, argv);
    break;
  case DATATYPE_STRUCT_VERYDEEP:
    struct_verydeep::struct_verydeep(argc, argv);
    break;
  case DATATYPE_STRUCT_ZERO_COUNT:
    struct_zero_count::struct_zero_count(argc, argv);
    break;
  case DATATYPE_SUBARRAY_PACK:
    subarray_pack::subarray_pack(argc, argv);
    break;
  case DATATYPE_SUBARRAY:
    subarray::subarray(argc, argv);
    break;
  case DATATYPE_TFREE:
    tfree::tfree(argc, argv);
    break;
  case DATATYPE_TMATCHSIZE:
    tmatchsize::tmatchsize(argc, argv);
    break;
  case DATATYPE_TRANSPOSE_PACK:
    transpose_pack::transpose_pack(argc, argv);
    break;
  case DATATYPE_TRESIZED:
    tresized::tresized(argc, argv);
    break;
  case DATATYPE_TRESIZED2:
    tresized2::tresized2(argc, argv);
    break;
  case DATATYPE_TRIANGULAR_PACK:
    triangular_pack::triangular_pack(argc, argv);
    break;
  case DATATYPE_TYPECOMMIT:
    typecommit::typecommit(argc, argv);
    break;
  case DATATYPE_TYPEFREE:
    typefree::typefree(argc, argv);
    break;
  case DATATYPE_TYPELB:
    typelb::typelb(argc, argv);
    break;
  case DATATYPE_TYPENAME:
    tname::tname(argc, argv);
    break;
  case DATATYPE_UNPACK:
    unpack::unpack(argc, argv);
    break;
  case DATATYPE_UNUSUAL_NONCONTIGS:
    unusual_noncontigs::unusual_noncontigs(argc, argv);
    break;
  case DATATYPE_ZERO_BLKLEN_VECTOR:
    zero_blklen_vector::zero_blklen_vector(argc, argv);
    break;
  case DATATYPE_ZEROBLKS:
    zeroblks::zeroblks(argc, argv);
    break;
  case DATATYPE_ZEROPARMS:
    zeroparms::zeroparms(argc, argv);
    break;
  case ERRHAN_ADDERR:
    adderr::adderr(argc, argv);
    break;
  case ERRHAN_COMMCALL:
    commcall::commcall(argc, argv);
    break;
    //  case ERRHAN_ERRCODE: errcode::errcode(argc, argv); break;
    //  case ERRHAN_ERRFATAL: errfatal::errfatal(argc, argv); break;
    //  case ERRHAN_ERRMSG: errmsg::errmsg(argc, argv); break;
    //  case ERRHAN_ERRRING: errring::errring(argc, argv); break;
    //  case ERRHAN_ERRSTRING: errstring::errstring(argc, argv); break;
    //  case ERRHAN_PREDEF_EH: predef_eh::predef_eh(argc, argv); break;
    //  case GROUP_GLPID: glpid::glpid(argc, argv); break;
  case GROUP_GROUPCREATE:
    groupcreate::groupcreate(argc, argv);
    break;
  case GROUP_GROUPNULLINCL:
    groupnullincl::groupnullincl(argc, argv);
    break;
  case GROUP_GROUPTEST:
    grouptest::grouptest(argc, argv);
    break;
  case GROUP_GROUPTEST2:
    grouptest2::grouptest2(argc, argv);
    break;
  case GROUP_GTRANKS:
    gtranks::gtranks(argc, argv);
    break;
    //case GROUP_GTRANKSPERF:
    //  gtranksperf::gtranksperf(argc, argv);
    //  break;
  case INFO_INFODEL:
    infodel::infodel(argc, argv);
    break;
  case INFO_INFODUP:
    infodup::infodup(argc, argv);
    break;
  case INFO_INFOMANY:
    infomany::infomany(argc, argv);
    break;
  case INFO_INFOMANY2:
    infomany2::infomany2(argc, argv);
    break;
  case INFO_INFOORDER:
    infoorder::infoorder(argc, argv);
    break;
  case INFO_INFOTEST:
    infotest::infotest(argc, argv);
    break;
  case INFO_INFOVALLEN:
    infovallen::infovallen(argc, argv);
    break;
  case INIT_ATTRSELF:
    attrself::attrself(argc, argv);
    break;
  case INIT_EXITST1:
    exitst1::exitst1(argc, argv);
    break;
  case INIT_EXITST2:
    exitst2::exitst2(argc, argv);
    break;
  case INIT_EXITST3:
    exitst3::exitst3(argc, argv);
    break;
  case INIT_FINALIZED:
    finalized::finalized(argc, argv);
    break;
  case INIT_INITSTAT:
    initstat::initstat(argc, argv);
    break;
    //  case INIT_TIMEOUT:    timeout::timeout(argc, argv); break;
  case INIT_VERSION:
    version::version(argc, argv);
    break; */
  case PT2PT_ANYALL:
    anyall::anyall(argc, argv);
    break;
  case PT2PT_BOTTOM:
    bottom::bottom(argc, argv);
    break;
  case PT2PT_BSEND1:
    bsend1::bsend1(argc, argv);
    break;
  case PT2PT_BSEND2:
    bsend2::bsend2(argc, argv);
    break;
  //case PT2PT_BSEND3:
  //  bsend3::bsend3(argc, argv);
  //  break;
  case PT2PT_BSEND4:
    bsend4::bsend4(argc, argv);
    break;
  //case PT2PT_BSEND5:
  //  bsend5::bsend5(argc, argv);
  //  break;
  case PT2PT_BSENDALIGN:
    bsendalign::bsendalign(argc, argv);
    break;
  case PT2PT_BSENDFRAG:
    bsendfrag::bsendfrag(argc, argv);
    break;
  case PT2PT_BSENDPENDING:
    bsendpending::bsendpending(argc, argv);
    break;
  /*** case PT2PT_CANCELRECV:
    cancelrecv::cancelrecv(argc, argv);
    break;
  case PT2PT_EAGERDT:
    eagerdt::eagerdt(argc, argv);
    break;
  case PT2PT_GREQ1:
    greq1::greq1(argc, argv);
    break;
  case PT2PT_ICSEND:
    icsend::icsend(argc, argv);
    break;
  case PT2PT_INACTIVEREQ:
    inactivereq::inactivereq(argc, argv);
    break; */
  case PT2PT_ISENDSELF:
    isendself::isendself(argc, argv);
    break;
    // case PT2PT_ISENDSELFPROBE:isendselfprobe::isendselfprobe(argc, argv);break;
  case PT2PT_LARGE_MESSAGE:
    large_message::large_message(argc, argv);
    break;
  case PT2PT_MPROBE:
    mprobe::mprobe(argc, argv);
    break;
    //  case PT2PT_PINGPING:   // ------ careful, this takes a while to run ----- //
    //   pingping::pingping(argc, argv);
    //   break;
  case PT2PT_PROBE_UNEXP:
    probe_unexp::probe_unexp(argc, argv);
    break;
  case PT2PT_PROBENULL:
    probenull::probenull(argc, argv);
    break;
  /*** case PT2PT_PSCANCEL:
    pscancel::pscancel(argc, argv);
    break;
  case PT2PT_RCANCEL:
    rcancel::rcancel(argc, argv);
    break;
  case PT2PT_RQFREEB:
    rqfreeb::rqfreeb(argc, argv);
    break;
  case PT2PT_RQSTATUS:
    rqstatus::rqstatus(argc, argv);
    break;
  case PT2PT_SCANCEL:
    scancel::scancel(argc, argv);
    break;
  case PT2PT_SCANCEL2:
    scancel2::scancel2(argc, argv);
    break; */
  case PT2PT_SENDALL:
    sendall::sendall(argc, argv);
    break;
  case PT2PT_SENDFLOOD:
    sendflood::sendflood(argc, argv);
    break;
  case PT2PT_SENDRECV1:
    sendrecv1::sendrecv1(argc, argv);
    break;
  case PT2PT_SENDRECV2:
    sendrecv2::sendrecv2(argc, argv);
    break;
  case PT2PT_SENDRECV3:
    sendrecv3::sendrecv3(argc, argv);
    break;
  case PT2PT_SENDSELF:
    sendself::sendself(argc, argv);
    break;
  case PT2PT_WAITANY_NULL:
    waitany_null::waitany_null(argc, argv);
    break;
  case PT2PT_WAITTESTNULL:
    waittestnull::waittestnull(argc, argv);
    break;
  /*** case TOPO_CARTCREATES:
    cartcreates::cartcreates(argc, argv);
    break;
  case TOPO_CARTMAP1:
    cartmap1::cartmap1(argc, argv);
    break;
  case TOPO_CARTSHIFT1:
    cartshift1::cartshift1(argc, argv);
    break;
  case TOPO_CARTSUBALL:
    cartsuball::cartsuball(argc, argv);
    break;
  case TOPO_CARTZERO:
    cartzero::cartzero(argc, argv);
    break;
  case TOPO_DGRAPH_UNWGT:
    dgraph_unwgt::dgraph_unwgt(argc, argv);
    break;
  case TOPO_DIMS1:
    dims1::dims1(argc, argv);
    break;
  case TOPO_DIMS2:
    dims2::dims2(argc, argv);
    break;
  case TOPO_DISTGRAPH1:
    distgraph1::distgraph1(argc, argv);
    break;
  case TOPO_GRAPHCR:
    graphcr::graphcr(argc, argv);
    break;
  case TOPO_GRAPHCR2:
    graphcr2::graphcr2(argc, argv);
    break;
  case TOPO_GRAPHMAP1:
    graphmap1::graphmap1(argc, argv);
    break;
  case TOPO_NEIGHB_COLL:
    neighb_coll::neighb_coll(argc, argv);
    break;
  case TOPO_TOPODUP:
    topodup::topodup(argc, argv);
    break;
  case TOPO_TOPOTEST:
    topotest::topotest(argc, argv);
    break;

  case RMA_ACCFENCE1:
    accfence1::accfence1(argc, argv);
    break;
  case RMA_ACCFENCE2_AM:
    accfence2_am::accfence2_am(argc, argv);
    break;
  case RMA_ACCFENCE2:
    accfence2::accfence2(argc, argv);
    break;
  case RMA_ALLOCMEM:
    allocmem::allocmem(argc, argv);
    break;
  case RMA_ATTRORDERWIN:
    attrorderwin::attorderwin(argc, argv);
    break;
  case RMA_BASEATTRWIN:
    baseattrwin::baseattrwin(argc, argv);
    break;
  case RMA_CONTIG_DISPL:
    contig_displ::contig_displ(argc, argv);
    break;
  case RMA_FETCHANDADD_AM:
    fetchandadd_am::fetchandadd_am(argc, argv);
    break;
  case RMA_FETCHANDADD_TREE_AM:
    fetchandadd_tree_am::fetchandadd_tree_am(argc, argv);
    break;
  case RMA_FETCHANDADD_TREE:
    fetchandadd_tree::fetchandadd_tree(argc, argv);
    break;
  case RMA_FETCHANDADD:
    fetchandadd::fetchandadd(argc, argv);
    break;
  case RMA_FKEYVALWIN:
    fkeyvalwin::fkeyvalwin(argc, argv);
    break;
  case RMA_GETFENCE1:
    getfence1::getfence1(argc, argv);
    break;
  case RMA_GETGROUP:
    getgroup::getgroup(argc, argv);
    break;
  case RMA_IRCPI:
    ircpi::ircpi(argc, argv);
    break;
  case RMA_LOCKCONTENTION:
    lockcontention::lockcontention(argc, argv);
    break;
  case RMA_LOCKNULL:
    locknull::locknull(argc, argv);
    break;
  case RMA_MIXEDSYNC:
    mixedsync::mixedsync(argc, argv);
    break;
  case RMA_NULLPSCW:
    nullpscw::nullpscw(argc, argv);
    break;
  case RMA_PUTFENCE1:
    putfence1::putfence1(argc, argv);
    break;
  case RMA_PUTFIDX:
    putfidx::putfidx(argc, argv);
    break;
  case RMA_PUTPSCW1:
    putpscw1::putpscw1(argc, argv);
    break;
  case RMA_SELFRMA:
    selfrma::selfrma(argc, argv);
    break;
  case RMA_TEST1_AM:
    test1_am::test1_am(argc, argv);
    break;
  case RMA_TEST1:
    test1::test1(argc, argv);
    break;
  case RMA_TEST2_AM:
    test2_am::test2_am(argc, argv);
    break;
  case RMA_TEST2:
    test2::test2(argc, argv);
    break;
  case RMA_TEST3_AM:
    test3_am::test3_am(argc, argv);
    break;
  case RMA_TEST3:
    test3::test3(argc, argv);
    break;
  case RMA_TEST4_AM:
    test4_am::test4_am(argc, argv);
    break;
  case RMA_TEST4:
    test4::test4(argc, argv);
    break;
  case RMA_TEST5_AM:
    test5_am::test5_am(argc, argv);
    break;
  case RMA_TEST5:
    test5::test5(argc, argv);
    break;
  case RMA_TRANSPOSE1:
    transpose1::transpose1(argc, argv);
    break;
  case RMA_TRANSPOSE2:
    transpose2::transpose2(argc, argv);
    break;
  case RMA_TRANSPOSE3:
    transpose3::transpose3(argc, argv);
    break;
  case RMA_TRANSPOSE4:
    transpose4::transpose4(argc, argv);
    break;
  case RMA_TRANSPOSE5:
    transpose5::transpose5(argc, argv);
    break;
  case RMA_TRANSPOSE6:
    transpose6::transpose6(argc, argv);
    break;
  case RMA_TRANSPOSE7:
    transpose7::transpose7(argc, argv);
    break;
  case RMA_WINCALL:
    wincall::wincall(argc, argv);
    break;
  case RMA_WINNAME:
    winname::winname(argc, argv);
    break;
  case RMA_WINTEST:
    wintest::wintest(argc, argv);
    break; */

  default:
    spkt_throw_printf(sprockit::spkt_error, "testmpi: unknown test mode %d", testmode_);
    return 1;
  }

  double t_stop = get_time();
  double t_total = t_stop - t_start;
  if (sumi::sstmac_mpi()->rank() == 0)
    printf("MPI test ran for %8.4fms\n", t_total*1e3);

  sstmac::runtime::exit_deadlock_region();

  return 0;
}