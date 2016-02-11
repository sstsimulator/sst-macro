#include <sstmac/replacements/mpi.h>
#include <sstmac/replacements/sys/time.h>
#include <sstmac/util.h>
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

  ATTR_ATTR2TYPE = 0,
  ATTR_ATTREND = 1,
  ATTR_ATTREND2 = 2,
  ATTR_ATTRERR = 3,
  ATTR_ATTRERRCOMM = 4,
  ATTR_ATTRERRTYPE = 5,
  ATTR_ATTRIC = 6,
  ATTR_ATTRORDER = 7,
  ATTR_ATTRORDERCOMM = 8,
  ATTR_ATTRORDERTYPE = 9,
  ATTR_ATTRT = 10,
  ATTR_BASEATTR2 = 11,
  ATTR_BASEATTRCOMM = 12,
  ATTR_FKEYVAL = 13,
  ATTR_FKEYVALCOMM = 14,
  ATTR_FKEYVALTYPE = 15,
  ATTR_KEYVAL_DOUBLE_FREE = 16,
  COLL_ALLGATHER2 = 17,
  COLL_ALLGATHER3 = 18,
  COLL_ALLGATHERV2 = 19,
  COLL_ALLGATHERV3 = 20,
  COLL_ALLGATHERV4 = 21,
  COLL_ALLRED = 22,
  COLL_ALLRED2 = 23,
  COLL_ALLRED3 = 24,
  COLL_ALLRED4 = 25,
  COLL_ALLRED5 = 26,
  COLL_ALLRED6 = 27,
  COLL_ALLREDMANY = 28,
  COLL_ALLTOALL1 = 29,
  COLL_ALLTOALLV = 30,
  COLL_ALLTOALLV0 = 31,
  COLL_ALLTOALLW1 = 32,
  COLL_ALLTOALLW2 = 33,
  COLL_ALLTOALLW_ZEROS = 34,
  COLL_BCAST2 = 35, //this guy takes a while, eats up a lot of memory too
  COLL_BCAST3 = 36,
  COLL_BCASTTEST = 37,
  COLL_BCASTZEROTYPE = 38,
  COLL_COLL10 = 39,
  COLL_COLL11 = 40,
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
  COLL_EXSCAN = 51,
  COLL_EXSCAN2 = 52,
  COLL_GATHER = 53,
  COLL_GATHER2 = 54,
  COLL_IALLRED = 55,
  COLL_ICALLGATHER = 56,
  COLL_ICALLGATHERV = 57,
  COLL_ICALLREDUCE = 58,
  COLL_ICALLTOALL = 59,
  COLL_ICALLTOALLV = 60,
  COLL_ICALLTOALLW = 61,
  COLL_ICBARRIER = 62,
  COLL_ICBCAST = 63,
  COLL_ICGATHER = 64,
  COLL_ICGATHERV = 65,
  COLL_ICREDUCE = 66,
  COLL_ICSCATTER = 67,
  COLL_ICSCATTERV = 68,
  COLL_LONGUSER = 69,
  COLL_NONBLOCKING = 70,
  COLL_NONBLOCKING2 = 71,
  COLL_NONBLOCKING3 = 72,
  COLL_OP_COMMUTATIVE = 73,
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
  COLL_RED3 = 86,
  COLL_RED4 = 87,
  COLL_RED_SCAT_BLOCK = 88,
  COLL_RED_SCAT_BLOCK2 = 89,
  COLL_REDSCAT = 90,
  COLL_REDSCAT2 = 91,
  COLL_REDSCAT3 = 92,
  COLL_REDSCATBKINTER = 93,
  COLL_REDSCATBLK3 = 94,
  COLL_REDSCATINTER = 95,
  COLL_REDUCE = 96,
  COLL_REDUCE_LOCAL = 97,
  COLL_SCANTST = 98,
  COLL_SCATTER2 = 99,
  COLL_SCATTER3 = 100,
  COLL_SCATTERN = 101,
  COLL_SCATTERV = 102,
  COMM_CMFREE = 103,
  COMM_CMSPLIT = 104,
  COMM_CMSPLIT2 = 105,
  COMM_CMSPLIT_TYPE = 106,
  COMM_COMM_CREATE_GROUP = 107,
  COMM_COMM_GROUP_HALF = 108,
  COMM_COMM_GROUP_RAND = 109,
  COMM_COMM_IDUP = 110,
  COMM_COMMCREATE1 = 111,
  COMM_COMMNAME = 112,
  COMM_CTXALLOC = 113,
  COMM_CTXSPLIT = 114,
  COMM_DUP = 115,
  COMM_DUPIC = 116,
  COMM_IC1 = 117,
  COMM_IC2 = 118,
  COMM_ICCREATE = 119,
  COMM_ICGROUP = 120,
  COMM_ICM = 121,
  COMM_ICSPLIT = 122,
  COMM_PROBE_INTERCOMM = 123,
  DATATYPE_BLOCKINDEXED_MISC = 124,
  DATATYPE_BLOCKINDEXED_ZERO_COUNT = 125,
  DATATYPE_CONTENTS = 126,
  DATATYPE_CONTIG_ZERO_COUNT = 127,
  DATATYPE_CONTIGSTRUCT = 128,
  DATATYPE_DARRAY_PACK = 129,
  DATATYPE_GADDRESS = 130,
  DATATYPE_GET_ELEMENTS_PAIRTYPE = 131,
  DATATYPE_GET_ELEMENTS = 132,
  DATATYPE_GETPARTELM = 133,
  DATATYPE_HINDEXED_ZEROS = 134,
  DATATYPE_HINDEXED_BLOCK = 135,
  DATATYPE_HINDEXED_BLOCK_CONTENTS = 136,
  DATATYPE_INDEXED_MISC = 137,
  DATATYPE_LBUB = 138,
  DATATYPE_LOCALPACK = 139,
  DATATYPE_LONGDOUBLE = 140,
  DATATYPE_LOTS_OF_TYPES = 141,
  DATATYPE_PAIRTYPE_PACK = 142,
  DATATYPE_PAIRTYPE_SIZE_EXTENT = 143,
  //  DATATYPE_SEGTEST = 144,
  DATATYPE_SIMPLE_COMMIT = 145,
  DATATYPE_SIMPLE_PACK_EXTERNAL = 146,
  DATATYPE_SIMPLE_PACK = 147,
  DATATYPE_SIMPLE_RESIZED = 148,
  DATATYPE_SIMPLE_SIZE_EXTENT = 149,
  DATATYPE_SIZEDTYPES = 150,
  DATATYPE_SLICE_PACK_EXTERNAL = 151,
  DATATYPE_SLICE_PACK = 152,
  DATATYPE_STRUCT_DERIVED_ZEROS = 153,
  DATATYPE_STRUCT_EMPTY_EL = 154,
  DATATYPE_STRUCT_EZHOV = 155,
  DATATYPE_STRUCT_NO_REAL_TYPES = 156,
  DATATYPE_STRUCT_PACK = 157,
  DATATYPE_STRUCT_VERYDEEP = 158,
  DATATYPE_STRUCT_ZERO_COUNT = 159,
  DATATYPE_SUBARRAY_PACK = 160,
  DATATYPE_SUBARRAY = 161,
  DATATYPE_TFREE = 162,
  DATATYPE_TMATCHSIZE = 163,
  DATATYPE_TRANSPOSE_PACK = 164,
  DATATYPE_TRESIZED = 165,
  DATATYPE_TRESIZED2 = 166,
  DATATYPE_TRIANGULAR_PACK = 167,
  DATATYPE_TYPECOMMIT = 168,
  DATATYPE_TYPEFREE = 169,
  DATATYPE_TYPELB = 170,
  DATATYPE_TYPENAME = 171,
  DATATYPE_UNPACK = 172,
  DATATYPE_UNUSUAL_NONCONTIGS = 173,
  DATATYPE_ZERO_BLKLEN_VECTOR = 174,
  DATATYPE_ZEROBLKS = 175,
  DATATYPE_ZEROPARMS = 176,
  ERRHAN_ADDERR = 177,
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
  INIT_VERSION = 206,
  PT2PT_ANYALL = 207,
  PT2PT_BOTTOM = 208,
  PT2PT_BSEND1 = 209,
  PT2PT_BSEND2 = 210,
  PT2PT_BSEND3 = 211,
  PT2PT_BSEND4 = 212,
  PT2PT_BSEND5 = 213,
  PT2PT_BSENDALIGN = 214,
  PT2PT_BSENDFRAG = 215,
  PT2PT_BSENDPENDING = 216,
  PT2PT_CANCELRECV = 217,
  PT2PT_EAGERDT = 218,
  PT2PT_GREQ1 = 219,
  PT2PT_ICSEND = 220,
  PT2PT_INACTIVEREQ = 221,
  PT2PT_ISENDSELF = 222,
  //  PT2PT_ISENDSELFPROBE = 223,
  PT2PT_LARGE_MESSAGE = 224,
  PT2PT_MPROBE = 225,
  PT2PT_PINGPING = 226,
  PT2PT_PROBE_UNEXP = 227,
  PT2PT_PROBENULL = 228,
  PT2PT_PSCANCEL = 229,
  PT2PT_RCANCEL = 230,
  PT2PT_RQFREEB = 231,
  PT2PT_RQSTATUS = 232,
  PT2PT_SCANCEL = 233,
  PT2PT_SCANCEL2 = 234,
  PT2PT_SENDALL = 235,
  PT2PT_SENDFLOOD = 236,
  PT2PT_SENDRECV1 = 237,
  PT2PT_SENDRECV2 = 238,
  PT2PT_SENDRECV3 = 239,
  PT2PT_SENDSELF = 240,
  PT2PT_WAITANY_NULL = 241,
  PT2PT_WAITTESTNULL = 242,
  TOPO_CARTCREATES = 243,
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
  RMA_WINTEST = 300,
  TEST_MODE_END = 301

};

//-------- attr ---------//
#include "attr/attr2type.cc"
#include "attr/attrend.cc"
#include "attr/attrend2.cc"
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

//----------- coll ----------------//
#include "coll/allgather2.cc"
#include "coll/allgather3.cc"
#include "coll/allgatherv2.cc"
#include "coll/allgatherv3.cc"
#include "coll/allgatherv4.cc"
#include "coll/allred.cc"
#include "coll/allred2.cc"
#include "coll/allred3.cc"
#include "coll/allred4.cc"
#include "coll/allred5.cc"
#include "coll/allred6.cc"
#include "coll/allredmany.cc"
#include "coll/alltoall1.cc"
#include "coll/alltoallv.cc"
#include "coll/alltoallv0.cc"
#include "coll/alltoallw1.cc"
#include "coll/alltoallw2.cc"
#include "coll/alltoallw_zeros.cc"
#include "coll/bcast2.cc"
#include "coll/bcast3.cc"
#include "coll/bcasttest.cc"
#include "coll/bcastzerotype.cc"
#include "coll/coll10.cc"
#include "coll/coll11.cc"
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
#include "coll/exscan.cc"
#include "coll/exscan2.cc"
#include "coll/gather.cc"
#include "coll/gather2.cc"
#include "coll/iallred.cc"
#include "coll/icallgather.cc"
#include "coll/icallgatherv.cc"
#include "coll/icallreduce.cc"
#include "coll/icalltoall.cc"
#include "coll/icalltoallv.cc"
#include "coll/icalltoallw.cc"
#include "coll/icbarrier.cc"
#include "coll/icbcast.cc"
#include "coll/icgather.cc"
#include "coll/icgatherv.cc"
#include "coll/icreduce.cc"
#include "coll/icscatter.cc"
#include "coll/icscatterv.cc"
#include "coll/longuser.cc"
#include "coll/nonblocking.cc"
#include "coll/nonblocking2.cc"
#include "coll/nonblocking3.cc"
#include "coll/op_commutative.cc"
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
#include "coll/red3.cc"
#include "coll/red4.cc"
#include "coll/red_scat_block.cc"
#include "coll/red_scat_block2.cc"
#include "coll/redscat.cc"
#include "coll/redscat2.cc"
#include "coll/redscat3.cc"
#include "coll/redscatbkinter.cc"
#include "coll/redscatblk3.cc"
#include "coll/redscatinter.cc"
#include "coll/reduce.cc"
#include "coll/reduce_local.cc"
#include "coll/scantst.cc"
#include "coll/scatter2.cc"
#include "coll/scatter3.cc"
#include "coll/scattern.cc"
#include "coll/scatterv.cc"

//-------- comm -------------------//
#include "comm/cmfree.cc"
#include "comm/cmsplit.cc"
#include "comm/cmsplit2.cc"
#include "comm/cmsplit_type.cc"
#include "comm/comm_create_group.cc"
#include "comm/comm_group_half.cc"
#include "comm/comm_group_rand.cc"
#include "comm/comm_idup.cc"
#include "comm/commcreate1.cc"
#include "comm/commname.cc"
#include "comm/ctxalloc.cc"
#include "comm/ctxsplit.cc"
#include "comm/dup.cc"
#include "comm/dupic.cc"
#include "comm/ic1.cc"
#include "comm/ic2.cc"
#include "comm/iccreate.cc"
#include "comm/icgroup.cc"
#include "comm/icm.cc"
#include "comm/icsplit.cc"
#include "comm/probe-intercomm.cc"

// ---------------- datatype ---------------- //
#include "datatype/blockindexed-misc.cc"
#include "datatype/blockindexed-zero-count.cc"
#include "datatype/contents.cc"
#include "datatype/contig-zero-count.cc"
#include "datatype/contigstruct.cc"
#include "datatype/darray-pack.cc"
#include "datatype/gaddress.cc"
#include "datatype/get-elements-pairtype.cc"
#include "datatype/get-elements.cc"
#include "datatype/getpartelm.cc"
#include "datatype/hindexed-zeros.cc"
#include "datatype/hindexed_block.cc"
#include "datatype/hindexed_block_contents.cc"
#include "datatype/indexed-misc.cc"
#include "datatype/lbub.cc"
#include "datatype/localpack.cc"
#include "datatype/longdouble.cc"
#include "datatype/lots-of-types.cc"
#include "datatype/pairtype-pack.cc"
#include "datatype/pairtype-size-extent.cc"
//#include "datatype/segtest.cc"
#include "datatype/simple-commit.cc"
#include "datatype/simple-pack-external.cc"
#include "datatype/simple-pack.cc"
#include "datatype/simple-resized.cc"
#include "datatype/simple-size-extent.cc"
#include "datatype/sizedtypes.cc"
#include "datatype/slice-pack-external.cc"
#include "datatype/slice-pack.cc"
#include "datatype/struct-derived-zeros.cc"
#include "datatype/struct-empty-el.cc"
#include "datatype/struct-ezhov.cc"
#include "datatype/struct-no-real-types.cc"
#include "datatype/struct-pack.cc"
#include "datatype/struct-verydeep.cc"
#include "datatype/struct-zero-count.cc"
#include "datatype/subarray-pack.cc"
#include "datatype/subarray.cc"
#include "datatype/tfree.cc"
#include "datatype/tmatchsize.cc"
#include "datatype/transpose-pack.cc"
#include "datatype/tresized.cc"
#include "datatype/tresized2.cc"
#include "datatype/triangular-pack.cc"
#include "datatype/typecommit.cc"
#include "datatype/typefree.cc"
#include "datatype/typelb.cc"
#include "datatype/typename.cc"
#include "datatype/unpack.cc"
#include "datatype/unusual-noncontigs.cc"
#include "datatype/zero-blklen-vector.cc"
#include "datatype/zeroblks.cc"
#include "datatype/zeroparms.cc"

// ------------------ errhan ------------- //
#include "errhan/adderr.cc"
#include "errhan/commcall.cc"
//#include "errhan/errcode.cc"
//#include "errhan/errfatal.cc"
//#include "errhan/errmsg.cc"
//#include "errhan/errring.cc"
//#include "errhan/errstring.cc"
//#include "errhan/predef_eh.cc"

// --------------- group --------------- //
//#include "group/glpid.cc"
#include "group/groupcreate.cc"
#include "group/groupnullincl.cc"
#include "group/grouptest.cc"
#include "group/grouptest2.cc"
#include "group/gtranks.cc"
#include "group/gtranksperf.cc"

// --------------- info -------------- //
#include "info/infodel.cc"
#include "info/infodup.cc"
#include "info/infomany.cc"
#include "info/infomany2.cc"
#include "info/infoorder.cc"
#include "info/infotest.cc"
#include "info/infovallen.cc"

// --------------- init ----------- //
#include "init/attrself.cc"
#include "init/exitst1.cc"
#include "init/exitst2.cc"
#include "init/exitst3.cc"
#include "init/finalized.cc"
#include "init/initstat.cc"
#include "init/timeout.cc"
#include "init/version.cc"

// -------------- pt2pt ----------- //
#include "pt2pt/anyall.cc"
#include "pt2pt/bottom.cc"
#include "pt2pt/bsend1.cc"
#include "pt2pt/bsend2.cc"
#include "pt2pt/bsend3.cc"
#include "pt2pt/bsend4.cc"
#include "pt2pt/bsend5.cc"
#include "pt2pt/bsendalign.cc"
#include "pt2pt/bsendfrag.cc"
#include "pt2pt/bsendpending.cc"
#include "pt2pt/cancelrecv.cc"
#include "pt2pt/eagerdt.cc"
#include "pt2pt/greq1.cc"
#include "pt2pt/icsend.cc"
#include "pt2pt/inactivereq.cc"
#include "pt2pt/isendself.cc"
#include "pt2pt/isendselfprobe.cc"
#include "pt2pt/large_message.cc"
#include "pt2pt/mprobe.cc"
#include "pt2pt/pingping.cc"
#include "pt2pt/probe-unexp.cc"
#include "pt2pt/probenull.cc"
#include "pt2pt/pscancel.cc"
#include "pt2pt/rcancel.cc"
#include "pt2pt/rqfreeb.cc"
#include "pt2pt/rqstatus.cc"
#include "pt2pt/scancel.cc"
#include "pt2pt/scancel2.cc"
#include "pt2pt/sendall.cc"
#include "pt2pt/sendflood.cc"
#include "pt2pt/sendrecv1.cc"
#include "pt2pt/sendrecv2.cc"
#include "pt2pt/sendrecv3.cc"
#include "pt2pt/sendself.cc"
#include "pt2pt/waitany-null.cc"
#include "pt2pt/waittestnull.cc"

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

// -------------- RMA ------------ //
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

int testmode_ = -1;
bool justprint = false;
bool printnumtests = false;

std::string
print_test_name(int testmode);

const char* valid_keywords[] = {
"testsuite_testmode",
"testsuite_print",
"testsuite_numtests" };
sprockit::StaticKeywordRegister reg(3, valid_keywords);

int USER_MAIN(int argc, char *argv[])
{
  sprockit::sim_parameters* params = get_params();
  testmode_ = params->get_int_param("testsuite_testmode");
  justprint = params->get_optional_bool_param("testsuite_print", false);
  printnumtests = params->get_optional_bool_param("testsuite_numtests", false);
  
  int thr_id = sstmac::sw::operating_system::current_thread()->thread_id();
  double t_start, t_stop;
  if (thr_id == 0){
    t_start = get_time();
  }

  if (justprint)
  {
    std::cout << "name: " << print_test_name(testmode_) << "\n";
    return 0;
  }
  else if (printnumtests)
  {
    std::cout << (TEST_MODE_END - 1) << "\n";
    return 0;
  }

  switch (testmode_)
  {
  case ATTR_ATTR2TYPE:
    attr2type::attr2type(argc, argv);
    break;
  case ATTR_ATTREND:
    attrend::attrend(argc, argv);
    break;
  case ATTR_ATTREND2:
    attrend2::attrend2(argc, argv);
    break;
  case ATTR_ATTRERR:
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
    break;
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
  case COLL_ALLRED3:
    allred3::allred3(argc, argv);
    break;
  case COLL_ALLRED4:
    allred4::allred4(argc, argv);
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
  case COLL_ALLTOALLW1:
    alltoallw1::alltoallw1(argc, argv);
    break;
  case COLL_ALLTOALLW2:
    alltoallw2::alltoallw2(argc, argv);
    break;
  case COLL_ALLTOALLW_ZEROS:
    alltoallw_zeros::alltoallw_zeros(argc, argv);
    break;
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
  case COLL_COLL10:
    coll10::coll10(argc, argv);
    break;
  case COLL_COLL11:
    coll11::coll11(argc, argv);
    break;
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
  case COLL_EXSCAN:
    exscan::exscan(argc, argv);
    break;
  case COLL_EXSCAN2:
    exscan2::exscan2(argc, argv);
    break;
  case COLL_GATHER:
    gather::gather(argc, argv);
    break;
  case COLL_GATHER2:
    gather2::gather2(argc, argv);
    break;
  case COLL_IALLRED:
    iallred::iallred(argc, argv);
    break;
  case COLL_ICALLGATHER:
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
    break;
  case COLL_LONGUSER:
    longuser::longuser(argc, argv);
    break;
  case COLL_NONBLOCKING:
    nonblocking::nonblocking(argc, argv);
    break;
  case COLL_NONBLOCKING2:
    nonblocking2::nonblocking2(argc, argv);
    break;
  case COLL_NONBLOCKING3:
    nonblocking3::nonblocking3(argc, argv);
    break;
  case COLL_OP_COMMUTATIVE:
    op_commutative::op_commutative(argc, argv);
    break;
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
  case COLL_RED3:
    red3::red3(argc, argv);
    break;
  case COLL_RED4:
    red4::red4(argc, argv);
    break;
  case COLL_RED_SCAT_BLOCK:
    red_scat_block::red_scat_block(argc, argv);
    break;
  case COLL_RED_SCAT_BLOCK2:
    red_scat_block2::red_scat_block2(argc, argv);
    break;
  case COLL_REDSCAT:
    redscat::redscat(argc, argv);
    break;
  case COLL_REDSCAT2:
    redscat2::redscat2(argc, argv);
    break;
  case COLL_REDSCAT3:
    redscat3::redscat3(argc, argv);
    break;
  case COLL_REDSCATBKINTER:
    redscatbkinter::redscatbkinter(argc, argv);
    break;
  case COLL_REDSCATBLK3:
    redscatblk3::redscatblk3(argc, argv);
    break;
  case COLL_REDSCATINTER:
    redscatinter::redscatinter(argc, argv);
    break;
  case COLL_REDUCE:
    reduce::reduce(argc, argv);
    break;
  case COLL_REDUCE_LOCAL:
    reduce_local::reduce_local(argc, argv);
    break;
  case COLL_SCANTST:
    scantst::scantst(argc, argv);
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
  case COLL_SCATTERV:
    scatterv::scatterv(argc, argv);
    break;
  case COMM_CMFREE:
    cmfree::cmfree(argc, argv);
    break;
  case COMM_CMSPLIT:
    cmsplit::cmsplit(argc, argv);
    break;
  case COMM_CMSPLIT2:
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
    break;
  case COMM_COMM_IDUP:
    comm_idup::comm_idup(argc, argv);
    break;
  case COMM_COMMCREATE1:
    commcreate1::commcreate1(argc, argv);
    break;
  case COMM_COMMNAME:
    commname::commname(argc, argv);
    break;
  case COMM_CTXALLOC:
    // ctxalloc::ctxalloc(argc, argv);       //------------should be fixed, takes up a lot of memory
    break;
  case COMM_CTXSPLIT:
    //ctxsplit::ctxsplit(argc, argv);       //------------should be fixed, takes up a lot of memory
    break;
  case COMM_DUP:
    duptest::duptest(argc, argv);
    break;
  case COMM_DUPIC:
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
    break;
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
  case PT2PT_BSEND3:
    bsend3::bsend3(argc, argv);
    break;
  case PT2PT_BSEND4:
    bsend4::bsend4(argc, argv);
    break;
  case PT2PT_BSEND5:
    bsend5::bsend5(argc, argv);
    break;
  case PT2PT_BSENDALIGN:
    bsendalign::bsendalign(argc, argv);
    break;
  case PT2PT_BSENDFRAG:
    bsendfrag::bsendfrag(argc, argv);
    break;
  case PT2PT_BSENDPENDING:
    bsendpending::bsendpending(argc, argv);
    break;
  case PT2PT_CANCELRECV:
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
    break;
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
  case PT2PT_PSCANCEL:
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
    break;
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
  case TOPO_CARTCREATES:
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
    break;

  default:
    spkt_throw_printf(sprockit::spkt_error, "testmpi: unknown test mode %d", testmode_);
    return 1;
  }
  if (thr_id == 0){
    t_stop = get_time();
    double t_total = t_stop - t_start;
    printf("MPI test ran for %8.4fms\n", t_total*1e3);
  }

  return 0;
}

std::string
print_test_name(int testmode)
{
  switch (testmode)
  {
  case ATTR_ATTR2TYPE:
    return "ATTR_ATTR2TYPE";
  case ATTR_ATTREND:
    return "ATTR_ATTREND";
  case ATTR_ATTREND2:
    return "ATTR_ATTREND2";
  case ATTR_ATTRERR:
    return "ATTR_ATTRERR";
  case ATTR_ATTRERRCOMM:
    return "ATTR_ATTRERRCOMM";
  case ATTR_ATTRERRTYPE:
    return "ATTR_ATTRERRTYPE";
  case ATTR_ATTRIC:
    return "ATTR_ATTRIC";
  case ATTR_ATTRORDER:
    return "ATTR_ATTRORDER";
  case ATTR_ATTRORDERCOMM:
    return "ATTR_ATTRORDERCOMM";
  case ATTR_ATTRORDERTYPE:
    return "ATTR_ATTRORDERTYPE";
  case ATTR_ATTRT:
    return "ATTR_ATTRT";
  case ATTR_BASEATTR2:
    return "ATTR_BASEATTR2";
  case ATTR_BASEATTRCOMM:
    return "ATTR_BASEATTRCOMM";
  case ATTR_FKEYVAL:
    return "ATTR_FKEYVAL";
  case ATTR_FKEYVALCOMM:
    return "ATTR_FKEYVALCOMM";
  case ATTR_FKEYVALTYPE:
    return "ATTR_FKEYVALTYPE";
  case ATTR_KEYVAL_DOUBLE_FREE:
    return "ATTR_KEYVAL_DOUBLE_FREE";
  case COLL_ALLGATHER2:
    return "COLL_ALLGATHER2";
  case COLL_ALLGATHER3:
    return "COLL_ALLGATHER3";
  case COLL_ALLGATHERV2:
    return "COLL_ALLGATHERV2";
  case COLL_ALLGATHERV3:
    return "COLL_ALLGATHERV3";
  case COLL_ALLGATHERV4:
    return "COLL_ALLGATHERV4";
  case COLL_ALLRED:
    return "COLL_ALLRED";
  case COLL_ALLRED2:
    return "COLL_ALLRED2";
  case COLL_ALLRED3:
    return "COLL_ALLRED3";
  case COLL_ALLRED4:
    return "COLL_ALLRED4";
  case COLL_ALLRED5:
    return "COLL_ALLRED5";
  case COLL_ALLRED6:
    return "COLL_ALLRED6";
  case COLL_ALLREDMANY:
    return "COLL_ALLREDMANY";
  case COLL_ALLTOALL1:
    return "COLL_ALLTOALL1";
  case COLL_ALLTOALLV:
    return "COLL_ALLTOALLV";
  case COLL_ALLTOALLV0:
    return "COLL_ALLTOALLV0";
  case COLL_ALLTOALLW1:
    return "COLL_ALLTOALLW1";
  case COLL_ALLTOALLW2:
    return "COLL_ALLTOALLW2";
  case COLL_ALLTOALLW_ZEROS:
    return "COLL_ALLTOALLW_ZEROS";
  case COLL_BCAST2:
    return "COLL_BCAST2";
  case COLL_BCAST3:
    return "COLL_BCAST3";
  case COLL_BCASTTEST:
    return "COLL_BCASTTEST";
  case COLL_BCASTZEROTYPE:
    return "COLL_BCASTZEROTYPE";
  case COLL_COLL10:
    return "COLL_COLL10";
  case COLL_COLL11:
    return "COLL_COLL11";
  case COLL_COLL12:
    return "COLL_COLL12";
  case COLL_COLL13:
    return "COLL_COLL13";
  case COLL_COLL2:
    return "COLL_COLL2";
  case COLL_COLL3:
    return "COLL_COLL3";
  case COLL_COLL4:
    return "COLL_COLL4";
  case COLL_COLL5:
    return "COLL_COLL5";
  case COLL_COLL6:
    return "COLL_COLL6";
  case COLL_COLL7:
    return "COLL_COLL7";
  case COLL_COLL8:
    return "COLL_COLL8";
  case COLL_COLL9:
    return "COLL_COLL9";
  case COLL_EXSCAN:
    return "COLL_EXSCAN";
  case COLL_EXSCAN2:
    return "COLL_EXSCAN2";
  case COLL_GATHER:
    return "COLL_GATHER";
  case COLL_GATHER2:
    return "COLL_GATHER2";
  case COLL_IALLRED:
    return "COLL_IALLRED";
  case COLL_ICALLGATHER:
    return "COLL_ICALLGATHER";
  case COLL_ICALLGATHERV:
    return "COLL_ICALLGATHERV";
  case COLL_ICALLREDUCE:
    return "COLL_ICALLREDUCE";
  case COLL_ICALLTOALL:
    return "COLL_ICALLTOALL";
  case COLL_ICALLTOALLV:
    return "COLL_ICALLTOALLV";
  case COLL_ICALLTOALLW:
    return "COLL_ICALLTOALLW";
  case COLL_ICBARRIER:
    return "COLL_ICBARRIER";
  case COLL_ICBCAST:
    return "COLL_ICBCAST";
  case COLL_ICGATHER:
    return "COLL_ICGATHER";
  case COLL_ICGATHERV:
    return "COLL_ICGATHERV";
  case COLL_ICREDUCE:
    return "COLL_ICREDUCE";
  case COLL_ICSCATTER:
    return "COLL_ICSCATTER";
  case COLL_ICSCATTERV:
    return "COLL_ICSCATTERV";
  case COLL_LONGUSER:
    return "COLL_LONGUSER";
  case COLL_NONBLOCKING:
    return "COLL_NONBLOCKING";
  case COLL_NONBLOCKING2:
    return "COLL_NONBLOCKING2";
  case COLL_NONBLOCKING3:
    return "COLL_NONBLOCKING3";
  case COLL_OP_COMMUTATIVE:
    return "COLL_OP_COMMUTATIVE";
  case COLL_OPBAND:
    return "COLL_OPBAND";
  case COLL_OPBOR:
    return "COLL_OPBOR";
  case COLL_OPBXOR:
    return "COLL_OPBXOR";
  case COLL_OPLAND:
    return "COLL_OPLAND";
  case COLL_OPLOR:
    return "COLL_OPLOR";
  case COLL_OPLXOR:
    return "COLL_OPLXOR";
  case COLL_OPMAX:
    return "COLL_OPMAX";
  case COLL_OPMAXLOC:
    return "COLL_OPMAXLOC";
  case COLL_OPMIN:
    return "COLL_OPMIN";
  case COLL_OPMINLOC:
    return "COLL_OPMINLOC";
  case COLL_OPPROD:
    return "COLL_OPPROD";
  case COLL_OPSUM:
    return "COLL_OPSUM";
  case COLL_RED3:
    return "COLL_RED3";
  case COLL_RED4:
    return "COLL_RED4";
  case COLL_RED_SCAT_BLOCK:
    return "COLL_RED_SCAT_BLOCK";
  case COLL_RED_SCAT_BLOCK2:
    return "COLL_RED_SCAT_BLOCK2";
  case COLL_REDSCAT:
    return "COLL_REDSCAT";
  case COLL_REDSCAT2:
    return "COLL_REDSCAT2";
  case COLL_REDSCAT3:
    return "COLL_REDSCAT3";
  case COLL_REDSCATBKINTER:
    return "COLL_REDSCATBKINTER";
  case COLL_REDSCATBLK3:
    return "COLL_REDSCATBLK3";
  case COLL_REDSCATINTER:
    return "COLL_REDSCATINTER";
  case COLL_REDUCE:
    return "COLL_REDUCE";
  case COLL_REDUCE_LOCAL:
    return "COLL_REDUCE_LOCAL";
  case COLL_SCANTST:
    return "COLL_SCANTST";
  case COLL_SCATTER2:
    return "COLL_SCATTER2";
  case COLL_SCATTER3:
    return "COLL_SCATTER3";
  case COLL_SCATTERN:
    return "COLL_SCATTERN";
  case COLL_SCATTERV:
    return "COLL_SCATTERV";
  case COMM_CMFREE:
    return "COMM_CMFREE";
  case COMM_CMSPLIT:
    return "COMM_CMSPLIT";
  case COMM_CMSPLIT2:
    return "COMM_CMSPLIT2";
  case COMM_CMSPLIT_TYPE:
    return "COMM_CMSPLIT_TYPE";
  case COMM_COMM_CREATE_GROUP:
    return "COMM_COMM_CREATE_GROUP";
  case COMM_COMM_GROUP_HALF:
    return "COMM_COMM_GROUP_HALF";
  case COMM_COMM_GROUP_RAND:
    return "COMM_COMM_GROUP_RAND";
  case COMM_COMM_IDUP:
    return "COMM_COMM_IDUP";
  case COMM_COMMCREATE1:
    return "COMM_COMMCREATE1";
  case COMM_COMMNAME:
    return "COMM_COMMNAME";
  case COMM_CTXALLOC:
    return "COMM_CTXALLOC";
  case COMM_CTXSPLIT:
    return "COMM_CTXSPLIT";
  case COMM_DUP:
    return "COMM_DUP";
  case COMM_DUPIC:
    return "COMM_DUPIC";
  case COMM_IC1:
    return "COMM_IC1";
  case COMM_IC2:
    return "COMM_IC2";
  case COMM_ICCREATE:
    return "COMM_ICCREATE";
  case COMM_ICGROUP:
    return "COMM_ICGROUP";
  case COMM_ICM:
    return "COMM_ICM";
  case COMM_ICSPLIT:
    return "COMM_ICSPLIT";
  case COMM_PROBE_INTERCOMM:
    return "COMM_PROBE_INTERCOMM";
  case DATATYPE_BLOCKINDEXED_MISC:
    return "DATATYPE_BLOCKINDEXED_MISC";
  case DATATYPE_BLOCKINDEXED_ZERO_COUNT:
    return "DATATYPE_BLOCKINDEXED_ZERO_COUNT";
  case DATATYPE_CONTENTS:
    return "DATATYPE_CONTENTS";
  case DATATYPE_CONTIG_ZERO_COUNT:
    return "DATATYPE_CONTIG_ZERO_COUNT";
  case DATATYPE_CONTIGSTRUCT:
    return "DATATYPE_CONTIGSTRUCT";
  case DATATYPE_DARRAY_PACK:
    return "DATATYPE_DARRAY_PACK";
  case DATATYPE_GADDRESS:
    return "DATATYPE_GADDRESS";
  case DATATYPE_GET_ELEMENTS_PAIRTYPE:
    return "DATATYPE_GET_ELEMENTS_PAIRTYPE";
  case DATATYPE_GET_ELEMENTS:
    return "DATATYPE_GET_ELEMENTS";
  case DATATYPE_GETPARTELM:
    return "DATATYPE_GETPARTELM";
  case DATATYPE_HINDEXED_ZEROS:
    return "DATATYPE_HINDEXED_ZEROS";
  case DATATYPE_HINDEXED_BLOCK:
    return "DATATYPE_HINDEXED_BLOCK";
  case DATATYPE_HINDEXED_BLOCK_CONTENTS:
    return "DATATYPE_HINDEXED_BLOCK_CONTENTS";
  case DATATYPE_INDEXED_MISC:
    return "DATATYPE_INDEXED_MISC";
  case DATATYPE_LBUB:
    return "DATATYPE_LBUB";
  case DATATYPE_LOCALPACK:
    return "DATATYPE_LOCALPACK";
  case DATATYPE_LONGDOUBLE:
    return "DATATYPE_LONGDOUBLE";
  case DATATYPE_LOTS_OF_TYPES:
    return "DATATYPE_LOTS_OF_TYPES";
  case DATATYPE_PAIRTYPE_PACK:
    return "DATATYPE_PAIRTYPE_PACK";
  case DATATYPE_PAIRTYPE_SIZE_EXTENT:
    return "DATATYPE_PAIRTYPE_SIZE_EXTENT";
    //    case DATATYPE_SEGTEST: return "DATATYPE_SEGTEST";
  case DATATYPE_SIMPLE_COMMIT:
    return "DATATYPE_SIMPLE_COMMIT";
  case DATATYPE_SIMPLE_PACK_EXTERNAL:
    return "DATATYPE_SIMPLE_PACK_EXTERNAL";
  case DATATYPE_SIMPLE_PACK:
    return "DATATYPE_SIMPLE_PACK";
  case DATATYPE_SIMPLE_RESIZED:
    return "DATATYPE_SIMPLE_RESIZED";
  case DATATYPE_SIMPLE_SIZE_EXTENT:
    return "DATATYPE_SIMPLE_SIZE_EXTENT";
  case DATATYPE_SIZEDTYPES:
    return "DATATYPE_SIZEDTYPES";
  case DATATYPE_SLICE_PACK_EXTERNAL:
    return "DATATYPE_SLICE_PACK_EXTERNAL";
  case DATATYPE_SLICE_PACK:
    return "DATATYPE_SLICE_PACK";
  case DATATYPE_STRUCT_DERIVED_ZEROS:
    return "DATATYPE_STRUCT_DERIVED_ZEROS";
  case DATATYPE_STRUCT_EMPTY_EL:
    return "DATATYPE_STRUCT_EMPTY_EL";
  case DATATYPE_STRUCT_EZHOV:
    return "DATATYPE_STRUCT_EZHOV";
  case DATATYPE_STRUCT_NO_REAL_TYPES:
    return "DATATYPE_STRUCT_NO_REAL_TYPES";
  case DATATYPE_STRUCT_PACK:
    return "DATATYPE_STRUCT_PACK";
  case DATATYPE_STRUCT_VERYDEEP:
    return "DATATYPE_STRUCT_VERYDEEP";
  case DATATYPE_STRUCT_ZERO_COUNT:
    return "DATATYPE_STRUCT_ZERO_COUNT";
  case DATATYPE_SUBARRAY_PACK:
    return "DATATYPE_SUBARRAY_PACK";
  case DATATYPE_SUBARRAY:
    return "DATATYPE_SUBARRAY";
  case DATATYPE_TFREE:
    return "DATATYPE_TFREE";
  case DATATYPE_TMATCHSIZE:
    return "DATATYPE_TMATCHSIZE";
  case DATATYPE_TRANSPOSE_PACK:
    return "DATATYPE_TRANSPOSE_PACK";
  case DATATYPE_TRESIZED:
    return "DATATYPE_TRESIZED";
  case DATATYPE_TRESIZED2:
    return "DATATYPE_TRESIZED2";
  case DATATYPE_TRIANGULAR_PACK:
    return "DATATYPE_TRIANGULAR_PACK";
  case DATATYPE_TYPECOMMIT:
    return "DATATYPE_TYPECOMMIT";
  case DATATYPE_TYPEFREE:
    return "DATATYPE_TYPEFREE";
  case DATATYPE_TYPELB:
    return "DATATYPE_TYPELB";
  case DATATYPE_TYPENAME:
    return "DATATYPE_TYPENAME";
  case DATATYPE_UNPACK:
    return "DATATYPE_UNPACK";
  case DATATYPE_UNUSUAL_NONCONTIGS:
    return "DATATYPE_UNUSUAL_NONCONTIGS";
  case DATATYPE_ZERO_BLKLEN_VECTOR:
    return "DATATYPE_ZERO_BLKLEN_VECTOR";
  case DATATYPE_ZEROBLKS:
    return "DATATYPE_ZEROBLKS";
  case DATATYPE_ZEROPARMS:
    return "DATATYPE_ZEROPARMS";
  case ERRHAN_ADDERR:
    return "ERRHAN_ADDERR";
  case ERRHAN_COMMCALL:
    return "ERRHAN_COMMCALL";
  case ERRHAN_ERRCODE:
    return "ERRHAN_ERRCODE";
  case ERRHAN_ERRFATAL:
    return "ERRHAN_ERRFATAL";
    //    case ERRHAN_ERRMSG: return "ERRHAN_ERRMSG";
    //    case ERRHAN_ERRRING: return "ERRHAN_ERRRING";
    //    case ERRHAN_ERRSTRING: return "ERRHAN_ERRSTRING";
    //    case ERRHAN_PREDEF_EH: return "ERRHAN_PREDEF_EH";
    //   case GROUP_GLPID: return "GROUP_GLPID";
  case GROUP_GROUPCREATE:
    return "GROUP_GROUPCREATE";
  case GROUP_GROUPNULLINCL:
    return "GROUP_GROUPNULLINCL";
  case GROUP_GROUPTEST:
    return "GROUP_GROUPTEST";
  case GROUP_GROUPTEST2:
    return "GROUP_GROUPTEST2";
  case GROUP_GTRANKS:
    return "GROUP_GTRANKS";
    // case GROUP_GTRANKSPERF:
    //   return "GROUP_GTRANKSPERF";
  case INFO_INFODEL:
    return "INFO_INFODEL";
  case INFO_INFODUP:
    return "INFO_INFODUP";
  case INFO_INFOMANY:
    return "INFO_INFOMANY";
  case INFO_INFOMANY2:
    return "INFO_INFOMANY2";
  case INFO_INFOORDER:
    return "INFO_INFOORDER";
  case INFO_INFOTEST:
    return "INFO_INFOTEST";
  case INFO_INFOVALLEN:
    return "INFO_INFOVALLEN";
  case INIT_ATTRSELF:
    return "INIT_ATTRSELF";
  case INIT_EXITST1:
    return "INIT_EXITST1";
  case INIT_EXITST2:
    return "INIT_EXITST2";
  case INIT_EXITST3:
    return "INIT_EXITST3";
  case INIT_FINALIZED:
    return "INIT_FINALIZED";
  case INIT_INITSTAT:
    return "INIT_INITSTAT";
    //   case INIT_TIMEOUT: return "INIT_TIMEOUT";
  case INIT_VERSION:
    return "INIT_VERSION";
  case PT2PT_ANYALL:
    return "PT2PT_ANYALL";
  case PT2PT_BOTTOM:
    return "PT2PT_BOTTOM";
  case PT2PT_BSEND1:
    return "PT2PT_BSEND1";
  case PT2PT_BSEND2:
    return "PT2PT_BSEND2";
  case PT2PT_BSEND3:
    return "PT2PT_BSEND3";
  case PT2PT_BSEND4:
    return "PT2PT_BSEND4";
  case PT2PT_BSEND5:
    return "PT2PT_BSEND5";
  case PT2PT_BSENDALIGN:
    return "PT2PT_BSENDALIGN";
  case PT2PT_BSENDFRAG:
    return "PT2PT_BSENDFRAG";
  case PT2PT_BSENDPENDING:
    return "PT2PT_BSENDPENDING";
  case PT2PT_CANCELRECV:
    return "PT2PT_CANCELRECV";
  case PT2PT_EAGERDT:
    return "PT2PT_EAGERDT";
  case PT2PT_GREQ1:
    return "PT2PT_GREQ1";
  case PT2PT_ICSEND:
    return "PT2PT_ICSEND";
  case PT2PT_INACTIVEREQ:
    return "PT2PT_INACTIVEREQ";
  case PT2PT_ISENDSELF:
    return "PT2PT_ISENDSELF";
    //    case PT2PT_ISENDSELFPROBE:return "PT2PT_ISENDSELFPROBE";
  case PT2PT_LARGE_MESSAGE:
    return "PT2PT_LARGE_MESSAGE";
  case PT2PT_MPROBE:
    return "PT2PT_MPROBE";
  case PT2PT_PINGPING:
    return "PT2PT_PINGPING";
  case PT2PT_PROBE_UNEXP:
    return "PT2PT_PROBE_UNEXP";
  case PT2PT_PROBENULL:
    return "PT2PT_PROBENULL";
  case PT2PT_PSCANCEL:
    return "PT2PT_PSCANCEL";
  case PT2PT_RCANCEL:
    return "PT2PT_RCANCEL";
  case PT2PT_RQFREEB:
    return "PT2PT_RQFREEB";
  case PT2PT_RQSTATUS:
    return "PT2PT_RQSTATUS";
  case PT2PT_SCANCEL:
    return "PT2PT_SCANCEL";
  case PT2PT_SCANCEL2:
    return "PT2PT_SCANCEL2";
  case PT2PT_SENDALL:
    return "PT2PT_SENDALL";
  case PT2PT_SENDFLOOD:
    return "PT2PT_SENDFLOOD";
  case PT2PT_SENDRECV1:
    return "PT2PT_SENDRECV1";
  case PT2PT_SENDRECV2:
    return "PT2PT_SENDRECV2";
  case PT2PT_SENDRECV3:
    return "PT2PT_SENDRECV3";
  case PT2PT_SENDSELF:
    return "PT2PT_SENDSELF";
  case PT2PT_WAITANY_NULL:
    return "PT2PT_WAITANY_NULL";
  case PT2PT_WAITTESTNULL:
    return "PT2PT_WAITTESTNULL";
  case TOPO_CARTCREATES:
    return "TOPO_CARTCREATES";
  case TOPO_CARTMAP1:
    return "TOPO_CARTMAP1";
  case TOPO_CARTSHIFT1:
    return "TOPO_CARTSHIFT1";
  case TOPO_CARTSUBALL:
    return "TOPO_CARTSUBALL";
  case TOPO_CARTZERO:
    return "TOPO_CARTZERO";
  case TOPO_DGRAPH_UNWGT:
    return "TOPO_DGRAPH_UNWGT";
  case TOPO_DIMS1:
    return "TOPO_DIMS1";
  case TOPO_DIMS2:
    return "TOPO_DIMS2";
  case TOPO_DISTGRAPH1:
    return "TOPO_DISTGRAPH1";
  case TOPO_GRAPHCR:
    return "TOPO_GRAPHCR";
  case TOPO_GRAPHCR2:
    return "TOPO_GRAPHCR2";
  case TOPO_GRAPHMAP1:
    return "TOPO_GRAPHMAP1";
  case TOPO_NEIGHB_COLL:
    return "TOPO_NEIGHB_COLL";
  case TOPO_TOPODUP:
    return "TOPO_TOPODUP";
  case TOPO_TOPOTEST:
    return "TOPO_TOPOTEST";

  case RMA_ACCFENCE1:
    return "RMA_ACCFENCE1";
  case RMA_ACCFENCE2_AM:
    return "RMA_ACCFENCE2_AM";
  case RMA_ACCFENCE2:
    return "RMA_ACCFENCE2";
  case RMA_ALLOCMEM:
    return "RMA_ACCFENCE2";
  case RMA_ATTRORDERWIN:
    return "RMA_ATTRORDERWIN";
  case RMA_BASEATTRWIN:
    return "RMA_BASEATTRWIN";
  case RMA_CONTIG_DISPL:
    return "RMA_BASEATTRWIN";
  case RMA_FETCHANDADD_AM:
    return "RMA_FETCHANDADD_AM";
  case RMA_FETCHANDADD_TREE_AM:
    return "RMA_FETCHANDADD_AM";
  case RMA_FETCHANDADD_TREE:
    return "RMA_FETCHANDADD_AM";
  case RMA_FETCHANDADD:
    return "RMA_FETCHANDADD";
  case RMA_FKEYVALWIN:
    return "RMA_FETCHANDADD";
  case RMA_GETFENCE1:
    return "RMA_GETFENCE1";
  case RMA_GETGROUP:
    return "RMA_GETGROUP";
  case RMA_IRCPI:
    return "RMA_IRCPI";
  case RMA_LOCKCONTENTION:
    return "RMA_LOCKCONTENTION";
  case RMA_LOCKNULL:
    return "RMA_LOCKNULL";
  case RMA_MIXEDSYNC:
    return "RMA_MIXEDSYNC";
  case RMA_NULLPSCW:
    return "RMA_NULLPSCW";
  case RMA_PUTFENCE1:
    return "RMA_PUTFENCE1";
  case RMA_PUTFIDX:
    return "RMA_PUTFIDX";
  case RMA_PUTPSCW1:
    return "RMA_PUTPSCW1";
  case RMA_SELFRMA:
    return "RMA_SELFRMA";
  case RMA_TEST1_AM:
    return "RMA_TEST1_AM";
  case RMA_TEST1:
    return "RMA_TEST1";
  case RMA_TEST2_AM:
    return "RMA_TEST2_AM";
  case RMA_TEST2:
    return "RMA_TEST2";
  case RMA_TEST3_AM:
    return "RMA_TEST3_AM";
  case RMA_TEST3:
    return "RMA_TEST3";
  case RMA_TEST4_AM:
    return "RMA_TEST4_AM";
  case RMA_TEST4:
    return "RMA_TEST4";
  case RMA_TEST5_AM:
    return "RMA_TEST5_AM";
  case RMA_TEST5:
    return "RMA_TEST5";
  case RMA_TRANSPOSE1:
    return "RMA_TRANSPOSE1";
  case RMA_TRANSPOSE2:
    return "RMA_TRANSPOSE2";
  case RMA_TRANSPOSE3:
    return "RMA_TRANSPOSE3";
  case RMA_TRANSPOSE4:
    return "RMA_TRANSPOSE4";
  case RMA_TRANSPOSE5:
    return "RMA_TRANSPOSE5";
  case RMA_TRANSPOSE6:
    return "RMA_TRANSPOSE6";
  case RMA_TRANSPOSE7:
    return "RMA_TRANSPOSE6";
  case RMA_WINCALL:
    return "RMA_WINCALL";
  case RMA_WINNAME:
    return "RMA_WINNAME";
  case RMA_WINTEST:
    return "RMA_WINTEST";

  default:
    return "not a valid test mode";

  }

}

