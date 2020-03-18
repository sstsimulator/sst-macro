/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#if !defined(USE_MPI)
# error "You should specify USE_MPI=0 or USE_MPI=1 on the compile line"
#endif


// OpenMP will be compiled in if this flag is set to 1 AND the compiler beging
// used supports it (i.e. the _OPENMP symbol is defined)
#define USE_OMP 1

#define heisenbug printf("%s:%d\n", __FILE__, __LINE__)

#if USE_MPI
#include <mpi.h>

/*
   define one of these three symbols:

   SEDOV_SYNC_POS_VEL_NONE
   SEDOV_SYNC_POS_VEL_EARLY
   SEDOV_SYNC_POS_VEL_LATE
*/

#define SEDOV_SYNC_POS_VEL_EARLY 1
#endif

#include <math.h>
#include <vector>
#include <sys/time.h>
#include <cstring>

//**************************************************
// Allow flexibility for arithmetic representations 
//**************************************************

#define MAX(a, b) ( ((a) > (b)) ? (a) : (b))


// Precision specification
typedef float        real4 ;
typedef double       real8 ;
typedef long double  real10 ;  // 10 bytes on x86

typedef int    Index_t ; // array subscript and loop index
typedef real8  Real_t ;  // floating point representation
typedef int    Int_t ;   // integer representation

enum { VolumeError = -1, QStopError = -2 } ;

inline real4  SQRT(real4  arg) { return sqrtf(arg) ; }
inline real8  SQRT(real8  arg) { return sqrt(arg) ; }
inline real10 SQRT(real10 arg) { return sqrtl(arg) ; }

inline real4  CBRT(real4  arg) { return cbrtf(arg) ; }
inline real8  CBRT(real8  arg) { return cbrt(arg) ; }
inline real10 CBRT(real10 arg) { return cbrtl(arg) ; }

inline real4  FABS(real4  arg) { return fabsf(arg) ; }
inline real8  FABS(real8  arg) { return fabs(arg) ; }
inline real10 FABS(real10 arg) { return fabsl(arg) ; }


// Stuff needed for boundary conditions
// 2 BCs on each of 6 hexahedral faces (12 bits)
#define XI_M        0x00007
#define XI_M_SYMM   0x00001
#define XI_M_FREE   0x00002
#define XI_M_COMM   0x00004

#define XI_P        0x00038
#define XI_P_SYMM   0x00008
#define XI_P_FREE   0x00010
#define XI_P_COMM   0x00020

#define ETA_M       0x001c0
#define ETA_M_SYMM  0x00040
#define ETA_M_FREE  0x00080
#define ETA_M_COMM  0x00100

#define ETA_P       0x00e00
#define ETA_P_SYMM  0x00200
#define ETA_P_FREE  0x00400
#define ETA_P_COMM  0x00800

#define ZETA_M      0x07000
#define ZETA_M_SYMM 0x01000
#define ZETA_M_FREE 0x02000
#define ZETA_M_COMM 0x04000

#define ZETA_P      0x38000
#define ZETA_P_SYMM 0x08000
#define ZETA_P_FREE 0x10000
#define ZETA_P_COMM 0x20000

// MPI Message Tags
#define MSG_COMM_SBN      1024
#define MSG_SYNC_POS_VEL  2048
#define MSG_MONOQ         3072

#define MAX_FIELDS_PER_MPI_COMM 6

// Assume 128 byte coherence
// Assume Real_t is an "integral power of 2" bytes wide
#define CACHE_COHERENCE_PAD_REAL (128 / sizeof(Real_t))

#define CACHE_ALIGN_REAL(n) \
   (((n) + (CACHE_COHERENCE_PAD_REAL - 1)) & ~(CACHE_COHERENCE_PAD_REAL-1))

//////////////////////////////////////////////////////
// Primary data structure
//////////////////////////////////////////////////////

/*
 * The implementation of the data abstraction used for lulesh
 * resides entirely in the Domain class below.  You can change
 * grouping and interleaving of fields here to maximize data layout
 * efficiency for your underlying architecture or compiler.
 *
 * For example, fields can be implemented as STL objects or
 * raw array pointers.  As another example, individual fields
 * m_x, m_y, m_z could be budled into
 *
 *    struct { Real_t x, y, z ; } *m_coord ;
 *
 * allowing accessor functions such as
 *
 *  "Real_t &x(Index_t idx) { return m_coord[idx].x ; }"
 *  "Real_t &y(Index_t idx) { return m_coord[idx].y ; }"
 *  "Real_t &z(Index_t idx) { return m_coord[idx].z ; }"
 */

class Domain {

   public:

   // Constructor
   Domain(Int_t numRanks, Index_t colLoc,
          Index_t rowLoc, Index_t planeLoc,
          Index_t nx, Int_t tp, Int_t nr, Int_t balance, Int_t cost);

   ~Domain();

   //
   // ALLOCATION
   //

   void AllocateNodePersistent(Int_t numNode) // Node-centered
   {
      m_x.resize(numNode);  // coordinates
      m_y.resize(numNode);
      m_z.resize(numNode);

      m_xd.resize(numNode); // velocities
      m_yd.resize(numNode);
      m_zd.resize(numNode);

      m_xdd.resize(numNode); // accelerations
      m_ydd.resize(numNode);
      m_zdd.resize(numNode);

      m_fx.resize(numNode);  // forces
      m_fy.resize(numNode);
      m_fz.resize(numNode);

      m_nodalMass.resize(numNode);  // mass
   }

   void AllocateElemPersistent(Int_t numElem) // Elem-centered
   {
      m_nodelist.resize(8*numElem);

      // elem connectivities through face
      m_lxim.resize(numElem);
      m_lxip.resize(numElem);
      m_letam.resize(numElem);
      m_letap.resize(numElem);
      m_lzetam.resize(numElem);
      m_lzetap.resize(numElem);

      m_elemBC.resize(numElem);

      m_e.resize(numElem);
      m_p.resize(numElem);

      m_q.resize(numElem);
      m_ql.resize(numElem);
      m_qq.resize(numElem);

      m_v.resize(numElem);

      m_volo.resize(numElem);
      m_delv.resize(numElem);
      m_vdov.resize(numElem);

      m_arealg.resize(numElem);

      m_ss.resize(numElem);

      m_elemMass.resize(numElem);
   }

   void AllocateGradients(Int_t numElem, Int_t allElem)
   {
      // Position gradients
      m_delx_xi.resize(numElem) ;
      m_delx_eta.resize(numElem) ;
      m_delx_zeta.resize(numElem) ;

      // Velocity gradients
      m_delv_xi.resize(allElem) ;
      m_delv_eta.resize(allElem);
      m_delv_zeta.resize(allElem) ;
   }

   void DeallocateGradients()
   {
      m_delx_zeta.clear() ;
      m_delx_eta.clear() ;
      m_delx_xi.clear() ;

      m_delv_zeta.clear() ;
      m_delv_eta.clear() ;
      m_delv_xi.clear() ;
   }

   void AllocateStrains(Int_t numElem)
   {
      m_dxx.resize(numElem) ;
      m_dyy.resize(numElem) ;
      m_dzz.resize(numElem) ;
   }

   void DeallocateStrains()
   {
      m_dzz.clear() ;
      m_dyy.clear() ;
      m_dxx.clear() ;
   }
   
   //
   // ACCESSORS
   //

   // Node-centered

   // Nodal coordinates
#pragma sst delete
   Real_t& x(Index_t idx)    { return m_x[idx] ; }
#pragma sst delete
   Real_t& y(Index_t idx)    { return m_y[idx] ; }
#pragma sst delete
   Real_t& z(Index_t idx)    { return m_z[idx] ; }

   // Nodal velocities
#pragma sst delete
   Real_t& xd(Index_t idx)   { return m_xd[idx] ; }
#pragma sst delete
   Real_t& yd(Index_t idx)   { return m_yd[idx] ; }
#pragma sst delete
   Real_t& zd(Index_t idx)   { return m_zd[idx] ; }

   // Nodal accelerations
#pragma sst delete
   Real_t& xdd(Index_t idx)  { return m_xdd[idx] ; }
#pragma sst delete
   Real_t& ydd(Index_t idx)  { return m_ydd[idx] ; }
#pragma sst delete
   Real_t& zdd(Index_t idx)  { return m_zdd[idx] ; }

   // Nodal forces
#pragma sst delete
   Real_t& fx(Index_t idx)   { return m_fx[idx] ; }
#pragma sst delete
   Real_t& fy(Index_t idx)   { return m_fy[idx] ; }
#pragma sst delete
   Real_t& fz(Index_t idx)   { return m_fz[idx] ; }

   // Nodal mass
#pragma sst delete
   Real_t& nodalMass(Index_t idx) { return m_nodalMass[idx] ; }

   // Nodes on symmertry planes
#pragma sst delete
   Index_t symmX(Index_t idx) { return m_symmX[idx] ; }
#pragma sst delete
   Index_t symmY(Index_t idx) { return m_symmY[idx] ; }
#pragma sst delete
   Index_t symmZ(Index_t idx) { return m_symmZ[idx] ; }
   bool symmXempty()          { return m_symmX.empty(); }
   bool symmYempty()          { return m_symmY.empty(); }
   bool symmZempty()          { return m_symmZ.empty(); }

   //
   // Element-centered
   //
   Index_t&  regElemSize(Index_t idx) { return m_regElemSize[idx] ; }
#pragma sst delete
   Index_t&  regNumList(Index_t idx) { return m_regNumList[idx] ; }
#pragma sst delete
   Index_t*  regNumList()            { return &m_regNumList[0] ; }
   Index_t*  regElemlist(Int_t r)    { 
#pragma sst return nullptr
    return m_regElemlist[r] ; 
  }
#pragma sst delete
   Index_t&  regElemlist(Int_t r, Index_t idx) { return m_regElemlist[r][idx] ; }
#pragma sst delete
   Index_t*  nodelist(Index_t idx)    { return &m_nodelist[Index_t(8)*idx] ; }

   // elem connectivities through face
#pragma sst delete
   Index_t&  lxim(Index_t idx) { return m_lxim[idx] ; }
#pragma sst delete
   Index_t&  lxip(Index_t idx) { return m_lxip[idx] ; }
#pragma sst delete
   Index_t&  letam(Index_t idx) { return m_letam[idx] ; }
#pragma sst delete
   Index_t&  letap(Index_t idx) { return m_letap[idx] ; }
#pragma sst delete
   Index_t&  lzetam(Index_t idx) { return m_lzetam[idx] ; }
#pragma sst delete
   Index_t&  lzetap(Index_t idx) { return m_lzetap[idx] ; }

   // elem face symm/free-surface flag
#pragma sst delete
   Int_t&  elemBC(Index_t idx) { return m_elemBC[idx] ; }

   // Principal strains - temporary
#pragma sst delete
   Real_t& dxx(Index_t idx)  { return m_dxx[idx] ; }
#pragma sst delete
   Real_t& dyy(Index_t idx)  { return m_dyy[idx] ; }
#pragma sst delete
   Real_t& dzz(Index_t idx)  { return m_dzz[idx] ; }

   // Velocity gradient - temporary
#pragma sst delete
   Real_t& delv_xi(Index_t idx)    { return m_delv_xi[idx] ; }
#pragma sst delete
   Real_t& delv_eta(Index_t idx)   { return m_delv_eta[idx] ; }
#pragma sst delete
   Real_t& delv_zeta(Index_t idx)  { return m_delv_zeta[idx] ; }

   // Position gradient - temporary
#pragma sst delete
   Real_t& delx_xi(Index_t idx)    { return m_delx_xi[idx] ; }
#pragma sst delete
   Real_t& delx_eta(Index_t idx)   { return m_delx_eta[idx] ; }
#pragma sst delete
   Real_t& delx_zeta(Index_t idx)  { return m_delx_zeta[idx] ; }

   // Energy
#pragma sst delete
   Real_t& e(Index_t idx)          { return m_e[idx] ; }

   // Pressure
#pragma sst delete
   Real_t& p(Index_t idx)          { return m_p[idx] ; }

   // Artificial viscosity
#pragma sst delete
   Real_t& q(Index_t idx)          { return m_q[idx] ; }

   // Linear term for q
#pragma sst delete
   Real_t& ql(Index_t idx)         { return m_ql[idx] ; }
   // Quadratic term for q
#pragma sst delete
   Real_t& qq(Index_t idx)         { return m_qq[idx] ; }

   // Relative volume
#pragma sst delete
   Real_t& v(Index_t idx)          { return m_v[idx] ; }
#pragma sst delete
   Real_t& delv(Index_t idx)       { return m_delv[idx] ; }

   // Reference volume
#pragma sst delete
   Real_t& volo(Index_t idx)       { return m_volo[idx] ; }

   // volume derivative over volume
#pragma sst delete
   Real_t& vdov(Index_t idx)       { return m_vdov[idx] ; }

   // Element characteristic length
#pragma sst delete
   Real_t& arealg(Index_t idx)     { return m_arealg[idx] ; }

   // Sound speed
#pragma sst delete
   Real_t& ss(Index_t idx)         { return m_ss[idx] ; }

   // Element mass
#pragma sst delete
   Real_t& elemMass(Index_t idx)  { return m_elemMass[idx] ; }

#pragma sst delete
   Index_t nodeElemCount(Index_t idx)
   { return m_nodeElemStart[idx+1] - m_nodeElemStart[idx] ; }

#pragma sst delete
   Index_t *nodeElemCornerList(Index_t idx)
   { return &m_nodeElemCornerList[m_nodeElemStart[idx]] ; }

   // Parameters 

   // Cutoffs
   Real_t u_cut() const               { return m_u_cut ; }
   Real_t e_cut() const               { return m_e_cut ; }
   Real_t p_cut() const               { return m_p_cut ; }
   Real_t q_cut() const               { return m_q_cut ; }
   Real_t v_cut() const               { return m_v_cut ; }

   // Other constants (usually are settable via input file in real codes)
   Real_t hgcoef() const              { return m_hgcoef ; }
   Real_t qstop() const               { return m_qstop ; }
   Real_t monoq_max_slope() const     { return m_monoq_max_slope ; }
   Real_t monoq_limiter_mult() const  { return m_monoq_limiter_mult ; }
   Real_t ss4o3() const               { return m_ss4o3 ; }
   Real_t qlc_monoq() const           { return m_qlc_monoq ; }
   Real_t qqc_monoq() const           { return m_qqc_monoq ; }
   Real_t qqc() const                 { return m_qqc ; }

   Real_t eosvmax() const             { return m_eosvmax ; }
   Real_t eosvmin() const             { return m_eosvmin ; }
   Real_t pmin() const                { return m_pmin ; }
   Real_t emin() const                { return m_emin ; }
   Real_t dvovmax() const             { return m_dvovmax ; }
   Real_t refdens() const             { return m_refdens ; }

   // Timestep controls, etc...
   Real_t& time()                 { return m_time ; }
   Real_t& deltatime()            { return m_deltatime ; }
   Real_t& deltatimemultlb()      { return m_deltatimemultlb ; }
   Real_t& deltatimemultub()      { return m_deltatimemultub ; }
   Real_t& stoptime()             { return m_stoptime ; }
   Real_t& dtcourant()            { return m_dtcourant ; }
   Real_t& dthydro()              { return m_dthydro ; }
   Real_t& dtmax()                { return m_dtmax ; }
   Real_t& dtfixed()              { return m_dtfixed ; }

   Int_t&  cycle()                { return m_cycle ; }
   Index_t&  numRanks()           { return m_numRanks ; }

   Index_t&  colLoc()             { return m_colLoc ; }
   Index_t&  rowLoc()             { return m_rowLoc ; }
   Index_t&  planeLoc()           { return m_planeLoc ; }
   Index_t&  tp()                 { return m_tp ; }

   Index_t&  sizeX()              { return m_sizeX ; }
   Index_t&  sizeY()              { return m_sizeY ; }
   Index_t&  sizeZ()              { return m_sizeZ ; }
   Index_t&  numReg()             { return m_numReg ; }
   Int_t&  cost()             { return m_cost ; }
   Index_t&  numElem()            { return m_numElem ; }
   Index_t&  numNode()            { return m_numNode ; }
   
   Index_t&  maxPlaneSize()       { return m_maxPlaneSize ; }
   Index_t&  maxEdgeSize()        { return m_maxEdgeSize ; }
   
   //
   // MPI-Related additional data
   //

#if USE_MPI   
   // Communication Work space 
   Real_t *commDataSend ;
   Real_t *commDataRecv ;
   
   // Maximum number of block neighbors 
   MPI_Request recvRequest[26] ; // 6 faces + 12 edges + 8 corners 
   MPI_Request sendRequest[26] ; // 6 faces + 12 edges + 8 corners 
#endif


  private:

   void BuildMesh(Int_t nx, Int_t edgeNodes, Int_t edgeElems);
   void SetupThreadSupportStructures();
   void CreateRegionIndexSets(Int_t nreg, Int_t balance);
   void SetupCommBuffers(Int_t edgeNodes);
   void SetupSymmetryPlanes(Int_t edgeNodes);
   void SetupElementConnectivities(Int_t edgeElems);
   void SetupBoundaryConditions(Int_t edgeElems);

   //
   // IMPLEMENTATION
   //

   /* Node-centered */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_x ;  /* coordinates */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_y ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_z ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_xd ; /* velocities */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_yd ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_zd ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_xdd ; /* accelerations */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_ydd ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_zdd ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_fx ;  /* forces */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_fy ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_fz ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_nodalMass ;  /* mass */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t> m_symmX ;  /* symmetry plane nodesets */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t> m_symmY ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t> m_symmZ ;

   // Element-centered

   // Region information
   Int_t    m_numReg ;
   Int_t    m_cost; //imbalance cost
   Index_t *m_regElemSize ;   // Size of region sets
#pragma sst null_ptr
   Index_t *m_regNumList ;    // Region number per domain element
#pragma sst null_ptr
   Index_t **m_regElemlist ;  // region indexset
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_nodelist ;     /* elemToNode connectivity */


#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_lxim ;  /* element connectivity across each face */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_lxip ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_letam ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_letap ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_lzetam ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Index_t>  m_lzetap ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Int_t>    m_elemBC ;  /* symmetry/free-surface flags for each elem face */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_dxx ;  /* principal strains -- temporary */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_dyy ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_dzz ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delv_xi ;    /* velocity gradient -- temporary */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delv_eta ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delv_zeta ;

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delx_xi ;    /* coordinate gradient -- temporary */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delx_eta ;
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delx_zeta ;
   
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_e ;   /* energy */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_p ;   /* pressure */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_q ;   /* q */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_ql ;  /* linear term for q */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_qq ;  /* quadratic term for q */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_v ;     /* relative volume */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_volo ;  /* reference volume */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_vnew ;  /* new relative volume -- temporary */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_delv ;  /* m_vnew - m_v */
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_vdov ;  /* volume derivative over volume */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_arealg ;  /* characteristic length of an element */
   
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_ss ;      /* "sound speed" */

#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_elemMass ;  /* mass */

   // Cutoffs (treat as constants)
   const Real_t  m_e_cut ;             // energy tolerance 
   const Real_t  m_p_cut ;             // pressure tolerance 
   const Real_t  m_q_cut ;             // q tolerance 
   const Real_t  m_v_cut ;             // relative volume tolerance 
   const Real_t  m_u_cut ;             // velocity tolerance 

   // Other constants (usually setable, but hardcoded in this proxy app)

   const Real_t  m_hgcoef ;            // hourglass control 
   const Real_t  m_ss4o3 ;
   const Real_t  m_qstop ;             // excessive q indicator 
   const Real_t  m_monoq_max_slope ;
   const Real_t  m_monoq_limiter_mult ;
   const Real_t  m_qlc_monoq ;         // linear term coef for q 
   const Real_t  m_qqc_monoq ;         // quadratic term coef for q 
   const Real_t  m_qqc ;
   const Real_t  m_eosvmax ;
   const Real_t  m_eosvmin ;
   const Real_t  m_pmin ;              // pressure floor 
   const Real_t  m_emin ;              // energy floor 
   const Real_t  m_dvovmax ;           // maximum allowable volume change 
   const Real_t  m_refdens ;           // reference density 

   // Variables to keep track of timestep, simulation time, and cycle
   Real_t  m_dtcourant ;         // courant constraint 
   Real_t  m_dthydro ;           // volume change constraint 
   Int_t   m_cycle ;             // iteration count for simulation 
   Real_t  m_dtfixed ;           // fixed time increment 
   Real_t  m_time ;              // current time 
   Real_t  m_deltatime ;         // variable time increment 
   Real_t  m_deltatimemultlb ;
   Real_t  m_deltatimemultub ;
   Real_t  m_dtmax ;             // maximum allowable time increment 
   Real_t  m_stoptime ;          // end time for simulation 


   Int_t   m_numRanks ;

   Index_t m_colLoc ;
   Index_t m_rowLoc ;
   Index_t m_planeLoc ;
   Index_t m_tp ;

   Index_t m_sizeX ;
   Index_t m_sizeY ;
   Index_t m_sizeZ ;
   Index_t m_numElem ;
   Index_t m_numNode ;

   Index_t m_maxPlaneSize ;
   Index_t m_maxEdgeSize ;

   // OMP hack 
   Index_t *m_nodeElemStart ;
   Index_t *m_nodeElemCornerList ;

   // Used in setup
   Index_t m_rowMin, m_rowMax;
   Index_t m_colMin, m_colMax;
   Index_t m_planeMin, m_planeMax ;

 public:
  typedef enum {
  CalcLagrangeElements=0,
  CalcQForElems=1,
  UpdateVolumesForElems=2,
  ApplyMaterialPropertiesForElems=3,
  CalcTimeConstraintsForElems=4,
  CalcAccelerationForNodes=5,
  InitStressTermsForElems=6,
  IntegrateStressForElems=7,
  CalcHourglassControlForElems=8,
  ApplyAccelerationBoundaryConditionsForNodes=9,
  CalcVelocityForNodes=10,
  CalcPositionForNodes=11,
  TimerEnd=12
  } Timer_t;

  void start_timer(){
   gettimeofday(&t_start,NULL);
  }

  void stop_timer(Timer_t t){
   timeval t_stop;
   gettimeofday(&t_stop, NULL);
   double delta_t = (t_stop.tv_sec - t_start.tv_sec) + 1e-6*(t_stop.tv_usec-t_start.tv_usec);
   timers[t] += delta_t;
  }

  void dump_timers();

 private:
  double timers[TimerEnd];
  timeval t_start;
} ;

typedef Real_t &(Domain::* Domain_member )(Index_t) ;

struct cmdLineOpts {
   Int_t its; // -i 
   Int_t nx;  // -s 
   Int_t numReg; // -r 
   Int_t numFiles; // -f
   Int_t showProg; // -p
   Int_t quiet; // -q
   Int_t viz; // -v 
   Int_t cost; // -c
   Int_t balance; // -b
};



// Function Prototypes

static inline
Real_t CalcElemVolume( const Real_t x0, const Real_t x1,
               const Real_t x2, const Real_t x3,
               const Real_t x4, const Real_t x5,
               const Real_t x6, const Real_t x7,
               const Real_t y0, const Real_t y1,
               const Real_t y2, const Real_t y3,
               const Real_t y4, const Real_t y5,
               const Real_t y6, const Real_t y7,
               const Real_t z0, const Real_t z1,
               const Real_t z2, const Real_t z3,
               const Real_t z4, const Real_t z5,
               const Real_t z6, const Real_t z7 )
{
  Real_t twelveth = Real_t(1.0)/Real_t(12.0);

  Real_t dx61 = x6 - x1;
  Real_t dy61 = y6 - y1;
  Real_t dz61 = z6 - z1;

  Real_t dx70 = x7 - x0;
  Real_t dy70 = y7 - y0;
  Real_t dz70 = z7 - z0;

  Real_t dx63 = x6 - x3;
  Real_t dy63 = y6 - y3;
  Real_t dz63 = z6 - z3;

  Real_t dx20 = x2 - x0;
  Real_t dy20 = y2 - y0;
  Real_t dz20 = z2 - z0;

  Real_t dx50 = x5 - x0;
  Real_t dy50 = y5 - y0;
  Real_t dz50 = z5 - z0;

  Real_t dx64 = x6 - x4;
  Real_t dy64 = y6 - y4;
  Real_t dz64 = z6 - z4;

  Real_t dx31 = x3 - x1;
  Real_t dy31 = y3 - y1;
  Real_t dz31 = z3 - z1;

  Real_t dx72 = x7 - x2;
  Real_t dy72 = y7 - y2;
  Real_t dz72 = z7 - z2;

  Real_t dx43 = x4 - x3;
  Real_t dy43 = y4 - y3;
  Real_t dz43 = z4 - z3;

  Real_t dx57 = x5 - x7;
  Real_t dy57 = y5 - y7;
  Real_t dz57 = z5 - z7;

  Real_t dx14 = x1 - x4;
  Real_t dy14 = y1 - y4;
  Real_t dz14 = z1 - z4;

  Real_t dx25 = x2 - x5;
  Real_t dy25 = y2 - y5;
  Real_t dz25 = z2 - z5;

#define TRIPLE_PRODUCT(x1, y1, z1, x2, y2, z2, x3, y3, z3) \
   ((x1)*((y2)*(z3) - (z2)*(y3)) + (x2)*((z1)*(y3) - (y1)*(z3)) + (x3)*((y1)*(z2) - (z1)*(y2)))

  Real_t volume =
    TRIPLE_PRODUCT(dx31 + dx72, dx63, dx20,
       dy31 + dy72, dy63, dy20,
       dz31 + dz72, dz63, dz20) +
    TRIPLE_PRODUCT(dx43 + dx57, dx64, dx70,
       dy43 + dy57, dy64, dy70,
       dz43 + dz57, dz64, dz70) +
    TRIPLE_PRODUCT(dx14 + dx25, dx61, dx50,
       dy14 + dy25, dy61, dy50,
       dz14 + dz25, dz61, dz50);

#undef TRIPLE_PRODUCT

  volume *= twelveth;

  return volume ;
}

// lulesh-par
static inline
Real_t CalcElemVolume(const Real_t x[8], const Real_t y[8], const Real_t z[8]){
  return CalcElemVolume( x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
                         y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7],
                         z[0], z[1], z[2], z[3], z[4], z[5], z[6], z[7]);
}

// lulesh-util
void ParseCommandLineOptions(int argc, char *argv[],
                             Int_t myRank, struct cmdLineOpts *opts);
void VerifyAndWriteFinalOutput(Real_t elapsed_time,
                               Domain& locDom,
                               Int_t nx,
                               Int_t numRanks);

// lulesh-viz
void DumpToVisit(Domain& domain, int numFiles, int myRank, int numRanks);

// lulesh-comm
void CommRecv(Domain& domain, Int_t msgType, Index_t xferFields,
              Index_t dx, Index_t dy, Index_t dz,
              bool doRecv, bool planeOnly);
void CommSend(Domain& domain, Int_t msgType,
              Index_t xferFields, Domain_member *fieldData,
              Index_t dx, Index_t dy, Index_t dz,
              bool doSend, bool planeOnly);
void CommSBN(Domain& domain, Int_t xferFields, Domain_member *fieldData);
void CommSyncPosVel(Domain& domain);
void CommMonoQ(Domain& domain);

// lulesh-init
void InitMeshDecomp(Int_t numRanks, Int_t myRank,
                    Int_t *col, Int_t *row, Int_t *plane, Int_t *side);
