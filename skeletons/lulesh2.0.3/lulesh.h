#if !defined(USE_MPI)
# error "You should specify USE_MPI=0 or USE_MPI=1 on the compile line"
#endif


// OpenMP will be compiled in if this flag is set to 1 AND the compiler beging
// used supports it (i.e. the _OPENMP symbol is defined)
#define USE_OMP 1

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

#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
#include <variable.h>

#if _OPENMP
#error The SST model does not support OpenMP.
#endif
#endif

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

#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
typedef Variable<Real_t> Real_t_sim;
typedef VariablePtr<Real_t> Real_t_ptr_sim;
typedef VariablePtr<Real_t> Real_t_ptr_const_sim;
typedef VariablePtr<Real_t> const_Real_t_ptr_sim;
typedef VariablePtr<Real_t> const_Real_t_ptr_const_sim;
typedef VariablePtr<Real_t> Real_t_vec_sim;

typedef Variable<Index_t> Index_t_sim;
typedef VariablePtr<Index_t> Index_t_ptr_sim;
typedef VariablePtr<Index_t> Index_t_ptr_const_sim;
typedef VariablePtr<Index_t> const_Index_t_ptr_sim;
typedef VariablePtr<Index_t> const_Index_t_ptr_const_sim;
typedef VariablePtr<Index_t> Index_t_vec_sim;

typedef Variable<Int_t> Int_t_sim;
typedef VariablePtr<Int_t> Int_t_vec_sim;
#else
typedef Real_t Real_t_sim;
typedef Real_t* Real_t_ptr_sim;
typedef Real_t* const Real_t_ptr_const_sim;
typedef const Real_t* const_Real_t_ptr_sim;
typedef const Real_t* const const_Real_t_ptr_const_sim;
typedef std::vector<Real_t> Real_t_vec_sim;

typedef Index_t Index_t_sim;
typedef Index_t* Index_t_ptr_sim;
typedef Index_t* const Index_t_ptr_const_sim;
typedef const Index_t* const_Index_t_ptr_sim;
typedef const Index_t* const const_Index_t_ptr_const_sim;
typedef std::vector<Index_t> Index_t_vec_sim;

typedef Int_t Int_t_sim;
typedef std::vector<Int_t> Int_t_vec_sim;
#endif

enum { VolumeError = -1, QStopError = -2 } ;

inline real4  SQRT(real4  arg) { return sqrtf(arg) ; }
inline real8  SQRT(real8  arg) { return sqrt(arg) ; }
inline real10 SQRT(real10 arg) { return sqrtl(arg) ; }
#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
inline Variable<Real_t> SQRT(const Variable<Real_t> &arg) { return sqrt(arg) ; }
#endif

inline real4  CBRT(real4  arg) { return cbrtf(arg) ; }
inline real8  CBRT(real8  arg) { return cbrt(arg) ; }
inline real10 CBRT(real10 arg) { return cbrtl(arg) ; }
#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
inline Variable<Real_t> CBRT(const Variable<Real_t> &arg) { return cbrt(arg) ; }
#endif

inline real4  FABS(real4  arg) { return fabsf(arg) ; }
inline real8  FABS(real8  arg) { return fabs(arg) ; }
inline real10 FABS(real10 arg) { return fabsl(arg) ; }
#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
inline Variable<Real_t> FABS(const Variable<Real_t> &arg) { return fabs(arg) ; }
#endif


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
   Real_t_sim& x(Index_t_sim idx)    { return m_x[idx] ; }
   Real_t_sim& y(Index_t_sim idx)    { return m_y[idx] ; }
   Real_t_sim& z(Index_t_sim idx)    { return m_z[idx] ; }

   // Nodal velocities
   Real_t_sim& xd(Index_t_sim idx)   { return m_xd[idx] ; }
   Real_t_sim& yd(Index_t_sim idx)   { return m_yd[idx] ; }
   Real_t_sim& zd(Index_t_sim idx)   { return m_zd[idx] ; }

   // Nodal accelerations
   Real_t_sim& xdd(Index_t_sim idx)  { return m_xdd[idx] ; }
   Real_t_sim& ydd(Index_t_sim idx)  { return m_ydd[idx] ; }
   Real_t_sim& zdd(Index_t_sim idx)  { return m_zdd[idx] ; }

   // Nodal forces
   Real_t_sim& fx(Index_t_sim idx)   { return m_fx[idx] ; }
   Real_t_sim& fy(Index_t_sim idx)   { return m_fy[idx] ; }
   Real_t_sim& fz(Index_t_sim idx)   { return m_fz[idx] ; }

   // Nodal mass
   Real_t_sim& nodalMass(Index_t_sim idx) { return m_nodalMass[idx] ; }

   // Nodes on symmertry planes
   Index_t_sim symmX(Index_t_sim idx) { return m_symmX[idx] ; }
   Index_t_sim symmY(Index_t_sim idx) { return m_symmY[idx] ; }
   Index_t_sim symmZ(Index_t_sim idx) { return m_symmZ[idx] ; }
#if defined(LULESH_SST_MODS) && defined(LULESH_SST_SIM)
   bool symmXempty()          { return m_colLoc != 0; }
   bool symmYempty()          { return m_rowLoc != 0; }
   bool symmZempty()          { return m_planeLoc != 0; }
#else
   bool symmXempty()          { return m_symmX.empty(); }
   bool symmYempty()          { return m_symmY.empty(); }
   bool symmZempty()          { return m_symmZ.empty(); }
#endif

   //
   // Element-centered
   //
   Index_t&  regElemSize(Index_t idx) { return m_regElemSize[idx] ; }
   Index_t&  regNumList(Index_t idx) { return m_regNumList[idx] ; }
   Index_t*  regNumList()            { return &m_regNumList[0] ; }
   Index_t_ptr_sim  regElemlist(Int_t r)    { return m_regElemlist[r] ; }
   Index_t_sim&  regElemlist(Int_t r, Index_t_sim idx) { return m_regElemlist[r][idx] ; }

   Index_t_ptr_sim  nodelist(Index_t idx)    { return &m_nodelist[Index_t(8)*idx] ; }

   // elem connectivities through face
   Index_t_sim&  lxim(Index_t_sim idx) { return m_lxim[idx] ; }
   Index_t_sim&  lxip(Index_t_sim idx) { return m_lxip[idx] ; }
   Index_t_sim&  letam(Index_t_sim idx) { return m_letam[idx] ; }
   Index_t_sim&  letap(Index_t_sim idx) { return m_letap[idx] ; }
   Index_t_sim&  lzetam(Index_t_sim idx) { return m_lzetam[idx] ; }
   Index_t_sim&  lzetap(Index_t_sim idx) { return m_lzetap[idx] ; }

   // elem face symm/free-surface flag
   Int_t_sim&  elemBC(Index_t_sim idx) { return m_elemBC[idx] ; }

   // Principal strains - temporary
   Real_t_sim& dxx(Index_t_sim idx)  { return m_dxx[idx] ; }
   Real_t_sim& dyy(Index_t_sim idx)  { return m_dyy[idx] ; }
   Real_t_sim& dzz(Index_t_sim idx)  { return m_dzz[idx] ; }

   // Velocity gradient - temporary
   Real_t_sim& delv_xi(Index_t_sim idx)    { return m_delv_xi[idx] ; }
   Real_t_sim& delv_eta(Index_t_sim idx)   { return m_delv_eta[idx] ; }
   Real_t_sim& delv_zeta(Index_t_sim idx)  { return m_delv_zeta[idx] ; }

   // Position gradient - temporary
   Real_t_sim& delx_xi(Index_t_sim idx)    { return m_delx_xi[idx] ; }
   Real_t_sim& delx_eta(Index_t_sim idx)   { return m_delx_eta[idx] ; }
   Real_t_sim& delx_zeta(Index_t_sim idx)  { return m_delx_zeta[idx] ; }

   // Energy
   Real_t_sim& e(Index_t_sim idx)          { return m_e[idx] ; }

   // Pressure
   Real_t_sim& p(Index_t_sim idx)          { return m_p[idx] ; }

   // Artificial viscosity
   Real_t_sim& q(Index_t_sim idx)          { return m_q[idx] ; }

   // Linear term for q
   Real_t_sim& ql(Index_t_sim idx)         { return m_ql[idx] ; }
   // Quadratic term for q
   Real_t_sim& qq(Index_t_sim idx)         { return m_qq[idx] ; }

   // Relative volume
   Real_t_sim& v(Index_t_sim idx)          { return m_v[idx] ; }
   Real_t_sim& delv(Index_t_sim idx)       { return m_delv[idx] ; }

   // Reference volume
   Real_t_sim& volo(Index_t_sim idx)       { return m_volo[idx] ; }

   // volume derivative over volume
   Real_t_sim& vdov(Index_t_sim idx)       { return m_vdov[idx] ; }

   // Element characteristic length
   Real_t_sim& arealg(Index_t_sim idx)     { return m_arealg[idx] ; }

   // Sound speed
   Real_t_sim& ss(Index_t_sim idx)         { return m_ss[idx] ; }

   // Element mass
   Real_t_sim& elemMass(Index_t_sim idx)  { return m_elemMass[idx] ; }

   Index_t_sim nodeElemCount(Index_t_sim idx)
   { return m_nodeElemStart[idx+1] - m_nodeElemStart[idx] ; }

   Index_t_ptr_sim nodeElemCornerList(Index_t_sim idx)
   { return &m_nodeElemCornerList[m_nodeElemStart[idx]] ; }

   // Parameters 

   // Cutoffs
   Real_t_sim u_cut() const               { return m_u_cut ; }
   Real_t_sim e_cut() const               { return m_e_cut ; }
   Real_t_sim p_cut() const               { return m_p_cut ; }
   Real_t_sim q_cut() const               { return m_q_cut ; }
   Real_t_sim v_cut() const               { return m_v_cut ; }

   // Other constants (usually are settable via input file in real codes)
   Real_t_sim hgcoef() const              { return m_hgcoef ; }
   Real_t_sim qstop() const               { return m_qstop ; }
   Real_t_sim monoq_max_slope() const     { return m_monoq_max_slope ; }
   Real_t_sim monoq_limiter_mult() const  { return m_monoq_limiter_mult ; }
   Real_t_sim ss4o3() const               { return m_ss4o3 ; }
   Real_t_sim qlc_monoq() const           { return m_qlc_monoq ; }
   Real_t_sim qqc_monoq() const           { return m_qqc_monoq ; }
   Real_t_sim qqc() const                 { return m_qqc ; }

   Real_t_sim eosvmax() const             { return m_eosvmax ; }
   Real_t_sim eosvmin() const             { return m_eosvmin ; }
   Real_t_sim pmin() const                { return m_pmin ; }
   Real_t_sim emin() const                { return m_emin ; }
   Real_t_sim dvovmax() const             { return m_dvovmax ; }
   Real_t_sim refdens() const             { return m_refdens ; }

   // Timestep controls, etc...
   Real_t_sim& time()                 { return m_time ; }
   Real_t_sim& deltatime()            { return m_deltatime ; }
   Real_t_sim& deltatimemultlb()      { return m_deltatimemultlb ; }
   Real_t_sim& deltatimemultub()      { return m_deltatimemultub ; }
   Real_t_sim& stoptime()             { return m_stoptime ; }
   Real_t_sim& dtcourant()            { return m_dtcourant ; }
   Real_t_sim& dthydro()              { return m_dthydro ; }
   Real_t_sim& dtmax()                { return m_dtmax ; }
   Real_t_sim& dtfixed()              { return m_dtfixed ; }

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
   Real_t_ptr_sim commDataSend ;
   Real_t_ptr_sim commDataRecv ;
   
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
   Real_t_vec_sim m_x ;  /* coordinates */
   Real_t_vec_sim m_y ;
   Real_t_vec_sim m_z ;

   Real_t_vec_sim m_xd ; /* velocities */
   Real_t_vec_sim m_yd ;
   Real_t_vec_sim m_zd ;

   Real_t_vec_sim m_xdd ; /* accelerations */
   Real_t_vec_sim m_ydd ;
   Real_t_vec_sim m_zdd ;

   Real_t_vec_sim m_fx ;  /* forces */
   Real_t_vec_sim m_fy ;
   Real_t_vec_sim m_fz ;

   Real_t_vec_sim m_nodalMass ;  /* mass */

   Index_t_vec_sim m_symmX ;  /* symmetry plane nodesets */
   Index_t_vec_sim m_symmY ;
   Index_t_vec_sim m_symmZ ;

   // Element-centered

   // Region information

   Int_t    m_numReg ;                                 // NLS: NEEDED FOR FLOPS
   Int_t    m_cost; //imbalance cost                   // NLS: NEEDED FOR FLOPS
   Index_t *m_regElemSize ;   // Size of region sets   // NLS: NEEDED FOR FLOPS
   // NLS: NEEDED TO COMPUTE m_regElemSize BUT COULD BE FREED AFTER THAT (UNLESS DUMP VTK)
   Index_t *m_regNumList ;    // Region number per domain element
   Index_t_ptr_sim *m_regElemlist ;  // region indexset 

   Index_t_vec_sim  m_nodelist ;     /* elemToNode connectivity */

   Index_t_vec_sim  m_lxim ;  /* element connectivity across each face */
   Index_t_vec_sim  m_lxip ;
   Index_t_vec_sim  m_letam ;
   Index_t_vec_sim  m_letap ;
   Index_t_vec_sim  m_lzetam ;
   Index_t_vec_sim  m_lzetap ;

   Int_t_vec_sim    m_elemBC ;  /* symmetry/free-surface flags for each elem face */

   Real_t_vec_sim m_dxx ;  /* principal strains -- temporary */
   Real_t_vec_sim m_dyy ;
   Real_t_vec_sim m_dzz ;

   Real_t_vec_sim m_delv_xi ;    /* velocity gradient -- temporary */
   Real_t_vec_sim m_delv_eta ;
   Real_t_vec_sim m_delv_zeta ;

   Real_t_vec_sim m_delx_xi ;    /* coordinate gradient -- temporary */
   Real_t_vec_sim m_delx_eta ;
   Real_t_vec_sim m_delx_zeta ;
   
   Real_t_vec_sim m_e ;   /* energy */

   Real_t_vec_sim m_p ;   /* pressure */
   Real_t_vec_sim m_q ;   /* q */
   Real_t_vec_sim m_ql ;  /* linear term for q */
   Real_t_vec_sim m_qq ;  /* quadratic term for q */

   Real_t_vec_sim m_v ;     /* relative volume */
   Real_t_vec_sim m_volo ;  /* reference volume */
   Real_t_vec_sim m_vnew ;  /* new relative volume -- temporary */
   Real_t_vec_sim m_delv ;  /* m_vnew - m_v */
   Real_t_vec_sim m_vdov ;  /* volume derivative over volume */

   Real_t_vec_sim m_arealg ;  /* characteristic length of an element */
   
   Real_t_vec_sim m_ss ;      /* "sound speed" */

   Real_t_vec_sim m_elemMass ;  /* mass */

   // Cutoffs (treat as constants)
   const Real_t_sim  m_e_cut ;             // energy tolerance 
   const Real_t_sim  m_p_cut ;             // pressure tolerance 
   const Real_t_sim  m_q_cut ;             // q tolerance 
   const Real_t_sim  m_v_cut ;             // relative volume tolerance 
   const Real_t_sim  m_u_cut ;             // velocity tolerance 

   // Other constants (usually setable, but hardcoded in this proxy app)

   const Real_t_sim  m_hgcoef ;            // hourglass control 
   const Real_t_sim  m_ss4o3 ;
   const Real_t_sim  m_qstop ;             // excessive q indicator 
   const Real_t_sim  m_monoq_max_slope ;
   const Real_t_sim  m_monoq_limiter_mult ;
   const Real_t_sim  m_qlc_monoq ;         // linear term coef for q 
   const Real_t_sim  m_qqc_monoq ;         // quadratic term coef for q 
   const Real_t_sim  m_qqc ;
   const Real_t_sim  m_eosvmax ;
   const Real_t_sim  m_eosvmin ;
   const Real_t_sim  m_pmin ;              // pressure floor 
   const Real_t_sim  m_emin ;              // energy floor 
   const Real_t_sim  m_dvovmax ;           // maximum allowable volume change 
   const Real_t_sim  m_refdens ;           // reference density 

   // Variables to keep track of timestep, simulation time, and cycle
   Real_t_sim  m_dtcourant ;         // courant constraint 
   Real_t_sim  m_dthydro ;           // volume change constraint 
   Int_t   m_cycle ;             // iteration count for simulation 
   Real_t_sim  m_dtfixed ;           // fixed time increment 
   Real_t_sim  m_time ;              // current time 
   Real_t_sim  m_deltatime ;         // variable time increment 
   Real_t_sim  m_deltatimemultlb ;
   Real_t_sim  m_deltatimemultub ;
   Real_t_sim  m_dtmax ;             // maximum allowable time increment 
   Real_t_sim  m_stoptime ;          // end time for simulation 


   Int_t   m_numRanks ;          // NLS: NEEDED FOR COMM PATTERN

   Index_t m_colLoc ;            // NLS: NEEDED FOR COMM PATTERN
   Index_t m_rowLoc ;            // NLS: NEEDED FOR COMM PATTERN
   Index_t m_planeLoc ;          // NLS: NEEDED FOR COMM PATTERN
   Index_t m_tp ;                // NLS: NEEDED FOR COMM PATTERN

   Index_t m_sizeX ;             // NLS: NEEDED FOR COMM PATTERN
   Index_t m_sizeY ;             // NLS: NEEDED FOR COMM PATTERN
   Index_t m_sizeZ ;             // NLS: NEEDED FOR COMM PATTERN
   Index_t m_numElem ;           // NLS: NEEDED FOR COMM PATTERN AND FLOPS
   Index_t m_numNode ;           // NLS: NEEDED FOR FLOPS

   Index_t m_maxPlaneSize ;      // NLS: NEEDED FOR COMM PATTERN
   Index_t m_maxEdgeSize ;       // NLS: NEEDED FOR COMM PATTERN

   // OMP hack 
   Index_t_ptr_sim m_nodeElemStart ;
   Index_t_ptr_sim m_nodeElemCornerList ;

   // Used in setup
   Index_t m_rowMin, m_rowMax;
   Index_t m_colMin, m_colMax;
   Index_t m_planeMin, m_planeMax ;

} ;

typedef Real_t_sim &(Domain::* Domain_member )(Index_t_sim) ;

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

// lulesh-par
Real_t_sim CalcElemVolume( const Real_t_sim x[8],
                       const Real_t_sim y[8],
                       const Real_t_sim z[8]);

// lulesh-util
void ParseCommandLineOptions(int argc, char *argv[],
                             Int_t myRank, struct cmdLineOpts *opts);
void VerifyAndWriteFinalOutput(Real_t_sim elapsed_time,
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
