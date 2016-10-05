/** \file
 * Hello world example app.
 * A very simple mpi skeleton application.
 */
#include <sstmac/sstmacro.h>
#include <sstmac/skeleton.h>
#include <sstmac/software/process/app.h>


/// A hello world example mpi skeleton application.
namespace luleshmodel {
 
  /** luleshapp is an example mpi skeleton application.  It inherits mpi 
    * support [things like mpiapi()] from apiapp.  apiapp is derived from
    * the thread class, so this will be a runnable task thread [it must
    * implement a run() method].
    */

  class luleshapp : public sstmac::sw::app {

  protected:

    MPI_Request reqs[52];

  double SPTL_INTEGRATE_STRESS_ELEMS;
  double SPTL_CALC_HOURGLASS_ELEMS;
  double SPTL_CALC_ACCEL_NODES;
  double SPTL_CALC_VEL_NODES;
  double SPTL_CALC_POSITION_NODES;
  double SPTL_CALC_KINEMATICS_ELEM;
  double SPTL_CALC_MONOTONIC_Q_GRADIENT_ELEM;
  double SPTL_CALC_MONOTONIC_Q_REGION_ELEM;
  double SPTL_APPLY_MATERIAL_PROP_ELEM;
  double SPTL_EVAL_EOS_COMPRESS;
  double SPTL_CALC_PRESSURE_ELEM;
  double SPTL_CALC_SOUND_SPEED_ELEM;
  double SPTL_UPDATE_VOL_ELEM;
  double SPTL_CALC_LAGRANGE_ELEMENTS;
  double SPTL_CALC_FB_HOURGLASS_ELEM;
  double SPTL_CALC_COURANT_ELEM;
  double SPTL_CALC_HYDRO_CONSTRAINT_ELEM;
  double SPTL_CALC_ENERGY_FOR_ELEMS;

  MPI_Comm world_;
  bool usetopo_;

  public:
    luleshapp(sprockit::sim_parameters* params, sstmac::sw::software_id sid);

    /** Destructor. */
    virtual ~luleshapp() throw() {};

    /** The main routine for the skeleton application. */
    void skeleton_main();

    void consume_params(sprockit::sim_parameters *params);

    /** LULESH application entities */
  void comm_recv(int numElem, int numNodes, int px, int nx);
  void comm_send(int numElem, int numNodes, int px, int nx);
  void LagrangeLeapFrog(int numElem, int numNodes, int px, int nx);
  void LagrangeNodal(int numElem, int numNodes, int px, int nx);
  void CalcForceForNodes(int numElem, int numNodes, int px, int nx);
  void CalcVolumeForceForElems(int numElem, int numNodes, int px, int nx);
  void InitStressForElems(int numElem, int numNodes, int px, int nx);
  void IntegrateStressForElems(int numElem, int numNodesm, int px, int nx);
  void CalcHourglassControlForElems(int numElem, int numNodes, int px, int nx);
  void CalcFBHourglassForceForElems(int numElem, int numNodes, int px, int nx);
  void CalcAccelerationForNodes(int numElem, int numNodes, int px, int nx);
  void ApplyAccelerationBoundaryConditionsForNodes(int numElem, int numNodes, int px, int nx);
  void CalcVelocityForNodes(int numElem, int numNodes, int px, int nx);
  void CalcPositionForNodes(int numElem, int numNodes, int px, int nx);
  void CalcKinematicsForElems(int numElem, int numNodes, int px, int nx);
  void CalcLagrangeElements(int numElem, int numNodes, int px, int nx);
  void LagrangeElements(int numElem, int numNodes, int px, int nx);
  void UpdateVolumesForElems(int numElem, int numNodes, int px, int nx);
  void ApplyMaterialPropertiesForElems(int numElem, int numNodes, int px, int nx);
  void EvalEOSForElems(int numElem, int numNodes, int px, int nx);
  void CalcSoundSpeedForElems(int numElem, int numNodes, int px, int nx);
  void CalcEnergyForElems(int numElem, int numNodes, int px, int nx);
  void CalcPressureForElems(int numElem, int numNodes, int px, int nx);
  void CalcQForElems(int numElem, int numNodes, int px, int nx);
  void CalcMonotonicQForElems(int numElem, int numNodes, int px, int nx);
  void CalcMonotonicQRegionForElems(int numElem, int numNodes, int px, int nx);
  void CalcMonotonicQGradientsForElems(int numElem, int numNodes, int px, int nx);
  void CalcTimeConstraintsForElems(int numElem, int numNodes, int px, int nx);
  void CalcCourantConstraintForElems(int numElem, int numNodes, int px, int nx);
  void CalcHydroConstraintForElems(int numElem, int numNodes, int px, int nx);
  void CommMonoQ(int numElem, int numNodes, int px, int nx);

  void TimeIncrement(int numElem, int numNodes, int px, int nx);
  void application_main(int iter_max, int numElem, int numNodes, int px, int nx);

  };

} // end of namespace luleshmodel

