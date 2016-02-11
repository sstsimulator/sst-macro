//@header
// ************************************************************************
//
//      ministencil: stencil computations with boundary exchange.
//              Copyright (2011) sandia corporation
//
// Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
// license for use of this work by or on behalf of the U.S. Government.
//
// This library is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 2.1 of the
// License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
//                    Michael A. Heroux (maherou@sandia.gov)
//
// ************************************************************************
//@header

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <sstmac/common/c_params.h>
#include <mpi.h>
#ifdef USING_TPM
#include "tpm_c.h"
#endif

#define SCALING_STRONG 1     // Ensure these values match the Fortran settings!
#define SCALING_WEAK   2

typedef struct
{
  int scaling, nx, ny, nz, num_vars, percent_sum, num_spikes, num_tsteps,
      stencil, comm_method, send_protocol, check_diffusion, npx, npy, npz,
      profiling, checkpoint_interval;
  char checkpoint_file[1024];
} Input_params; // Problem parameters, passed to Fortran DRIVER.

// Prototypes

int
check_input(Input_params *param, int mype, int numpes);
void
print_help_message();

#if defined _F2C_UPPER_CASE
#define MINI_GHOST MINI_GHOST
#elif defined _F2C___
#define MINI_GHOST mini_ghost__
#elif defined _F2C_NO_
#define MINI_GHOST mini_ghost
#else
#define MINI_GHOST mini_ghost_
#endif

extern void MINI_GHOST(int *scaling, int *nx, int *ny, int *nz, int *num_vars,
    int *percent_sum, int *num_spikes, int *num_tsteps, int *stencil,
    int *comm_method, int *send_protocol, int *check_diffusion, int *npx,
    int *npy, int *npz, int *profiling, int *checkpoint_interval,
    char *checkpoint_file);

int
main(int argc, char** argv)
{

  // Purpose
  // =======
  // Parse command line input and call Fortran MINI_GHOST.

  //  =====================================================================

  // --------------
  //  Local Scalars
  // --------------

  int i, ierr = 0, num_args, remainder, root_pe = 0, tmp_nx, tmp_ny, tmp_nz;

  int mype, numpes;

  Input_params param;

  // ------------
  // Local Arrays
  // ------------

#define count_problem_params 17

  int problem_params[count_problem_params];

  // ---------------------
  // Executable Statements
  // ---------------------

  // Set defaults: Small problem run on single process.

#include "default-settings.h"

#ifdef USING_TPM
  //ierr = MPI_Init(NULL, NULL);
  TPM_Init(get_bool_param("use_topo"));
  mype = TPM_Comm_rank();
  numpes = TPM_Comm_size();

#else
  ierr = MPI_Init(NULL, NULL);
  ierr = MPI_Comm_rank(MPI_COMM_WORLD, &mype);
  ierr = MPI_Comm_size(MPI_COMM_WORLD, &numpes);
#endif

  ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_ARE_FATAL);

  // Set defaults: Weak scaling of BSPMA, 3d7pt stencil, error checking and profiling on.

  param.scaling = get_optional_int_param("minighost_scaling", 2);
  param.nx = get_optional_int_param("minighost_nx", 10);
  param.ny = get_optional_int_param("minighost_ny", 10);
  param.nz = get_optional_int_param("minighost_nz", 10);
  param.num_vars = get_optional_int_param("minighost_numvars", 5);
  param.percent_sum = get_optional_int_param("minighost_percent_sum", 0);
  param.num_spikes = get_optional_int_param("minighost_num_spikes", 1);
  param.num_tsteps = get_optional_int_param("minighost_num_tsteps", 10);
  param.stencil = get_optional_int_param("minighost_stencil", 14);
  param.comm_method = get_optional_int_param("minighost_comm_method", 1);
  param.send_protocol = get_optional_int_param("minighost_send_protocol", 22);
  param.check_diffusion
      = get_optional_int_param("minighost_check_diffusion", 0);
  param.npx = get_optional_int_param("minighost_npx", numpes);
  param.npy = get_optional_int_param("minighost_npy", 1);
  param.npz = get_optional_int_param("minighost_npz", 1);
  param.profiling = get_optional_int_param("minighost_profiling", 1);
  param.checkpoint_interval = get_optional_int_param(
      "minighost_checkpoint_interval", 0);
  param.checkpoint_file[0] = '\0';

  tmp_nx = param.nx;
  tmp_ny = param.ny;
  tmp_nz = param.nz;

  switch (param.scaling)
    {
  case (SCALING_WEAK):

    param.nx = tmp_nx;
    param.ny = tmp_ny;
    param.nz = tmp_nz;

    break;

  case (SCALING_STRONG):

    param.nx = tmp_nx / param.npx;
    remainder = tmp_nx % param.npx;
    if (mype < remainder)
      param.nx++;

    param.ny = tmp_ny / param.npy;
    remainder = tmp_ny % param.npy;
    if (mype < remainder)
      param.ny++;

    param.nz = tmp_nz / param.npz;
    remainder = tmp_nz % param.npz;
    if (mype < remainder)
      param.nz++;

    break;

  default:

    fprintf(
        stderr,
        "\n\n ** Error ** Unknown scaling %d; options are weak (%d) and strong (%d). \n",
        param.scaling, SCALING_WEAK, SCALING_STRONG);
    MPI_Abort(MPI_COMM_WORLD, -1);

    } // End switch  ( param.scaling )

  if (ierr != 0)
    {
      fprintf(stderr, "[pe %d] ** Error ** illegal input parameter(s).", mype);
      MPI_Abort(MPI_COMM_WORLD, -1);
    }

  ierr
      = MPI_Bcast(NULL, count_problem_params, MPI_INT, root_pe, MPI_COMM_WORLD);
  assert(ierr == MPI_SUCCESS);
  ierr = MPI_Bcast(NULL, 1024, MPI_CHAR, root_pe, MPI_COMM_WORLD);
  assert(ierr == MPI_SUCCESS);

  ierr = check_input(&param, mype, numpes);
  if (ierr != 0)
    {
      fprintf(stderr, "[pe %d] ** Error ** illegal input parameter(s).", mype);
      MPI_Abort(MPI_COMM_WORLD, -1);
    }

  MINI_GHOST(&param.scaling, &param.nx, &param.ny, &param.nz, &param.num_vars,
      &param.percent_sum, &param.num_spikes, &param.num_tsteps, &param.stencil,
      &param.comm_method, &param.send_protocol, &param.check_diffusion,
      &param.npx, &param.npy, &param.npz, &param.profiling,
      &param.checkpoint_interval, &(param.checkpoint_file[0]));

  MPI_Finalize();

} // End main.

// ======================================== Utilities ======================================

void
print_help_message()
{
  fprintf(stderr, "\n\n (Optional) command line input is of the form: \n\n");

  fprintf(stderr, "--scaling \n");
  fprintf(stderr, "--nx \n");
  fprintf(stderr, "--ny \n");
  fprintf(stderr, "--nz \n");
  fprintf(stderr, "--num_vars \n");
  fprintf(stderr, "--percent_sum \n");
  fprintf(stderr, "--num_spikes \n");
  fprintf(stderr, "--num_tsteps \n");
  fprintf(stderr, "--stencil \n");
  fprintf(stderr, "--comm_method \n");
  fprintf(stderr, "--send_protocol \n");
  fprintf(stderr, "--check_diffusion \n");
  fprintf(stderr, "--npx \n");
  fprintf(stderr, "--npy \n");
  fprintf(stderr, "--npz \n");
  fprintf(stderr, "--profiling \n");
  fprintf(stderr, "--checkpoint_interval \n");
  fprintf(stderr, "--checkpoint_file \n\n");

  fprintf(
      stderr,
      "All associated settings are integers except --checkpoint_file. See MG_OPTIONS.F for listing of options.\n");

  return;
}

int
check_input(Input_params *param, int mype, int numpes)
{
  int num_input_err = 0;
  if (param->nx <= 0)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: nx %d <= 0. \n", mype,
          param->nx);
    }
  if (param->ny <= 0)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: ny %d <= 0. \n", mype,
          param->ny);
    }
  if (param->nz <= 0)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: nz %d <= 0. \n", mype,
          param->nz);
    }
  if (param->num_vars <= 0)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: num_vars %d <= 0. \n", mype,
          param->num_vars);
    }
  if (param->num_tsteps < 1)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: num_tsteps %d < 1. \n", mype,
          param->num_tsteps);
    }
  if (param->npx < 1)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: npx %d < 1. \n", mype,
          param->npx);
    }
  if (param->npy < 1)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: npy %d < 1. \n", mype,
          param->npy);
    }
  if (param->npz < 1)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: npz %d < 1. \n", mype,
          param->npz);
    }
  if (param->npx * param->npy * param->npz != numpes)
    {
      num_input_err++;
      fprintf(
          stderr,
          "[pe %d] ** Input error**: npx*npy*npz not equal to numpes; (npx, npy, npz) = (%d, %d, %d).\n",
          mype, param->npx, param->npy, param->npz);
    }
  if (param->percent_sum < 0)
    {
      num_input_err++;
      fprintf(stderr, "[pe %d] ** Input error**: percent_sum %d < 0.\n", mype,
          param->percent_sum);
    }
  if (param->percent_sum > 100)
    {
      num_input_err++;
      fprintf(stderr,
          "[pe %d] ** Input error**: percent_sum > num_vars = %d.\n", mype,
          param->percent_sum);
    }
  if (param->checkpoint_interval < 0)
    {
      num_input_err++;
      fprintf(stderr,
          "[pe %d] ** Input error**: checkpoint_interval %d < 0.\n", mype,
          param->checkpoint_interval);
    }

  return (num_input_err);

} // End check_input.

// End main.c
