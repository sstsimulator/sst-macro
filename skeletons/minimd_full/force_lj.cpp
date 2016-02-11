/* ----------------------------------------------------------------------
   miniMD is a simple, parallel molecular dynamics (MD) code.   miniMD is
   an MD microapplication in the Mantevo project at Sandia National
   Laboratories ( http://www.mantevo.org ). The primary
   authors of miniMD are Steve Plimpton (sjplimp@sandia.gov) , Paul Crozier
   (pscrozi@sandia.gov) and Christian Trott (crtrott@sandia.gov).

   Copyright (2008) Sandia Corporation.  Under the terms of Contract
   DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government retains
   certain rights in this software.  This library is free software; you
   can redistribute it and/or modify it under the terms of the GNU Lesser
   General Public License as published by the Free Software Foundation;
   either version 3 of the License, or (at your option) any later
   version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this software; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.  See also: http://www.gnu.org/licenses/lgpl.txt .

   For questions, contact Paul S. Crozier (pscrozi@sandia.gov) or
   Christian Trott (crtrott@sandia.gov).

   Please read the accompanying README and LICENSE files.
---------------------------------------------------------------------- */

#include "stdio.h"
#include "math.h"
#include "force_lj.h"
#include "openmp.h"

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
#include "lwperf.h"
#endif

#ifdef SSTMAC
#include <sstmac/compute.h>
#endif

#ifndef VECTORLENGTH
#define VECTORLENGTH 8
#endif
ForceLJ::ForceLJ()
{
  cutforce = 0.0;
  cutforcesq = 0.0;
  use_oldcompute = 0;
  reneigh = 1;
  style = FORCELJ;
}
ForceLJ::~ForceLJ() {}

void ForceLJ::setup()
{
  cutforcesq = cutforce * cutforce;
}


void ForceLJ::compute(Atom &atom, Neighbor &neighbor, Comm &comm, int me)
{
  eng_vdwl = 0;
  virial = 0;

  int my_nall = atom.nlocal + atom.nghost;
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOG(force_compute,ID(neighbor.halfneigh),ID(neighbor.ghost_newton),IN(my_nall),IN(atom.nlocal),DN(cutforcesq));
#endif
  if(evflag) {
    if(use_oldcompute)
      return compute_original<1>(atom, neighbor, me);

    if(neighbor.halfneigh) {
      if(neighbor.ghost_newton) {
        if(threads->omp_num_threads > 1)
          compute_halfneigh_threaded<1, 1>(atom, neighbor, me);
        else
          compute_halfneigh<1, 1>(atom, neighbor, me);
      } else {
        if(threads->omp_num_threads > 1)
          compute_halfneigh_threaded<1, 0>(atom, neighbor, me);
        else
          compute_halfneigh<1, 0>(atom, neighbor, me);
      }
    } else compute_fullneigh<1>(atom, neighbor, me);
  } else {
    if(use_oldcompute)
      compute_original<0>(atom, neighbor, me);

    if(neighbor.halfneigh) {
      if(neighbor.ghost_newton) {
        if(threads->omp_num_threads > 1)
          compute_halfneigh_threaded<0, 1>(atom, neighbor, me);
        else
          compute_halfneigh<0, 1>(atom, neighbor, me);
      } else {
        if(threads->omp_num_threads > 1)
          compute_halfneigh_threaded<0, 0>(atom, neighbor, me);
        else
          compute_halfneigh<0, 0>(atom, neighbor, me);
      }
    } else compute_fullneigh<0>(atom, neighbor, me);

  }
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOP(force_compute,ID(neighbor.halfneigh),ID(neighbor.ghost_newton),IN(my_nall),IN(atom.nlocal),DN(cutforcesq));
#endif
}

//original version of force compute in miniMD
//  -MPI only
//  -not vectorizable
template<int EVFLAG>
void ForceLJ::compute_original(Atom &atom, Neighbor &neighbor, int me)
{
  int i, j, k, nlocal, nall, numneigh;
  MMD_float xtmp, ytmp, ztmp, delx, dely, delz, rsq;
  MMD_float sr2, sr6, force;
  int* neighs;
  MMD_float** x, **f;

  nlocal = atom.nlocal;
  nall = atom.nlocal + atom.nghost;
  x = atom.x;
  f = atom.f;

  eng_vdwl = 0;
  virial = 0;
  // clear force on own and ghost atoms

  for(i = 0; i < nall; i++) {
    f[i][0] = 0.0;
    f[i][1] = 0.0;
    f[i][2] = 0.0;
  }

  // loop over all neighbors of my atoms
  // store force on both atoms i and j

  for(i = 0; i < nlocal; i++) {
    neighs = &neighbor.neighbors[i * neighbor.maxneighs];
    numneigh = neighbor.numneigh[i];
    xtmp = x[i][0];
    ytmp = x[i][1];
    ztmp = x[i][2];

    for(k = 0; k < numneigh; k++) {
      j = neighs[k];
      delx = xtmp - x[j][0];
      dely = ytmp - x[j][1];
      delz = ztmp - x[j][2];
      rsq = delx * delx + dely * dely + delz * delz;

      if(rsq < cutforcesq) {
        sr2 = 1.0 / rsq;
        sr6 = sr2 * sr2 * sr2;
        force = 48.0 * sr6 * (sr6 - 0.5) * sr2;
        f[i][0] += delx * force;
        f[i][1] += dely * force;
        f[i][2] += delz * force;
        f[j][0] -= delx * force;
        f[j][1] -= dely * force;
        f[j][2] -= delz * force;

        if(EVFLAG) {
          eng_vdwl += (4.0 * sr6 * (sr6 - 1.0));
          virial += (delx * delx + dely * dely + delz * delz) * force;
        }
      }
    }
  }
}


//optimised version of compute
//  -MPI only
//  -use temporary variable for summing up fi
//  -enables vectorization by:
//     -getting rid of 2d pointers
//     -use pragma simd to force vectorization of inner loop
template<int EVFLAG, int GHOST_NEWTON>
void ForceLJ::compute_halfneigh(Atom &atom, Neighbor &neighbor, int me)
{
  int* neighs;
  int tid = omp_get_thread_num();

  const int nlocal = atom.nlocal;
  const int nall = atom.nlocal + atom.nghost;
  MMD_float* x = &atom.x[0][0];
  MMD_float* f = &atom.f[0][0];

#ifndef _USE_LOOP_MODEL
  // clear force on own and ghost atoms
  for(int i = 0; i < nall; i++) {
    f[3 * i + 0] = 0.0;
    f[3 * i + 1] = 0.0;
    f[3 * i + 2] = 0.0;
  }
#endif
#ifdef SSTMAC
  SSTMAC_compute_loop(0,nall,3);
#endif

#ifndef _USE_LOOP_MODEL
  // loop over all neighbors of my atoms
  // store force on both atoms i and j
  MMD_float t_energy = 0;
  MMD_float t_virial = 0;
  for(int i = 0; i < nlocal; i++) {
    neighs = &neighbor.neighbors[i * neighbor.maxneighs];
    const int numneighs = neighbor.numneigh[i];
    const MMD_float xtmp = x[3 * i + 0];
    const MMD_float ytmp = x[3 * i + 1];
    const MMD_float ztmp = x[3 * i + 2];

    MMD_float fix = 0.0;
    MMD_float fiy = 0.0;
    MMD_float fiz = 0.0;

    //pragma simd forces vectorization (ignoring the performance objections of the compiler)
    //also give hint to use certain vectorlength for MIC and Sandy Bridge this should be 8, for Westmere 4
    //give hint to compiler that fix, fiy and fiz are used for reduction only


    //causes errors on MIC with VECTORLENGTH=4
#pragma simd vectorlength (VECTORLENGTH) reduction (+: fix,fiy,fiz,t_energy,t_virial)

    for(int k = 0; k < numneighs; k++) {
      const int j = neighs[k];
      const MMD_float delx = xtmp - x[3 * j + 0];
      const MMD_float dely = ytmp - x[3 * j + 1];
      const MMD_float delz = ztmp - x[3 * j + 2];
      const MMD_float rsq = delx * delx + dely * dely + delz * delz;

      if(rsq < cutforcesq) {
        const MMD_float sr2 = 1.0 / rsq;
        const MMD_float sr6 = sr2 * sr2 * sr2;
        const MMD_float force = 48.0 * sr6 * (sr6 - 0.5) * sr2;

        fix += delx * force;
        fiy += dely * force;
        fiz += delz * force;

        if(GHOST_NEWTON || j < nlocal) {
          f[3 * j + 0] -= delx * force;
          f[3 * j + 1] -= dely * force;
          f[3 * j + 2] -= delz * force;
        }

        if(EVFLAG) {
          const MMD_float scale = (GHOST_NEWTON || j < nlocal) ? 1.0 : 0.5;
          t_energy += scale * (4.0 * sr6 * (sr6 - 1.0));
          t_virial += scale * (delx * delx + dely * dely + delz * delz) * force;
        }

      }
    }

    f[3 * i + 0] += fix;
    f[3 * i + 1] += fiy;
    f[3 * i + 2] += fiz;
  }
#endif

#ifdef SSTMAC
  /* This number, 40, represents the average number of neighbors. The precise 
   * value was taken from previous versions of this skeleton, which took that 
   * value in as a parameter.
   */
  SSTMAC_compute_loop2(0,nlocal,0,40,6);
#endif
 
#ifndef _USE_LOOP_MODEL
  eng_vdwl += t_energy;
  virial += t_virial;
#endif

}

//optimised version of compute
//  -MPI + OpenMP (atomics for fj update)
//  -use temporary variable for summing up fi
//  -enables vectorization by:
//    -getting rid of 2d pointers
//    -use pragma simd to force vectorization of inner loop (not currently supported due to OpenMP atomics
template<int EVFLAG, int GHOST_NEWTON>
void ForceLJ::compute_halfneigh_threaded(Atom &atom, Neighbor &neighbor, int me)
{
  int nlocal, nall;
  int* neighs;
  MMD_float* x, *f;
  int tid = omp_get_thread_num();

  MMD_float t_eng_vdwl = 0;
  MMD_float t_virial = 0;

  nlocal = atom.nlocal;
  nall = atom.nlocal + atom.nghost;
  x = &atom.x[0][0];
  f = &atom.f[0][0];

  // clear force on own and ghost atoms
  #pragma omp barrier


  #pragma omp for schedule(static,CHUNKSIZE)

  for(int i = 0; i < nall; i++) {
    f[3 * i + 0] = 0.0;
    f[3 * i + 1] = 0.0;
    f[3 * i + 2] = 0.0;
  }

  #pragma omp barrier
  // loop over all neighbors of my atoms
  // store force on both atoms i and j

  #pragma omp for schedule(static,CHUNKSIZE)

  for(int i = 0; i < nlocal; i++) {
    neighs = &neighbor.neighbors[i * neighbor.maxneighs];
    const int numneighs = neighbor.numneigh[i];
    const MMD_float xtmp = x[3 * i + 0];
    const MMD_float ytmp = x[3 * i + 1];
    const MMD_float ztmp = x[3 * i + 2];
    MMD_float fix = 0.0;
    MMD_float fiy = 0.0;
    MMD_float fiz = 0.0;

    //pragma simd forces vectorization (ignoring the performance objections of the compiler)
    //also give hint to use certain vectorlength for MIC and Sandy Bridge this should be 8, for Westmere 4
    //give hint to compiler that fix, fiy and fiz are used for reduction only
#pragma simd vectorlength (VECTORLENGTH) reduction (+: fix,fiy,fiz)

    for(int k = 0; k < numneighs; k++) {
      const int j = neighs[k];
      const MMD_float delx = xtmp - x[3 * j + 0];
      const MMD_float dely = ytmp - x[3 * j + 1];
      const MMD_float delz = ztmp - x[3 * j + 2];
      const MMD_float rsq = delx * delx + dely * dely + delz * delz;

      if(rsq < cutforcesq) {
        const MMD_float sr2 = 1.0 / rsq;
        const MMD_float sr6 = sr2 * sr2 * sr2;
        const MMD_float force = 48.0 * sr6 * (sr6 - 0.5) * sr2;

        fix += delx * force;
        fiy += dely * force;
        fiz += delz * force;

        if(GHOST_NEWTON || j < nlocal) {
          #pragma omp atomic
          f[3 * j + 0] -= delx * force;
          #pragma omp atomic
          f[3 * j + 1] -= dely * force;
          #pragma omp atomic
          f[3 * j + 2] -= delz * force;
        }

        if(EVFLAG) {
          const MMD_float scale = (GHOST_NEWTON || j < nlocal) ? 1.0 : 0.5;
          t_eng_vdwl += scale * (4.0 * sr6 * (sr6 - 1.0));
          t_virial += scale * (delx * delx + dely * dely + delz * delz) * force;
        }
      }
    }

    #pragma omp atomic
    f[3 * i + 0] += fix;
    #pragma omp atomic
    f[3 * i + 1] += fiy;
    #pragma omp atomic
    f[3 * i + 2] += fiz;
  }

  #pragma omp atomic
  eng_vdwl += t_eng_vdwl;
  #pragma omp atomic
  virial += t_virial;

  #pragma omp barrier
}

//optimised version of compute
//  -MPI + OpenMP (using full neighborlists)
//  -gets rid of fj update (read/write to memory)
//  -use temporary variable for summing up fi
//  -enables vectorization by:
//    -get rid of 2d pointers
//    -use pragma simd to force vectorization of inner loop
template<int EVFLAG>
void ForceLJ::compute_fullneigh(Atom &atom, Neighbor &neighbor, int me)
{
  int nlocal, nall;
  int* neighs;
  MMD_float* x, *f;
  int tid = omp_get_thread_num();

  MMD_float t_eng_vdwl = 0;
  MMD_float t_virial = 0;
  nlocal = atom.nlocal;
  nall = atom.nlocal + atom.nghost;
  x = &atom.x[0][0];
  f = &atom.f[0][0];

  // clear force on own and ghost atoms
  #pragma omp barrier

  #pragma omp for schedule(static,CHUNKSIZE)

  for(int i = 0; i < nlocal; i++) {
    f[3 * i + 0] = 0.0;
    f[3 * i + 1] = 0.0;
    f[3 * i + 2] = 0.0;
  }

  #pragma omp barrier
  // loop over all neighbors of my atoms
  // store force on both atoms i and j

  #pragma omp for schedule(static,CHUNKSIZE)

  for(int i = 0; i < nlocal; i++) {
    neighs = &neighbor.neighbors[i * neighbor.maxneighs];
    const int numneighs = neighbor.numneigh[i];
    const MMD_float xtmp = x[3 * i + 0];
    const MMD_float ytmp = x[3 * i + 1];
    const MMD_float ztmp = x[3 * i + 2];
    MMD_float fix = 0;
    MMD_float fiy = 0;
    MMD_float fiz = 0;

    //pragma simd forces vectorization (ignoring the performance objections of the compiler)
    //also give hint to use certain vectorlength for MIC, Sandy Bridge and WESTMERE this should be be 8 here
    //give hint to compiler that fix, fiy and fiz are used for reduction only
#pragma simd vectorlength (VECTORLENGTH) reduction (+: fix,fiy,fiz)

    for(int k = 0; k < numneighs; k++) {
      const int j = neighs[k];
      const MMD_float delx = xtmp - x[3 * j + 0];
      const MMD_float dely = ytmp - x[3 * j + 1];
      const MMD_float delz = ztmp - x[3 * j + 2];
      const MMD_float rsq = delx * delx + dely * dely + delz * delz;

      if(rsq < cutforcesq) {
        const MMD_float sr2 = 1.0 / rsq;
        const MMD_float sr6 = sr2 * sr2 * sr2;
        const MMD_float force = 48.0 * sr6 * (sr6 - 0.5) * sr2;
        fix += delx * force;
        fiy += dely * force;
        fiz += delz * force;

        if(EVFLAG) {
          t_eng_vdwl += sr6 * (sr6 - 1.0);
          t_virial += delx * delx * force + dely * dely * force + delz * delz * force;
        }
      }
    }

    f[i * 3 + 0] += fix;
    f[i * 3 + 1] += fiy;
    f[i * 3 + 2] += fiz;

  }

  t_eng_vdwl *= 4.0;
  t_virial *= 0.5;

  #pragma omp atomic
  eng_vdwl += t_eng_vdwl;
  #pragma omp atomic
  virial += t_virial;
  #pragma omp barrier
}


