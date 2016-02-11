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
#include "stdlib.h"

#include "neighbor.h"
#include "openmp.h"

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
#include "lwperf.h"
#endif

#ifdef SSTMAC
#include <sstmac/compute.h>
#endif

#define FACTOR 0.999
#define SMALL 1.0e-6

Neighbor::Neighbor()
{
  ncalls = 0;
  max_totalneigh = 0;
  numneigh = NULL;
  neighbors = NULL;
  maxneighs = 100;
  nmax = 0;
  bincount = NULL;
  bins = NULL;
  atoms_per_bin = 8;
  stencil = NULL;
  threads = NULL;
  halfneigh = 0;
  ghost_newton = 1;
}

Neighbor::~Neighbor()
{
  if(numneigh) free(numneigh);

  if(neighbors) free(neighbors);

  if(bincount) free(bincount);

  if(bins) free(bins);
}

/* binned neighbor list construction with full Newton's 3rd law
   every pair stored exactly once by some processor
   each owned atom i checks its own bin and other bins in Newton stencil */

void Neighbor::build(Atom &atom)
{
  ncalls++;
  const int nlocal = atom.nlocal;
  const int nall = atom.nlocal + atom.nghost;
  /* extend atom arrays if necessary */

  #pragma omp master

  if(nall > nmax) {
    nmax = nall;

    if(numneigh) free(numneigh);

    if(neighbors) free(neighbors);

    numneigh = (int*) malloc(nmax * sizeof(int));
    neighbors = (int*) malloc(nmax * maxneighs * sizeof(int*));
  }

  int omp_me = omp_get_thread_num();
  int num_omp_threads = threads->omp_num_threads;
  int master = -1;

  #pragma omp master
  master = omp_me;

  #pragma omp barrier
  /* bin local & ghost atoms */

  binatoms(atom);
  count = 0;
  /* loop over each atom, storing neighbors */

  const MMD_float* x = &atom.x[0][0];

  resize = 1;
  #pragma omp barrier

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOG(neighbor,IN(nlocal),IN(nstencil),ID(mbins),IN(atoms_per_bin),IN(nall));
#endif
  while(resize) {
    #pragma omp barrier
    int new_maxneighs = maxneighs;
    resize = 0;
    #pragma omp barrier

    #pragma omp for schedule(static,CHUNKSIZE)

#ifndef _USE_LOOP_MODEL
    for(int i = 0; i < nlocal; i++) {
      int* neighptr = &neighbors[i * maxneighs];
      /* if necessary, goto next page and add pages */

      int n = 0;

      const MMD_float xtmp = x[3 * i + 0];
      const MMD_float ytmp = x[3 * i + 1];
      const MMD_float ztmp = x[3 * i + 2];

      /* loop over atoms in i's bin,
      */

      const int ibin = coord2bin(xtmp, ytmp, ztmp);

      for(int k = 0; k < nstencil; k++) {
        const int jbin = ibin + stencil[k];

        int* loc_bin = &bins[jbin * atoms_per_bin];

        if(ibin == jbin)
          for(int m = 0; m < bincount[jbin]; m++) {
            const int j = loc_bin[m];

            //for same bin as atom i skip j if i==j and skip atoms "below and to the left" if using halfneighborlists
            if(((j == i) || (halfneigh && !ghost_newton && (j < i)) ||
                (halfneigh && ghost_newton && ((j < i) || ((j >= nlocal) &&
                                               ((x[3 * j + 2] < ztmp) || (x[3 * j + 2] == ztmp && x[3 * j + 1] < ytmp) ||
                                                (x[3 * j + 2] == ztmp && x[3 * j + 1]  == ytmp && x[3 * j + 0] < xtmp))))))) continue;

            const MMD_float delx = xtmp - x[3 * j + 0];
            const MMD_float dely = ytmp - x[3 * j + 1];
            const MMD_float delz = ztmp - x[3 * j + 2];
            const MMD_float rsq = delx * delx + dely * dely + delz * delz;

            if((rsq <= cutneighsq)) neighptr[n++] = j;
          }
        else {
          for(int m = 0; m < bincount[jbin]; m++) {
            const int j = loc_bin[m];

            if(halfneigh && !ghost_newton && (j < i)) continue;

            const MMD_float delx = xtmp - x[3 * j + 0];
            const MMD_float dely = ytmp - x[3 * j + 1];
            const MMD_float delz = ztmp - x[3 * j + 2];
            const MMD_float rsq = delx * delx + dely * dely + delz * delz;

            if((rsq <= cutneighsq)) neighptr[n++] = j;
          }
        }
      }

      numneigh[i] = n;

      if(n >= maxneighs) {
        resize = 1;

        if(n >= new_maxneighs) new_maxneighs = n;
      }
    }
#endif
#ifdef SSTMAC
    SSTMAC_compute_loop3(0,nlocal,0,nstencil,0,atoms_per_bin,8);
#endif

    #pragma omp barrier

    if(resize) {
      #pragma omp master
      {
        free(neighbors);
        maxneighs = new_maxneighs * 1.2;
        neighbors = (int*) malloc(nmax* maxneighs * sizeof(int));
      }
      #pragma omp barrier
    }
  }

  #pragma omp barrier

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOP(neighbor,IN(nlocal),IN(nstencil),ID(mbins),IN(atoms_per_bin),IN(nall));
#endif
}

void Neighbor::binatoms(Atom &atom)
{
  const int omp_me = omp_get_thread_num();
  const int num_omp_threads = threads->omp_num_threads;

  const int nlocal = atom.nlocal;
  const int nall = atom.nlocal + atom.nghost;
  const MMD_float* x = &atom.x[0][0];

  xprd = atom.box.xprd;
  yprd = atom.box.yprd;
  zprd = atom.box.zprd;

  resize = 1;

  #pragma omp barrier

  while(resize > 0) {
    #pragma omp barrier
    resize = 0;
    #pragma omp barrier
    #pragma omp for schedule(static,8)

    for(int i = 0; i < mbins; i++) bincount[i] = 0;


    #pragma omp for schedule(static,CHUNKSIZE)

    for(int i = 0; i < nall; i++) {
      const int ibin = coord2bin(x[3 * i + 0], x[3 * i + 1], x[3 * i + 2]);

      if(bincount[ibin] < atoms_per_bin) {
        int ac;
#ifdef OpenMP31
        #pragma omp atomic capture
        ac = bincount[ibin]++;
#else
        ac = __sync_fetch_and_add(bincount + ibin, 1);
#endif
        bins[ibin * atoms_per_bin + ac] = i;
      } else resize = 1;
    }

    #pragma omp barrier

    #pragma omp master

    if(resize) {
      free(bins);
      atoms_per_bin *= 2;
      bins = (int*) malloc(mbins * atoms_per_bin * sizeof(int));
    }

    #pragma omp barrier
  }

  #pragma omp barrier
}

/* convert xyz atom coords into local bin #
   take special care to insure ghost atoms with
   coord >= prd or coord < 0.0 are put in correct bins */

inline int Neighbor::coord2bin(MMD_float x, MMD_float y, MMD_float z)
{
  int ix, iy, iz;

  if(x >= xprd)
    ix = (int)((x - xprd) * bininvx) + nbinx - mbinxlo;
  else if(x >= 0.0)
    ix = (int)(x * bininvx) - mbinxlo;
  else
    ix = (int)(x * bininvx) - mbinxlo - 1;

  if(y >= yprd)
    iy = (int)((y - yprd) * bininvy) + nbiny - mbinylo;
  else if(y >= 0.0)
    iy = (int)(y * bininvy) - mbinylo;
  else
    iy = (int)(y * bininvy) - mbinylo - 1;

  if(z >= zprd)
    iz = (int)((z - zprd) * bininvz) + nbinz - mbinzlo;
  else if(z >= 0.0)
    iz = (int)(z * bininvz) - mbinzlo;
  else
    iz = (int)(z * bininvz) - mbinzlo - 1;

  return (iz * mbiny * mbinx + iy * mbinx + ix + 1);
}


/*
setup neighbor binning parameters
bin numbering is global: 0 = 0.0 to binsize
                         1 = binsize to 2*binsize
                         nbin-1 = prd-binsize to binsize
                         nbin = prd to prd+binsize
                         -1 = -binsize to 0.0
coord = lowest and highest values of ghost atom coords I will have
        add in "small" for round-off safety
mbinlo = lowest global bin any of my ghost atoms could fall into
mbinhi = highest global bin any of my ghost atoms could fall into
mbin = number of bins I need in a dimension
stencil() = bin offsets in 1-d sense for stencil of surrounding bins
*/

int Neighbor::setup(Atom &atom)
{
  int i, j, k, nmax;
  MMD_float coord;
  int mbinxhi, mbinyhi, mbinzhi;
  int nextx, nexty, nextz;
  int num_omp_threads = threads->omp_num_threads;

  cutneighsq = cutneigh * cutneigh;

  xprd = atom.box.xprd;
  yprd = atom.box.yprd;
  zprd = atom.box.zprd;

  /*
  c bins must evenly divide into box size,
  c   becoming larger than cutneigh if necessary
  c binsize = 1/2 of cutoff is near optimal

  if (flag == 0) {
    nbinx = 2.0 * xprd / cutneigh;
    nbiny = 2.0 * yprd / cutneigh;
    nbinz = 2.0 * zprd / cutneigh;
    if (nbinx == 0) nbinx = 1;
    if (nbiny == 0) nbiny = 1;
    if (nbinz == 0) nbinz = 1;
  }
  */

  binsizex = xprd / nbinx;
  binsizey = yprd / nbiny;
  binsizez = zprd / nbinz;
  bininvx = 1.0 / binsizex;
  bininvy = 1.0 / binsizey;
  bininvz = 1.0 / binsizez;

  coord = atom.box.xlo - cutneigh - SMALL * xprd;
  mbinxlo = static_cast<int>(coord * bininvx);

  if(coord < 0.0) mbinxlo = mbinxlo - 1;

  coord = atom.box.xhi + cutneigh + SMALL * xprd;
  mbinxhi = static_cast<int>(coord * bininvx);

  coord = atom.box.ylo - cutneigh - SMALL * yprd;
  mbinylo = static_cast<int>(coord * bininvy);

  if(coord < 0.0) mbinylo = mbinylo - 1;

  coord = atom.box.yhi + cutneigh + SMALL * yprd;
  mbinyhi = static_cast<int>(coord * bininvy);

  coord = atom.box.zlo - cutneigh - SMALL * zprd;
  mbinzlo = static_cast<int>(coord * bininvz);

  if(coord < 0.0) mbinzlo = mbinzlo - 1;

  coord = atom.box.zhi + cutneigh + SMALL * zprd;
  mbinzhi = static_cast<int>(coord * bininvz);

  /* extend bins by 1 in each direction to insure stencil coverage */

  mbinxlo = mbinxlo - 1;
  mbinxhi = mbinxhi + 1;
  mbinx = mbinxhi - mbinxlo + 1;

  mbinylo = mbinylo - 1;
  mbinyhi = mbinyhi + 1;
  mbiny = mbinyhi - mbinylo + 1;

  mbinzlo = mbinzlo - 1;
  mbinzhi = mbinzhi + 1;
  mbinz = mbinzhi - mbinzlo + 1;

  /*
  compute bin stencil of all bins whose closest corner to central bin
  is within neighbor cutoff
  for partial Newton (newton = 0),
  stencil is all surrounding bins including self
  for full Newton (newton = 1),
  stencil is bins to the "upper right" of central bin, does NOT include self
  next(xyz) = how far the stencil could possibly extend
  factor < 1.0 for special case of LJ benchmark so code will create
  correct-size stencil when there are 3 bins for every 5 lattice spacings
  */

  nextx = static_cast<int>(cutneigh * bininvx);

  if(nextx * binsizex < FACTOR * cutneigh) nextx++;

  nexty = static_cast<int>(cutneigh * bininvy);

  if(nexty * binsizey < FACTOR * cutneigh) nexty++;

  nextz = static_cast<int>(cutneigh * bininvz);

  if(nextz * binsizez < FACTOR * cutneigh) nextz++;

  nmax = (2 * nextz + 1) * (2 * nexty + 1) * (2 * nextx + 1);

  if(stencil) free(stencil);

  stencil = (int*) malloc(nmax * sizeof(int));

  nstencil = 0;
  int kstart = -nextz;

  if(halfneigh && ghost_newton) {
    kstart = 0;
    stencil[nstencil++] = 0;
  }

  for(k = kstart; k <= nextz; k++) {
    for(j = -nexty; j <= nexty; j++) {
      for(i = -nextx; i <= nextx; i++) {
        if(!ghost_newton || !halfneigh || (k > 0 || j > 0 || (j == 0 && i > 0)))
          if(bindist(i, j, k) < cutneighsq) {
            stencil[nstencil++] = k * mbiny * mbinx + j * mbinx + i;
          }
      }
    }
  }

  mbins = mbinx * mbiny * mbinz;

  if(bincount) free(bincount);

  bincount = (int*) malloc(mbins * num_omp_threads * sizeof(int));

  if(bins) free(bins);

  bins = (int*) malloc(mbins * num_omp_threads * atoms_per_bin * sizeof(int));
  return 0;
}

/* compute closest distance between central bin (0,0,0) and bin (i,j,k) */

MMD_float Neighbor::bindist(int i, int j, int k)
{
  MMD_float delx, dely, delz;

  if(i > 0)
    delx = (i - 1) * binsizex;
  else if(i == 0)
    delx = 0.0;
  else
    delx = (i + 1) * binsizex;

  if(j > 0)
    dely = (j - 1) * binsizey;
  else if(j == 0)
    dely = 0.0;
  else
    dely = (j + 1) * binsizey;

  if(k > 0)
    delz = (k - 1) * binsizez;
  else if(k == 0)
    delz = 0.0;
  else
    delz = (k + 1) * binsizez;

  return (delx * delx + dely * dely + delz * delz);
}
