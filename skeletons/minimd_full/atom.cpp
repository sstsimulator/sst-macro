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
#include "string.h"
#include "stdlib.h"
#include "mpi.h"
#include "atom.h"

#define DELTA 20000

Atom::Atom()
{
  natoms = 0;
  nlocal = 0;
  nghost = 0;
  nmax = 0;

  x = v = f = xold = NULL;

  comm_size = 3;
  reverse_size = 3;
  border_size = 3;

  mass = 1;
}

Atom::~Atom()
{
  if(nmax) {
    destroy_2d_MMD_float_array(x);
    destroy_2d_MMD_float_array(v);
    destroy_2d_MMD_float_array(f);
    destroy_2d_MMD_float_array(xold);
  }
}

void Atom::growarray()
{
  int nold = nmax;
  nmax += DELTA;
  x = (MMD_float**) realloc_2d_MMD_float_array(x, nmax, 3, 3 * nold);
  v = (MMD_float**) realloc_2d_MMD_float_array(v, nmax, 3, 3 * nold);
  f = (MMD_float**) realloc_2d_MMD_float_array(f, nmax, 3, 3 * nold);
  xold = (MMD_float**) realloc_2d_MMD_float_array(xold, nmax, 3, 3 * nold);

  if(x == NULL || v == NULL || f == NULL || xold == NULL) {
    printf("ERROR: No memory for atoms\n");
  }
}

void Atom::addatom(MMD_float x_in, MMD_float y_in, MMD_float z_in,
                   MMD_float vx_in, MMD_float vy_in, MMD_float vz_in)
{
  if(nlocal == nmax) growarray();

  x[nlocal][0] = x_in;
  x[nlocal][1] = y_in;
  x[nlocal][2] = z_in;
  v[nlocal][0] = vx_in;
  v[nlocal][1] = vy_in;
  v[nlocal][2] = vz_in;

  nlocal++;
}

/* enforce PBC
   order of 2 tests is important to insure lo-bound <= coord < hi-bound
   even with round-off errors where (coord +/- epsilon) +/- period = bound */

void Atom::pbc()
{
  #pragma omp for

  for(int i = 0; i < nlocal; i++) {
    if(x[i][0] < 0.0) x[i][0] += box.xprd;

    if(x[i][0] >= box.xprd) x[i][0] -= box.xprd;

    if(x[i][1] < 0.0) x[i][1] += box.yprd;

    if(x[i][1] >= box.yprd) x[i][1] -= box.yprd;

    if(x[i][2] < 0.0) x[i][2] += box.zprd;

    if(x[i][2] >= box.zprd) x[i][2] -= box.zprd;
  }
}

void Atom::copy(int i, int j)
{
  x[j][0] = x[i][0];
  x[j][1] = x[i][1];
  x[j][2] = x[i][2];
  v[j][0] = v[i][0];
  v[j][1] = v[i][1];
  v[j][2] = v[i][2];
}

void Atom::pack_comm(int n, int* list, MMD_float* buf, int* pbc_flags)
{
  int i, j;

  if(pbc_flags[0] == 0) {
    #pragma omp for schedule(static,8)

    for(i = 0; i < n; i++) {
      j = list[i];
      buf[3 * i] = x[j][0];
      buf[3 * i + 1] = x[j][1];
      buf[3 * i + 2] = x[j][2];
    }
  } else {
    #pragma omp for schedule(static,8)

    for(i = 0; i < n; i++) {
      j = list[i];
      buf[3 * i] = x[j][0] + pbc_flags[1] * box.xprd;
      buf[3 * i + 1] = x[j][1] + pbc_flags[2] * box.yprd;
      buf[3 * i + 2] = x[j][2] + pbc_flags[3] * box.zprd;
    }
  }
}

void Atom::unpack_comm(int n, int first, MMD_float* buf)
{
  int i;

  #pragma omp for schedule(static,8)

  for(i = 0; i < n; i++) {
    x[first + i][0] = buf[3 * i];
    x[first + i][1] = buf[3 * i + 1];
    x[first + i][2] = buf[3 * i + 2];
  }
}

void Atom::pack_reverse(int n, int first, MMD_float* buf)
{
  int i;
  #pragma omp for schedule(static,8)

  for(i = 0; i < n; i++) {
    buf[3 * i] = f[first + i][0];
    buf[3 * i + 1] = f[first + i][1];
    buf[3 * i + 2] = f[first + i][2];
  }
}

void Atom::unpack_reverse(int n, int* list, MMD_float* buf)
{
  int i, j;
  #pragma omp for schedule(static,8)

  for(i = 0; i < n; i++) {
    j = list[i];
    f[j][0] += buf[3 * i];
    f[j][1] += buf[3 * i + 1];
    f[j][2] += buf[3 * i + 2];
  }
}

int Atom::pack_border(int i, MMD_float* buf, int* pbc_flags)
{
  int m = 0;

  if(pbc_flags[0] == 0) {
    buf[m++] = x[i][0];
    buf[m++] = x[i][1];
    buf[m++] = x[i][2];
  } else {
    buf[m++] = x[i][0] + pbc_flags[1] * box.xprd;
    buf[m++] = x[i][1] + pbc_flags[2] * box.yprd;
    buf[m++] = x[i][2] + pbc_flags[3] * box.zprd;
  }

  return m;
}

int Atom::unpack_border(int i, MMD_float* buf)
{
  if(i == nmax) growarray();

  int m = 0;
  x[i][0] = buf[m++];
  x[i][1] = buf[m++];
  x[i][2] = buf[m++];
  return m;
}

int Atom::pack_exchange(int i, MMD_float* buf)
{
  int m = 0;
  buf[m++] = x[i][0];
  buf[m++] = x[i][1];
  buf[m++] = x[i][2];
  buf[m++] = v[i][0];
  buf[m++] = v[i][1];
  buf[m++] = v[i][2];
  return m;
}

int Atom::unpack_exchange(int i, MMD_float* buf)
{
  if(i == nmax) growarray();

  int m = 0;
  x[i][0] = buf[m++];
  x[i][1] = buf[m++];
  x[i][2] = buf[m++];
  v[i][0] = buf[m++];
  v[i][1] = buf[m++];
  v[i][2] = buf[m++];
  return m;
}

int Atom::skip_exchange(MMD_float* buf)
{
  return 6;
}

/* realloc a 2-d MMD_float array */

MMD_float** Atom::realloc_2d_MMD_float_array(MMD_float** array,
    int n1, int n2, int nold)

{
  MMD_float** newarray;

  newarray = create_2d_MMD_float_array(n1, n2);

  if(nold) memcpy(newarray[0], array[0], nold * sizeof(MMD_float));

  destroy_2d_MMD_float_array(array);

  return newarray;
}

/* create a 2-d MMD_float array */

MMD_float** Atom::create_2d_MMD_float_array(int n1, int n2)
{
  int ALIGN = 16;
  MMD_float** array;
  MMD_float* data;
  int i, n;

  if(n1 * n2 == 0) return NULL;

  array = (MMD_float**) malloc(n1 * sizeof(MMD_float*));
  data = (MMD_float*) malloc((n1 * n2 + 1024 + 1) * sizeof(MMD_float));
  long mask64 = 0;

  for(int j = 0, k = 1; j < 8; j++, k *= 2) {
    mask64 = mask64 | k;
  }

  while((long)data & mask64) data++;

  //long data_align = (long) data;
  //data_align = (data_align/ALIGN)*(ALIGN+1);
  //((MMD_float*) data_align)[n1*n2]=(MMD_float) (data_align-(long)data);
  // data = (MMD_float*)data_align;
  n = 0;

  for(i = 0; i < n1; i++) {
    array[i] = &data[n];
    n += n2;
  }

  return array;
}

/* free memory of a 2-d MMD_float array */

void Atom::destroy_2d_MMD_float_array(MMD_float** array)
{

  if(array != NULL) {
    //free(array[0]);
    free(array);
  }
}
