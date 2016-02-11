//@HEADER
// ************************************************************************
// 
//               HPCCG: Simple Conjugate Gradient Benchmark Code
//                 Copyright (2006) Sandia Corporation
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
// Questions? Contact Michael A. Heroux (maherou@sandia.gov) 
// 
// ************************************************************************
//@HEADER

/////////////////////////////////////////////////////////////////////////

// Routine to compute an approximate solution to Ax = b where:

// A - known matrix stored as an HPC_Sparse_Matrix struct

// b - known right hand side vector

// x - On entry is initial guess, on exit new approximate solution

// max_iter - Maximum number of iterations to perform, even if
//            tolerance is not met.

// tolerance - Stop and assert convergence if norm of residual is <=
//             to tolerance.

// niters - On output, the number of iterations actually performed.

/////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <cmath>  // needed for fabs
#include <cstdio>
#include <cstdlib>
#include <cctype>
#include <cassert>
using std::fabs;
using std::cout;
using std::cerr;
using std::endl;
#include <cmath>

#include "HPCCG.hpp"
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
#include "lwperf.h"
#endif

void
dump(std::string name, const double* const arr, int num)
{
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  std::cout << "----- rank " << rank << " dumping array " << name << " ("
      << num << " elements)\n";
  for (int i = 0; i < num; i++)
  {
    std::cout << i << ": " << arr[i] << "\n";
  }
  // exit(0);
}

#define TICK()  t0 = mytimer() // Use TICK and TOCK to time a code section
#define TOCK(t) t += mytimer() - t0
int
HPCCG(HPC_Sparse_Matrix * A, const double * const b, double * const x,
    const int max_iter, const double tolerance, int &niters, double & normr,
    double * times)

{
  double t_begin = mytimer(); // Start timing right away

  double t0 = 0.0, t1 = 0.0, t2 = 0.0, t3 = 0.0, t4 = 0.0;

  double t5 = 0.0;

  int nrow = A->local_nrow;
  int ncol = A->local_ncol;

  double * r = new double[nrow];
  double * p = new double[ncol]; // In parallel case, A is rectangular
  double * Ap = new double[nrow];

  normr = 0.0;
  double rtrans = 0.0;
  double oldrtrans = 0.0;


  int size, rank; // Number of MPI processes, My process ID
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);


  int print_freq = max_iter / 10;
  if (print_freq > 50)
    print_freq = 50;
  if (print_freq < 1)
    print_freq = 1;

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOG(HPCCGa,IN(nrow), IN(ncol));
#endif
  // p is of length ncols, copy x to p for sparse MV operation
  TICK();
  waxpby(nrow, 1.0, x, 0.0, x, p);
  TOCK(t2);

#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOP(HPCCGa,IN(nrow), IN(ncol));
#endif

  TICK(); exchange_externals(A,p); TOCK(t5);


  TICK();
  HPC_sparsemv(A, p, Ap);
  TOCK(t3);
  //dump("b", b, nrow);

  TICK();
  waxpby(nrow, 1.0, b, -1.0, Ap, r);
  TOCK(t2);
  TICK();
  ddot(nrow, r, r, &rtrans, t4);
  TOCK(t1);
  normr = sqrt(rtrans);

  if (rank == 0)
    cout << "Initial Residual = " << normr << endl;

  for (int k = 1; k < max_iter && normr > tolerance; k++)
  {
    if (k == 1)
    {
      TICK();
      waxpby(nrow, 1.0, r, 0.0, r, p);
      TOCK(t2);
    }
    else
    {
      oldrtrans = rtrans;
      TICK();
      ddot(nrow, r, r, &rtrans, t4);
      TOCK(t1);// 2*nrow ops
      double beta = rtrans / oldrtrans;
      TICK();
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
      PERFLOG(waxpby,IN(nrow));
#endif
      waxpby(nrow, 1.0, r, beta, p, p);
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
      PERFSTOP(waxpby,IN(nrow));
#endif
      TOCK(t2);// 2*nrow ops
    }
    normr = sqrt(rtrans);
    if (rank == 0 && (k % print_freq == 0 || k + 1 == max_iter))
      cout << "Iteration = " << k << "   Residual = " << normr << endl;


    TICK(); exchange_externals(A,p); TOCK(t5);

    TICK();
    HPC_sparsemv(A, p, Ap);
    TOCK(t3); // 2*nnz ops
    double alpha = 0.0;
    TICK();
    ddot(nrow, p, Ap, &alpha, t4);
    TOCK(t1); // 2*nrow ops
    alpha = rtrans / alpha;
    TICK();
    waxpby(nrow, 1.0, x, alpha, p, x);// 2*nrow ops
    waxpby(nrow, 1.0, r, -alpha, Ap, r);
    TOCK(t2);// 2*nrow ops
    niters = k;
  }

  // Store times
  times[1] = t1; // ddot time
  times[2] = t2; // waxpby time
  times[3] = t3; // sparsemv time
  times[4] = t4; // AllReduce time

  times[5] = t5; // exchange boundary time

  delete[] p;
  delete[] Ap;
  delete[] r;
  times[0] = mytimer() - t_begin; // Total time. All done...
  return (0);
}

/////////////////////////////////////////////////////////////////////////

// Routine to compute the 1-norm difference between two vectors where:

// n - number of vector elements (on this processor)

// v1, v2 - input vectors

// residual - pointer to scalar value, on exit will contain result.

/////////////////////////////////////////////////////////////////////////


int
compute_residual(const int n, const double * const v1, const double * const v2,
    double * const residual)
{
  double local_residual = 0.0;
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOG(compute_residual,IN(n));
#endif
#ifndef _USE_LOOP_MODEL
  for (int i = 0; i < n; i++)
  {
    double diff = fabs(v1[i] - v2[i]);
    if (diff > local_residual)
      local_residual = diff;
  }
#endif
#ifdef SSTMAC
  // a little compute modeling
  SSTMAC_compute_loop(0, n, 2);
#endif


#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOP(compute_residual,IN(n));
#endif
  // Use MPI's reduce function to collect all partial sums

  double global_residual = 0;


  MPI_Allreduce(&local_residual, &global_residual, 1, MPI_DOUBLE, MPI_SUM,
      MPI_COMM_WORLD);
  *residual = global_residual;


  return (0);
}

/////////////////////////////////////////////////////////////////////////

// Routine to compute the dot product of two vectors where:

// n - number of vector elements (on this processor)

// x, y - input vectors

// residual - pointer to scalar value, on exit will contain result.

/////////////////////////////////////////////////////////////////////////

int
ddot(const int n, const double * const x, const double * const y,
    double * const result, double & time_allreduce)
{
  double local_result = 0.0;
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOG(ddot,IN(n));
#endif
#ifndef _USE_LOOP_MODEL
  if (y == x)
    for (int i = 0; i < n; i++)
      local_result += x[i] * x[i];
  else
    for (int i = 0; i < n; i++)
      local_result += x[i] * y[i];
#endif


#ifdef SSTMAC
  // a little compute modeling
  SSTMAC_compute_loop(0, n, 1);
#endif
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOP(ddot,IN(n));
#endif

  // Use MPI's reduce function to collect all partial sums
  double t0 = mytimer();
  double global_result = 0.0;
  int rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  // std::cout << "rank " << rank << " computed local ddot as  " << local_result << "\n";
  MPI_Allreduce(&local_result, &global_result, 1, MPI_DOUBLE, MPI_SUM,
      MPI_COMM_WORLD);
  *result = global_result;
  time_allreduce += mytimer() - t0;


  return (0);
}

/////////////////////////////////////////////////////////////////////////

// Routine to exchange with other ranks

/////////////////////////////////////////////////////////////////////////

void
exchange_externals(HPC_Sparse_Matrix * A, const double *x)
{
  int i, j, k;
  int num_external = 0;

  // Extract Matrix pieces

  int local_nrow = A->local_nrow;
  int num_neighbors = A->num_send_neighbors;
  int * recv_length = A->recv_length;
  int * send_length = A->send_length;
  int * neighbors = A->neighbors;
  double * send_buffer = A->send_buffer;
  int total_to_be_sent = A->total_to_be_sent;
  int * elements_to_send = A->elements_to_send;

  int size, rank; // Number of MPI processes, My process ID
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  //
  //  first post receives, these are immediate receives
  //  Do not wait for result to come, will do that at the
  //  wait call below.
  //

  int MPI_MY_TAG = 99;

  MPI_Request * request = new MPI_Request[num_neighbors];

  for(int i = 0; i < num_neighbors; i++)
  {
    request[i] = MPI_REQUEST_NULL;
  }

  //
  // Externals are at end of locals
  //
  double *x_external = (double *) x + local_nrow;

  // Post receives first
  for (i = 0; i < num_neighbors; i++)
  {
    int n_recv = recv_length[i];
    MPI_Irecv(x_external, n_recv, MPI_DOUBLE, neighbors[i], MPI_MY_TAG,
        MPI_COMM_WORLD, request+i);
    x_external += n_recv;
  }

  //
  // Fill up send buffer
  //

  for (i=0; i<total_to_be_sent; i++)
  {
    // std::cout << "exchange_externals(" << rank << "): accesing element " << elements_to_send[i] << "\n";
    send_buffer[i] = x[elements_to_send[i]];
  }

  //
  // Send to each neighbor
  //

  for (i = 0; i < num_neighbors; i++)
  {
    int n_send = send_length[i];
    MPI_Send(send_buffer, n_send, MPI_DOUBLE, neighbors[i], MPI_MY_TAG,
        MPI_COMM_WORLD);
    send_buffer += n_send;
  }

  //
  // Complete the reads issued above
  //

  MPI_Status status;
  for (i = 0; i < num_neighbors; i++)
  {
    if ( MPI_Wait(request+i, &status) )
    {
      cerr << "MPI_Wait error\n"<<endl;
      exit(-1);
    }
  }

  delete [] request;

  return;
}

/////////////////////////////////////////////////////////////////////////

// Routine to compute matrix vector product y = Ax where:
// First call exchange_externals to get off-processor values of x

// A - known matrix
// x - known vector
// y - On exit contains Ax.

/////////////////////////////////////////////////////////////////////////

int
HPC_sparsemv(HPC_Sparse_Matrix *A, const double * const x, double * const y)
{
  const int nrow = (const int) A->local_nrow;

  int countit = 0;
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFLOGKEEP(HPC_sparsemv,IN(nrow), IN(countit));
#endif
  for (int i = 0; i < nrow; i++)
  {
    double sum = 0.0;
    const double * const cur_vals =
        (const double * const ) A->ptr_to_vals_in_row[i];

    const int * const cur_inds = (const int * const ) A->ptr_to_inds_in_row[i];

    const int cur_nnz = (const int) A->nnz_in_row[i];

    for (int j = 0; j < cur_nnz; j++)
    {
#ifndef _USE_LOOP_MODEL
      sum += cur_vals[j] * x[cur_inds[j]];
#endif
      countit++;
    }

    y[i] = sum;
  }

#ifdef SSTMAC
  //here's some compute modeling
  SSTMAC_compute_loop(0, countit, 1);
#endif
#if defined(_USE_EIGER_MODEL) || defined(_USE_EIGER) || \
    defined(_USE_CSV) || defined(_USE_FAKEEIGER)
  PERFSTOPKEEP(HPC_sparsemv,IN(nrow), IN(countit));
#endif
  return (0);
}

/////////////////////////////////////////////////////////////////////////

// Function to return time in seconds.
// If compiled with no flags, return CPU time (user and system).
// If compiled with -DWALL, returns elapsed time.

/////////////////////////////////////////////////////////////////////////

double mytimer(void)
{
  return(MPI_Wtime());
}


/////////////////////////////////////////////////////////////////////////

// Routine to compute the update of a vector with the sum of two
// scaled vectors where:

// w = alpha*x + beta*y

// n - number of vector elements (on this processor)

// x, y - input vectors

// alpha, beta - scalars applied to x and y respectively.

// w - output vector.

/////////////////////////////////////////////////////////////////////////

int
waxpby(const int n, const double alpha, const double * const x,
    const double beta, const double * const y, double * const w)
{
#ifndef _USE_LOOP_MODEL
  if (alpha == 1.0)
    for (int i = 0; i < n; i++)
      w[i] = x[i] + beta * y[i];
  else if (beta == 1.0)
    for (int i = 0; i < n; i++)
      w[i] = alpha * x[i] + y[i];
  else
    for (int i = 0; i < n; i++)
      w[i] = alpha * x[i] + beta * y[i];
#endif

#ifdef SSTMAC
  SSTMAC_compute_loop(0, n, 1);
#endif

  return (0);
}
