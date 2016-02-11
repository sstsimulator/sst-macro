#include <stdlib.h>
#include <stdio.h>

#include <mpi.h>

#ifndef SSTMAC
int global_var_;
#else
global_int global_var_;
#endif

/*-------------------------------
  function to integrate
-------------------------------*/

double f_x(double x) {
  return x * x;
}


/*-------------------------------
   input variables
-------------------------------*/

int nintervals = 10000;
double start = 0;
double end = 1;


/*-------------------------------
   main 
-------------------------------*/

int main(int argc, char* argv[]) {
    int i, rank, size, nloop;
    double x, interval_size;
    double area=0;
    double total=-1.0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if(nintervals < 1)
        exit(EXIT_FAILURE);

#ifndef SSTMAC
    interval_size = (end - start) / nintervals;
    for(i=rank; i<nintervals; i+=size) {
        x = (i + 0.5) * interval_size;
        area += f_x(x) * interval_size;
    }
#else
    /* pretend computation takes 0.1us per loop iteration */
    nloop = nintervals/size + ((rank < (nintervals % size)) ? 1 : 0);
    SSTMAC_compute(nloop * 1e-7);
#endif
    
    MPI_Reduce(&area, &total, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);

    if(rank==0) printf("result is %lf\n",total);

    MPI_Finalize();

    return EXIT_SUCCESS;
}

