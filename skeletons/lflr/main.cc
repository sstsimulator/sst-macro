#include <mpi.h>
#include <cmath>
#include <cstring>
#include <algorithm>

#define sstmac_app_name "lflr"

int main(int argc, char** argv)
{
    MPI_Init(&argc, &argv);

    int me;
    int nproc;
    MPI_Comm_rank(MPI_COMM_WORLD, &me);
    MPI_Comm_size(MPI_COMM_WORLD, &nproc);

    int proc_grid_dim_size = sqrt(nproc + 0.5);
    int nproc_check = proc_grid_dim_size * proc_grid_dim_size;
    if (nproc_check != nproc){
        if (me == 0){
            fprintf(stderr, "MPI sysmxm must run on a square number of processors");
            abort();
        }
    }

    int nblocks_row = 20;
    int nblocks_col = 20;
    int nblocks_link = 20;
    int nblocks_max = std::max(nblocks_row, std::max(nblocks_col, nblocks_link));
    int blockdim = 200;
    int block_nrows = blockdim;
    int block_ncols = blockdim;
    int block_nlink = blockdim;
    int blocksize = blockdim * blockdim;
    int num_slow_nodes = 0;


    int root = 0;

    // divide into a square matrix
    int nelems_row = nblocks_row * block_nrows;
    int nelems_col = nblocks_col * block_ncols;
    int nelems_link = nblocks_col * block_nlink;

    int nelems_row_per_proc = nelems_row / proc_grid_dim_size;
    int nelems_col_per_proc = nelems_col / proc_grid_dim_size;
    int nelems_link_per_proc = nelems_link / proc_grid_dim_size;

    int nelems_product_block = nelems_row_per_proc*nelems_col_per_proc;
    int nelems_left_block = nelems_row_per_proc*nelems_link_per_proc;
    int nelems_right_block = nelems_link_per_proc*nelems_col_per_proc;

    double* product_block = new double[nelems_product_block];
    double* left_block = new double[nelems_left_block];
    double* right_block = new double[nelems_right_block];
    double* next_product_block = new double[nelems_product_block];
    double* next_left_block = new double[nelems_left_block];
    double* next_right_block = new double[nelems_right_block];

#if 0
    ::memset(product_block, 0, sizeof(double)*nelems_product_block);
    ::memset(next_product_block, 0, sizeof(double)*nelems_product_block);
    ::memset(left_block, 0, sizeof(double)*nelems_left_block);
    ::memset(next_left_block, 0, sizeof(double)*nelems_left_block);
    ::memset(right_block, 0, sizeof(double)*nelems_right_block);
    ::memset(next_right_block, 0, sizeof(double)*nelems_right_block);
#endif

    int niter = proc_grid_dim_size;

    if (me == 0){
        printf("Proc grid is %d x %d\n", proc_grid_dim_size, proc_grid_dim_size);
    }

    int my_row = me / proc_grid_dim_size;
    int my_col = me % proc_grid_dim_size;

    int up_row = (my_row + 1) % proc_grid_dim_size;
    int down_row = (my_row - 1 + proc_grid_dim_size) % proc_grid_dim_size;
    int up_col = (my_col + 1) % proc_grid_dim_size;
    int down_col = (my_col - 1 + proc_grid_dim_size) % proc_grid_dim_size;

    int row_send_partner = up_row*proc_grid_dim_size + my_col;
    int row_recv_partner = down_row*proc_grid_dim_size + my_col;
    int col_send_partner = my_row*proc_grid_dim_size + up_col;
    int col_recv_partner = my_row*proc_grid_dim_size + down_col;

    #define NUM_REQUESTS 4
    //4 requests, 2 sends, 2 recvs
    MPI_Request reqs[NUM_REQUESTS];

    int row_tag = 0;
    int col_tag = 1;

    int nrepeat = 1;
    int last_full_repeat = -1;
    double last_repeat_fraction = 1.0;

    double start = MPI_Wtime();

    double total_mult_time = 0;
    for (int iter=0; iter < niter; ++iter){
        //I receive from -1, send to +1
        MPI_Isend(left_block, nelems_left_block, MPI_DOUBLE, row_send_partner, row_tag, MPI_COMM_WORLD, &reqs[0]);
        MPI_Isend(right_block, nelems_right_block, MPI_DOUBLE, col_send_partner, col_tag, MPI_COMM_WORLD, &reqs[1]);
        MPI_Irecv(next_left_block, nelems_left_block, MPI_DOUBLE, row_recv_partner, row_tag, MPI_COMM_WORLD, &reqs[2]);
        MPI_Irecv(next_right_block, nelems_right_block, MPI_DOUBLE, col_recv_partner, col_tag, MPI_COMM_WORLD, &reqs[3]);

        double task_start = MPI_Wtime();
        
        //matmul


        MPI_Waitall(NUM_REQUESTS, reqs, MPI_STATUSES_IGNORE);

        if (me == 0){
            printf("Iteration %3d done\n", iter);
        }
    }
    double stop = MPI_Wtime();
    MPI_Finalize();

    if (me == 0){
        double avg_mult_time = total_mult_time / niter;
        printf("Average multiply %8.4f ms\n", avg_mult_time*1e3);
        printf("Ran for %8.4f seconds\n", stop-start);
    }

    return 0;
}


