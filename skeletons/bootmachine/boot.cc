#include <mpi.h>
#include <iostream>

static int num_launched = 0;
static int num_inited = 0;
static int num_finalized = 0;
static int print_interval = 100;

int main(int argc, char **argv)
{
    ++num_launched;
    if (num_launched % print_interval == 0){
        std::cout << "passed launch " << (num_launched / print_interval) << std::endl; 
    }
    MPI_Init(&argc, &argv);
    ++num_inited;
    if (num_inited % print_interval == 0){
        std::cout << "passed init " << (num_inited / print_interval) << std::endl; 
    }
    MPI_Finalize();
    ++num_finalized;
    if (num_finalized % print_interval == 0){
        std::cout << "passed finalize " << (num_finalized / print_interval) << std::endl; 
    }
    return 0;
}

