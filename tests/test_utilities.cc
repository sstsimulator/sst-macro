#include <sstmac/common/rng.h>
#include <cstdio>

void test_random_numbers()
{
    RNG::NormalDistribution* gaussian
            = new RNG::NormalDistribution(10.0, 5.0);

    for (int i=0; i < 10; ++i)
    {
        double val = gaussian->value();
        if (val <= 20.0 && val >= 0.0)
            printf("SUCCESS on normal distribution\n");
        else
            printf("FAILURE on normal distribution\n");
    }
}


int main(int argc, char** argv)
{
    test_random_numbers();
}

