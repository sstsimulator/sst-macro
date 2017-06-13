/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

int makeHDF5type0(MPI_Datatype *type);
int makeHDF5type0(MPI_Datatype *type)
{
    MPI_Datatype ctg, vect, structype, vec2, structype2,
                 vec3, structype3, vec4, structype4, vec5;

    int b[3];
    MPI_Aint d[3];
    MPI_Datatype t[3];

    MPI_Type_contiguous(4, MPI_BYTE, &ctg);

    MPI_Type_vector(1, 5, 1, ctg, &vect);

    b[0] =         b[1] =       b[2] = 1;
    d[0] = 0;      d[1] = 0;    d[2] = 40;
    t[0] = MPI_LB; t[1] = vect; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype);

    MPI_Type_vector(1, 5, 1, structype, &vec2);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 2000;  d[2] = 400;
    t[0] = MPI_LB; t[1] = vec2;  t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype2);

    MPI_Type_vector(1, 5, 1, structype2, &vec3);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 0;     d[2] = 4000;
    t[0] = MPI_LB; t[1] = vec3;  t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype3);

    MPI_Type_vector(1, 5, 1, structype3, &vec4);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 0;     d[2] = 40000;
    t[0] = MPI_LB; t[1] = vec4;  t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype4);

    MPI_Type_vector(1, 1, 1, structype4, &vec5);

    b[0] =         b[1] =         b[2] = 1;
    d[0] = 0;      d[1] = 160000; d[2] = 200000;
    t[0] = MPI_LB; t[1] = vec5;   t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, type);

    MPI_Type_free(&ctg);
    MPI_Type_free(&vect);
    MPI_Type_free(&structype);
    MPI_Type_free(&vec2);
    MPI_Type_free(&structype2);
    MPI_Type_free(&vec3);
    MPI_Type_free(&structype3);
    MPI_Type_free(&vec4);
    MPI_Type_free(&structype4);
    MPI_Type_free(&vec5);
    MPI_Type_commit(type);

    return 0;
}

int makeHDF5type1(MPI_Datatype *type);
int makeHDF5type1(MPI_Datatype *type)
{
    MPI_Datatype ctg, vect, structype, vec2, structype2,
                 vec3, structype3, vec4, structype4, vec5;

    int b[3];
    MPI_Aint d[3];
    MPI_Datatype t[3];

    MPI_Type_contiguous(4, MPI_BYTE, &ctg);

    MPI_Type_vector(1, 5, 1, ctg, &vect);

    b[0] =         b[1] =       b[2] = 1;
    d[0] = 0;      d[1] = 20;    d[2] = 40;
    t[0] = MPI_LB; t[1] = vect; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype);

    MPI_Type_vector(1, 5, 1, structype, &vec2);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 0;     d[2] = 400;
    t[0] = MPI_LB; t[1] = vec2; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype2);

    MPI_Type_vector(1, 5, 1, structype2, &vec3);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 0;     d[2] = 4000;
    t[0] = MPI_LB; t[1] = vec3; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype3);

    MPI_Type_vector(1, 5, 1, structype3, &vec4);

    b[0] =         b[1] =        b[2] = 1;
    d[0] = 0;      d[1] = 0;     d[2] = 40000;
    t[0] = MPI_LB; t[1] = vec4; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, &structype4);

    MPI_Type_vector(1, 1, 1, structype4, &vec5);

    b[0] =         b[1] =         b[2] = 1;
    d[0] = 0;      d[1] = 160000; d[2] = 200000;
    t[0] = MPI_LB; t[1] = vec5; t[2] = MPI_UB;
    MPI_Type_create_struct(3, b, d, t, type);

    MPI_Type_free(&ctg);
    MPI_Type_free(&vect);
    MPI_Type_free(&structype);
    MPI_Type_free(&vec2);
    MPI_Type_free(&structype2);
    MPI_Type_free(&vec3);
    MPI_Type_free(&structype3);
    MPI_Type_free(&vec4);
    MPI_Type_free(&structype4);
    MPI_Type_free(&vec5);
    MPI_Type_commit(type);

    return 0;
}

int makeHDF5type(MPI_Datatype *type);
int makeHDF5type(MPI_Datatype *type)
{
    int i;

#define NTYPES 2

    int blocklens[NTYPES];
    MPI_Aint disps[NTYPES];

    MPI_Datatype types[NTYPES];
    makeHDF5type0(&(types[0]));
    makeHDF5type1(&(types[1]));

    for (i=0; i< NTYPES; i++) {
        blocklens[i] = 1;
        disps[i] = 0;
    }

    MPI_Type_create_struct(NTYPES, blocklens, disps, types, type);
    MPI_Type_commit(type);

    for(i=0; i<NTYPES; i++) {
        MPI_Type_free(&(types[i]));
    }
    return 0;
}

int struct_verydeep(int argc, char **argv)
{
    MPI_Datatype hdf5type;

    MPI_Init(&argc, &argv);
    makeHDF5type(&hdf5type);

    /**MPIDU_Datatype_debug(hdf5type, 32);*/

    MPI_Type_free(&hdf5type);
    MPI_Finalize();

    printf(" No Errors\n");

    return 0;
}


}