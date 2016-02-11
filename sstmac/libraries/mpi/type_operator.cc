/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/libraries/mpi/type_operator.h>


DeclareSerializable(sstmac::sw::mpi_null_op);

typedef long long lli;
typedef unsigned long long ulli;
typedef unsigned long uli;
typedef long double ldouble;
typedef unsigned int ui;
typedef unsigned short us;
typedef signed char sc;
typedef unsigned char usc;

#define DeclarePrim(ty) \
    DeclareSerializable(sstmac::sw::mpi_prim_op<ty>)

DeclarePrim(int);
DeclarePrim(long);
DeclarePrim(char);
DeclarePrim(bool);
DeclarePrim(float);
DeclarePrim(double);
DeclarePrim(short);
DeclarePrim(lli);
DeclarePrim(ulli);
DeclarePrim(uli);
DeclarePrim(ldouble);
DeclarePrim(ui);
DeclarePrim(us);
DeclarePrim(sc);
DeclarePrim(usc);

#define DeclareBitPrim(ty) \
    typedef sstmac::sw::mpi_prim_bit_op<ty> paybtype_##ty; \
    DeclareSerializable(paybtype_##ty)

DeclareBitPrim(int);
DeclareBitPrim(long);
DeclareBitPrim(char);
DeclareBitPrim(bool);
DeclareBitPrim(short);
DeclareBitPrim(lli);
DeclareBitPrim(ulli);
DeclareBitPrim(uli);
DeclareBitPrim(ui);
DeclareBitPrim(us);
DeclareBitPrim(sc);
DeclareBitPrim(usc);

#define DeclarePairPrim(ty1, ty2) \
    typedef sstmac::sw::mpi_pair_op<ty1, ty2> payptype_##ty1##ty2; \
    DeclareSerializable(payptype_##ty1##ty2)

DeclarePairPrim(int, int);
DeclarePairPrim(long, int);
DeclarePairPrim(char, int);
DeclarePairPrim(bool, int);
DeclarePairPrim(float, int);
DeclarePairPrim(float, float);
DeclarePairPrim(double, int);
DeclarePairPrim(double, double);
DeclarePairPrim(short, int);
DeclarePairPrim(lli, int);
DeclarePairPrim(ulli, int);
DeclarePairPrim(uli, int);
DeclarePairPrim(ldouble, int);
DeclarePairPrim(ldouble, ldouble);
DeclarePairPrim(ui, int);

