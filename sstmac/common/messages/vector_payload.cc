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

#include <sstmac/common/messages/vector_payload.h>



DeclareSerializable(sstmac::vector1_payload<int>);
DeclareSerializable(sstmac::vector1_payload<long>);
DeclareSerializable(sstmac::vector1_payload<bool>);
DeclareSerializable(sstmac::vector1_payload<short>);
DeclareSerializable(sstmac::vector1_payload<float>);
DeclareSerializable(sstmac::vector1_payload<double>);
DeclareSerializable(sstmac::vector1_payload<char>);

#define DeclarePointerVec(ty) \
    DeclareSerializable(sstmac::vector1_payload<ty,ty*>)

DeclarePointerVec(int);
DeclarePointerVec(char);
DeclarePointerVec(bool);
DeclarePointerVec(double);
DeclarePointerVec(float);
DeclarePointerVec(long);
DeclarePointerVec(short);

typedef long long lli;
typedef unsigned long long ulli;
typedef unsigned long uli;
typedef long double ldouble;
typedef unsigned int ui;
DeclarePointerVec(ui);
DeclarePointerVec(lli);
DeclarePointerVec(uli);
DeclarePointerVec(ulli);
DeclarePointerVec(ldouble);






