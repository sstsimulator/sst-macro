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

#include <sstmac/common/messages/value_payload.h>


DeclareSerializable(sstmac::bitwisevaluepayload<int>);
DeclareSerializable(sstmac::bitwisevaluepayload<bool>);
DeclareSerializable(sstmac::bitwisevaluepayload<long>);
DeclareSerializable(sstmac::bitwisevaluepayload<short>);

DeclareSerializable(sstmac::value_payload<int>);
DeclareSerializable(sstmac::value_payload<bool>);
DeclareSerializable(sstmac::value_payload<long>);
DeclareSerializable(sstmac::value_payload<short>);
DeclareSerializable(sstmac::value_payload<float>);
DeclareSerializable(sstmac::value_payload<double>);
DeclareSerializable(sstmac::value_payload<long long>);
DeclareSerializable(sstmac::value_payload<unsigned long long>);
DeclareSerializable(sstmac::value_payload<unsigned long>);
DeclareSerializable(sstmac::value_payload<unsigned int>);
//DeclareSerializable(sstmac::valuepayload<unsigned short>);

