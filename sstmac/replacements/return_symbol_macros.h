/**
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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

#ifdef sstmac_must_return_free
#define free sstmac_free
#undef sstmac_must_return_free
#endif

#ifdef sstmac_must_return_memset
#undef memset
#define memset sstmac_memset
#undef sstmac_must_return_memset
#endif

#ifdef sstmac_must_return_memcpy
#undef memcpy
#define memcpy sstmac_memcpy
#undef sstmac_must_return_memcpy
#endif

#ifdef sstmac_must_return_gethostname
#undef gethostname
#define gethostname sstmac_gethostname
#undef sstmac_must_return_gethostname
#endif

#ifdef sstmac_must_return_gethostid
#undef gethostid
#define gethostid sstmac_gethostid
#undef sstmac_must_return_gethostid
#endif

#ifdef sstmac_must_return_mutex
#define mutex sstmac_mutex
#undef sstmac_must_return_mutex
#endif

#ifdef sstmac_must_return_getenv
#define getenv sstmac_getenv
#undef sstmac_must_return_getenv
#endif

#ifdef sstmac_must_return_setenv
#define setenv sstmac_setenv
#undef sstmac_must_return_setenv
#endif

#ifdef sstmac_must_return_putenv
#define putenv sstmac_putenv
#undef sstmac_must_return_putenv
#endif

