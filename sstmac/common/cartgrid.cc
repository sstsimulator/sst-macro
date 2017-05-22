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

#include <cmath>
#include <cstdlib>


typedef struct Counter_s {
  int length; //!< number of prime factor counts (cannot exceed 32 for a 32-bit integer)
  int max_counts[32+1]; //!< maximum value for prime factor counts
  int cur_counts[32+1]; //!< current prime factor counts
} Counter;

static void
Counter_new(Counter * this_, int * counts, int length) {
  int i;

  this_->length = length;

  for (i = 0; i < 32; ++i) {
    this_->max_counts[i] = counts[i];
    this_->cur_counts[i] = 0;
  }
  /* terminate with 0's */
  this_->max_counts[i] = this_->cur_counts[i] = 0;
  this_->max_counts[length] = this_->cur_counts[length] = 0;
}

static void
Counter_next(Counter * this_) {
  int i;

  for (i = 0; i < this_->length; ++i) {
    this_->cur_counts[i]++;
    if (this_->cur_counts[i] > this_->max_counts[i]) {
      this_->cur_counts[i] = 0;
      continue;
    }
    break;
  }
}

static int
Counter_is_zero(Counter * this_) {
  int i;
  for (i = 0; i < this_->length; ++i)
    if (this_->cur_counts[i]) return 0;
  return 1;
}

static int
Counter_product(Counter * this_, int * multipliers) {
  int i, j, k=0, x=1;

  for (i = 0; i < this_->length; ++i)
    for (j = 0; j < this_->cur_counts[i]; ++j) {
      k = 1;
      x *= multipliers[i];
    }

  return x * k;
}

static void
Counter_max_cur_sub(Counter * this_, Counter * that, Counter * res) {
  int i;

  res->length = this_->length;
  for (i = 0; i < this_->length; ++i) {
    res->max_counts[i] = this_->max_counts[i] - that->cur_counts[i];
    res->cur_counts[i] = 0;
  }
}

static void
primefactor_i(int x, int * factors) {
  int i, d, sq=(int)(sqrt((double)x))+1L;
  div_t r;

  /* remove 2 as a factor with shifts */
  for (i = 0; x > 1 && (x & 1) == 0; x >>= 1) {
    factors[i++] = 2;
  }

  /* keep removing subsequent odd numbers */
  for (d = 3; d <= sq; d += 2) {
    while (1) {
      r = div(x, d);
      if (r.rem == 0) {
        factors[i++] = d;
        x = r.quot;
        continue;
      }
      break;
    }
  }
  if (x > 1 || i == 0)  /* left with a prime or x==1 */
    factors[i++] = x;

  factors[i] = 0; /* terminate with 0 */
}

void
gen_cart_grid(int n, int& x, int& y, int& z) {
  int i, j, df_cnt;
  int tf1, tf2, tf3;
  int factors[32+1], distinct_factors[32+1], count_factors[32+1];
  Counter c_main, c1, c2;

  /* at the beginning, minimum area is the maximum area */
  double area, min_area = 2.0 * n + 1.0;

  primefactor_i( n, factors ); /* factors are sorted: ascending order */

  if (1 == n || factors[1] == 0) { /* prime number */
    x = n;
    y = 1;
    z = 1;
    return;
  } else if (factors[2] == 0) { /* two prime factors */
    x = factors[0];
    y = factors[1];
    z = 1;
    return;
  } else if (factors[3] == 0) { /* three prime factors */
    x = factors[0];
    y = factors[1];
    z = factors[2];
    return;
  }

  /* we have more than 3 prime factors so we need to try all possible combinations */

  for (j = 0, i = 0; factors[i];) {
    distinct_factors[j++] = factors[i];
    count_factors[j-1] = 0;
    do {
      count_factors[j-1]++;
    } while (distinct_factors[j-1] == factors[++i]);
  }
  df_cnt = j;

  Counter_new( &c_main, count_factors, df_cnt );

  Counter_new( &c1, count_factors, df_cnt );

  for (Counter_next( &c1 ); ! Counter_is_zero( &c1 ); Counter_next( &c1 )) {

    Counter_max_cur_sub( &c_main, &c1, &c2 );
    for (Counter_next( &c2 ); ! Counter_is_zero( &c2 ); Counter_next( &c2 )) {
      tf1 = Counter_product( &c1, distinct_factors );
      tf2 = Counter_product( &c2, distinct_factors );
      tf3 = n / tf1/ tf2;

      area = tf1 * (double)tf2 + tf2 * (double)tf3 + tf1 * (double)tf3;
      if (area < min_area) {
        min_area = area;
        x = tf1;
        y = tf2;
        z = tf3;
      }
    }
  }
}