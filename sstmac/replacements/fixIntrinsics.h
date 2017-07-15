#ifndef sstmac_replacements_fix_intrinsics_h
#define sstmac_replacements_fix_intrinsics_h

/** functions that are static in gcc and not static in clang
    change their names temporarily to get clang to not freak out */
#define __rdtsc __redirect_rdtsc

#define _mm_getcsr __redirect_mm_getcsr

#define _mm_sfence __redirect_mm_sfence

#define _mm_setcsr __redirect__mm_setcsr

#define _mm_clflush __redirect_mm_clflush

#define _mm_lfence __redirect_mm_lfence

#define _mm_pause __redirect_mm_pause

#define _mm_mfence __redirect_mm_mfence

/** these are void return intrinsics that clang doesn't know */
#define __builtin_ia32_storeups(...)

#define __builtin_ia32_movntps(...)

#define __builtin_ia32_storeupd(...)

#define __builtin_ia32_movntpd(...)

#define __builtin_ia32_movntdq(...)

#define __builtin_ia32_storedqu(...)

#define __builtin_ia32_storeupd256(...)

#define __builtin_ia32_storeups256(...)

#define __builtin_ia32_storedqu256(...)

#define __builtin_ia32_movntdq256(...)

#define __builtin_ia32_movntpd256(...)

#define __builtin_ia32_movntps256(...)

/** these are unary operator intrinsics that return the same type thing
    just return the input argument to satisfy clang during src-2-src */
#define __builtin_ia32_cvtps2pd(x) x

#define __builtin_ia32_cvtdq2pd(x) x

#define __builtin_ia32_pmovzxbw128(x) x

#define __builtin_ia32_pmovzxbd128(x) x

#define __builtin_ia32_pmovzxbq128(x) x

#define __builtin_ia32_pmovzxwq128(x) x

#define __builtin_ia32_pmovzxdq128(x) x

#define __builtin_ia32_pmovzxwd128(x) x

#define __builtin_ia32_cvtdq2pd256(x) get256()

#define __builtin_ia32_cvtps2pd256(x) get256d()

#define __builtin_ia32_pmovsxbw256(x) get256i()

#define __builtin_ia32_pmovsxbd256(x) get256i()

#define __builtin_ia32_pmovsxbq256(x) get256i()

#define __builtin_ia32_pmovsxwq256(x) get256i()

#define __builtin_ia32_pmovsxwd256(x) get256i()

#define __builtin_ia32_pmovsxdq256(x) get256i()

#define __builtin_ia32_pmovzxbw256(x) get256i()

#define __builtin_ia32_pmovzxbd256(x) get256i()

#define __builtin_ia32_pmovzxbq256(x) get256i()

#define __builtin_ia32_pmovzxwd256(x) get256i()

#define __builtin_ia32_pmovzxwq256(x) get256i()

#define __builtin_ia32_pmovzxdq256(x) get256i()

#define __builtin_ia32_pandd512_mask(...) get512i()

#define __builtin_ia32_pandd512_mask(...) get512i()

#define __builtin_ia32_pandq512_mask(...) get512i()

#define __builtin_ia32_pandnd512_mask(...) get512i()

#define __builtin_ia32_pandnd512_mask(...) get512i()

#define __builtin_ia32_pandnq512_mask(...) get512i()

#define __builtin_ia32_pord512_mask(...) get512i()

#define __builtin_ia32_porq512_mask(...) get512i()

#define __builtin_ia32_pxord512_mask(...) get512i()

#define __builtin_ia32_pxorq512_mask(...) get512i()

#define __builtin_ia32_paddq512_mask(...) get512i()

#define __builtin_ia32_psubq512_mask(...) get512i()

#define __builtin_ia32_paddd512_mask(...) get512i()

#define __builtin_ia32_psubd512_mask(...) get512i()

#define __builtin_ia32_loaddqudi512_mask(...) get512i()

#define __builtin_ia32_maxss_round(x,...) x

#define __builtin_ia32_maxsd_round(x,...) x

#define __builtin_ia32_minss_round(x,...) x

#define __builtin_ia32_minsd_round(x,...) x

#define __builtin_ia32_pmuldq512_mask(x,...) x

#define __builtin_ia32_pmuludq512_mask(x,...) x

#define __builtin_ia32_rsqrt14ss(x,...) x

#define __builtin_ia32_pmulld512_mask(x,...) x

#define __builtin_ia32_rsqrt14sd(x,...) x

#define __builtin_ia32_rcp14ss(x,...) x

#define __builtin_ia32_addss_round(x,...) x

#define __builtin_ia32_subss_round(x,...) x

#define __builtin_ia32_mulss_round(x,...) x

#define __builtin_ia32_rcp14sd(x,...) x

#define __builtin_ia32_addsd_round_mask(x,...) x

#define __builtin_ia32_subsd_round(x,...) x

#define __builtin_ia32_divsd_round(x,...) x

#define __builtin_ia32_blendmpd_512_mask(x,...) x

#define __builtin_ia32_blendmps_512_mask(x,...) x

#define __builtin_ia32_blendmq_512_mask(x,...) x

#define __builtin_ia32_cvtdq2pd512_mask(...) get512d()

#define __builtin_ia32_addsd_round(x,...) x

#define __builtin_ia32_divss_round(x,...) x

#define __builtin_ia32_mulsd_round(x,...) x

#define __builtin_ia32_blendmd_512_mask(x,...) x

#define __builtin_ia32_cvtudq2pd512_mask(...) get512d()

#define __builtin_ia32_loaddqusi512_mask(...) get512i()

#define __builtin_ia32_loadups512_mask(...) get512i()

#define __builtin_ia32_loadupd512_mask(...) get512d()

#define __builtin_ia32_storeupd512_mask(...)

#define __builtin_ia32_storedqudi512_mask(...) 

#define __builtin_ia32_storedqusi512_mask(...)

#define __builtin_ia32_storeups512_mask(...)

#define __builtin_ia32_paddd256_mask(x,...) x

#define __builtin_ia32_paddq256_mask(x,...) x

#define __builtin_ia32_psubd256_mask(x,...) x

#define __builtin_ia32_psubq256_mask(x,...) x

#define __builtin_ia32_paddd128_mask(x,...) x

#define __builtin_ia32_paddq128_mask(x,...) x

#define __builtin_ia32_psubd128_mask(x,...) x

#define __builtin_ia32_psubq128_mask(x,...) x

#define __builtin_ia32_pmuldq128_mask(x,...) x

#define __builtin_ia32_pmuludq128_mask(x,...) x

#define __builtin_ia32_pmuldq256_mask(x,...) x

#define __builtin_ia32_pmuludq256_mask(x,...) x

#define __builtin_ia32_pmulld256_mask(x,...) x

#define __builtin_ia32_pmulld128_mask(x,...) x

#define __builtin_ia32_pandd256_mask(x,...) x

#define __builtin_ia32_pandnd256_mask(x,...) x

#define __builtin_ia32_pandnd128_mask(x,...) x

#define __builtin_ia32_pord256_mask(x,...) x

#define __builtin_ia32_pord128_mask(x,...) x

#define __builtin_ia32_pandd128_mask(x,...) x

#define __builtin_ia32_pxord256_mask(x,...) x

#define __builtin_ia32_pxord128_mask(x,...) x

#define __builtin_ia32_pandq256_mask(x,...) x

#define __builtin_ia32_pandq128_mask(x,...) x

#define __builtin_ia32_pandnq256_mask(x,...) x

#define __builtin_ia32_pandnq128_mask(x,...) x

#define __builtin_ia32_porq256_mask(x,...) x

#define __builtin_ia32_porq128_mask(x,...) x

#define __builtin_ia32_pxorq256_mask(x,...) x

#define __builtin_ia32_addpd128_mask(x,...) x

#define __builtin_ia32_addps256_mask(x,...) x

#define __builtin_ia32_pxorq128_mask(x,...) x

#define __builtin_ia32_addps128_mask(x,...) x

#define __builtin_ia32_addpd256_mask(x,...) x

#define __builtin_ia32_blendmd_128_mask(x,...) x

#define __builtin_ia32_blendmd_256_mask(x,...) x

#define __builtin_ia32_blendmps_256_mask(x,...) x

#define __builtin_ia32_blendmq_256_mask(x,...) x

#define __builtin_ia32_cvtdq2pd128_mask(x,...) x

#define __builtin_ia32_cvtdq2pd256_mask(x,...) get256d()

#define __builtin_ia32_blendmpd_128_mask(x,...) x

#define __builtin_ia32_blendmpd_256_mask(x,...) x

#define __builtin_ia32_blendmq_128_mask(x,...) x

#define __builtin_ia32_cvtudq2pd128_mask(x,...) x

#define __builtin_ia32_divpd256_mask(x,...) x

#define __builtin_ia32_blendmps_128_mask(x,...) x

#define __builtin_ia32_cvtudq2pd256_mask(...) get256d()

#define __builtin_ia32_divpd_mask(x,...) x

#define __builtin_ia32_divps_mask(x,...) x

#define __builtin_ia32_divps256_mask(x,...) x

#define __builtin_ia32_maxpd_mask(x,...) x

#define __builtin_ia32_maxps_mask(x,...) x

#define __builtin_ia32_maxps256_mask(x,...) x

#define __builtin_ia32_maxpd256_mask(x,...) x

#define __builtin_ia32_minpd256_mask(x,...) x

#define __builtin_ia32_minpd_mask(x,...) x

#define __builtin_ia32_minps_mask(x,...) x

#define __builtin_ia32_minps256_mask(x,...) x

#define __builtin_ia32_mulpd_mask(x,...) x

#define __builtin_ia32_mulpd256_mask(x,...) x

#define __builtin_ia32_mulps_mask(x,...) x

#define __builtin_ia32_mulps256_mask(x,...) x

#define __builtin_ia32_pabsd128_mask(x,...) x

#define __builtin_ia32_pabsd256_mask(x,...) x

#define __builtin_ia32_pmaxsd128_mask(x,...) x

#define __builtin_ia32_pmaxsd256_mask(x,...) x

#define __builtin_ia32_pmaxud128_mask(x,...) x

#define __builtin_ia32_pminsd128_mask(x,...) x

#define __builtin_ia32_pmaxud256_mask(x,...) x

#define __builtin_ia32_pminsd256_mask(x,...) x

#define __builtin_ia32_pminud128_mask(x,...) x

#define __builtin_ia32_pminud256_mask(x,...) x

#define __builtin_ia32_sqrtpd128_mask(x,...) x

#define __builtin_ia32_sqrtpd256_mask(x,...) x

#define __builtin_ia32_sqrtps128_mask(x,...) x

#define __builtin_ia32_subpd128_mask(x,...) x

#define __builtin_ia32_subps128_mask(x,...) x

#define __builtin_ia32_subps256_mask(x,...) x

#define __builtin_ia32_sqrtps256_mask(x,...) x

#define __builtin_ia32_subpd256_mask(x,...) x

#define __builtin_ia32_paddb512_mask(x,...) x

#define __builtin_ia32_packusdw512_mask(x,...) x

#define __builtin_ia32_psubb512_mask(x,...) x

#define __builtin_ia32_paddw512_mask(x,...) x

#define __builtin_ia32_psubw512_mask(x,...) x

#define __builtin_ia32_pmullw512_mask(x,...) x

#define __builtin_ia32_packssdw512_mask(x,...) x

#define __builtin_ia32_packsswb512_mask(x,...) x

#define __builtin_ia32_packusdw512_mask(x,...) x

#define __builtin_ia32_blendmw_512_mask(x,...) x

#define __builtin_ia32_packuswb512_mask(x,...) x

#define __builtin_ia32_pshufb512_mask(x,...) x

#define __builtin_ia32_punpckhbw512_mask(x,...) x

#define __builtin_ia32_punpckhwd512_mask(x,...) x

#define __builtin_ia32_blendmb_512_mask(x,...) x

#define __builtin_ia32_punpcklbw512_mask(x,...) x

#define __builtin_ia32_punpcklwd512_mask(x,...) x

#define __builtin_ia32_pmullq512_mask(x,...) x

#define __builtin_ia32_pmulhw512_mask(x,...) x

#define __builtin_ia32_xorpd512_mask(x,...) x

#define __builtin_ia32_xorps512_mask(x,...) x

#define __builtin_ia32_sqrtps512_mask(x,...) x

#define __builtin_ia32_xorps512_mask(x,...) x

#define __builtin_ia32_orpd512_mask(x,...) x

#define __builtin_ia32_paddw256_mask(x,...) x

#define __builtin_ia32_paddb256_mask(x,...) x

#define __builtin_ia32_orps512_mask(x,...) x

#define __builtin_ia32_andpd512_mask(x,...) x

#define __builtin_ia32_andps512_mask(x,...) x

#define __builtin_ia32_andnpd512_mask(x,...) x

#define __builtin_ia32_andnps512_mask(x,...) x

#define __builtin_ia32_psubb256_mask(x,...) x

#define __builtin_ia32_psubw256_mask(x,...) x

#define __builtin_ia32_paddb128_mask(x,...) x

#define __builtin_ia32_paddb128_mask(x,...) x

#define __builtin_ia32_paddw128_mask(x,...) x

#define __builtin_ia32_blendmb_256_mask(x,...) x

#define __builtin_ia32_blendmw_256_mask(x,...) x

#define __builtin_ia32_pmullw256_mask(x,...) x 

#define __builtin_ia32_psubb128_mask(x,...) x

#define __builtin_ia32_psubsb512_mask(x,...) x

#define __builtin_ia32_psubsw512_mask(x,...) x

#define __builtin_ia32_psubw128_mask(x,...) x

#define __builtin_ia32_psubb128_mask(x,...) x

#define __builtin_ia32_pmullw128_mask(x,...) x

#define __builtin_ia32_blendmb_128_mask(x,...) x

#define __builtin_ia32_blendmw_128_mask(x,...) x

#define __builtin_ia32_pabsb128_mask(x,...) x

#define __builtin_ia32_pabsb256_mask(x,...) x

#define __builtin_ia32_pabsw128_mask(x,...) x

#define __builtin_ia32_pabsq128_mask(x,...) x

#define __builtin_ia32_pabsq256_mask(x,...) x

#define __builtin_ia32_pabsw256_mask(x,...) x

#define __builtin_ia32_packssdw128_mask(x,...) x

#define __builtin_ia32_packssdw256_mask(x,...) x

#define __builtin_ia32_packusdw256_mask(x,...) x

#define __builtin_ia32_packuswb128_mask(x,...) x

#define __builtin_ia32_packsswb128_mask(x,...) x

#define __builtin_ia32_packsswb256_mask(x,...) x

#define __builtin_ia32_packusdw128_mask(x,...) x

#define __builtin_ia32_paddsb128_mask(x,...) x

#define __builtin_ia32_paddsb512_mask(x,...) x

#define __builtin_ia32_paddsb128_mask(x,...) x

#define __builtin_ia32_paddsw128_mask(x,...) x

#define __builtin_ia32_paddsw512_mask(x,...) x

#define __builtin_ia32_packuswb256_mask(x,...) x

#define __builtin_ia32_paddsb256_mask(x,...) x

#define __builtin_ia32_paddsw256_mask(x,...) x

#define __builtin_ia32_paddusb512_mask(x,...) x

#define __builtin_ia32_paddusb128_mask(x,...) x

#define __builtin_ia32_paddusb256_mask(x,...) x

#define __builtin_ia32_paddusb256_mask(x,...) x

#define __builtin_ia32_paddusw512_mask(x,...) x

#define __builtin_ia32_paddusw128_mask(x,...) x

#define __builtin_ia32_paddusw256_mask(x,...) x

#define __builtin_ia32_pavgb128_mask(x,...) x

#define __builtin_ia32_pavgb512_mask(x,...) x

#define __builtin_ia32_pavgb256_mask(x,...) x

#define __builtin_ia32_pavgw512_mask(x,...) x

#define __builtin_ia32_pavgw128_mask(x,...) x

#define __builtin_ia32_pavgw256_mask(x,...) x

#define __builtin_ia32_pmaxsb256_mask(x,...) x

#define __builtin_ia32_pmaxsq256_mask(x,...) x

#define __builtin_ia32_pmaxsw128_mask(x,...) x

#define __builtin_ia32_pmaxsq128_mask(x,...) x

#define __builtin_ia32_pmaxub128_mask(x,...) x

#define __builtin_ia32_pmaxuq128_mask(x,...) x

#define __builtin_ia32_pmaxsb128_mask(x,...) x

#define __builtin_ia32_pmaxsw256_mask(x,...) x

#define __builtin_ia32_pmaxuq256_mask(x,...) x

#define __builtin_ia32_pmaxub256_mask(x,...) x

#define __builtin_ia32_pmaxuw128_mask(x,...) x

#define __builtin_ia32_pmaxuw256_mask(x,...) x

#define __builtin_ia32_pminsb128_mask(x,...) x

#define __builtin_ia32_pminsb256_mask(x,...) x

#define __builtin_ia32_pminsq256_mask(x,...) x

#define __builtin_ia32_pminsw128_mask(x,...) x

#define __builtin_ia32_pminsw128_mask(x,...) x

#define __builtin_ia32_pminsq128_mask(x,...) x

#define __builtin_ia32_pminub128_mask(x,...) x

#define __builtin_ia32_pminub256_mask(x,...) x

#define __builtin_ia32_pminuq256_mask(x,...) x

#define __builtin_ia32_pminub256_mask(x,...) x

#define __builtin_ia32_pminsw256_mask(x,...) x

#define __builtin_ia32_pminuw128_mask(x,...) x

#define __builtin_ia32_pminuq128_mask(x,...) x

#define __builtin_ia32_pminsw256_mask(x,...) x

#define __builtin_ia32_pminuw256_mask(x,...) x

#define __builtin_ia32_psubsb128_mask(x,...) x

#define __builtin_ia32_psubsb256_mask(x,...) x

#define __builtin_ia32_psubsw128_mask(x,...) x

#define __builtin_ia32_pshufb128_mask(x,...) x

#define __builtin_ia32_pshufb256_mask(x,...) x

#define __builtin_ia32_psubusb128_mask(x,...) x

#define __builtin_ia32_psubusb128_mask(x,...) x

#define __builtin_ia32_psubusb256_mask(x,...) x

#define __builtin_ia32_psubusw128_mask(x,...) x

#define __builtin_ia32_psubsw256_mask(x,...) x

#define __builtin_ia32_psubusw256_mask(x,...) x

#define __builtin_ia32_pmaddubsw128_mask(x,...) x

#define __builtin_ia32_pmaddubsw512_mask(x,...) x

#define __builtin_ia32_pmaddwd128_mask(x,...) x

#define __builtin_ia32_pmulhrsw256_mask(x,...) x

#define __builtin_ia32_pmulhuw128_mask(x,...) x

#define __builtin_ia32_pmulhuw512_mask(x,...) x

#define __builtin_ia32_pmaddubsw256_mask(x,...) x

#define __builtin_ia32_pmulhrsw128_mask(x,...) x

#define __builtin_ia32_pmaddwd256_mask(x,...) x

#define __builtin_ia32_pmulhw128_mask(x,...) x

#define __builtin_ia32_pmulhw512_mask(x,...) x

#define __builtin_ia32_pmulhuw256_mask(x,...) x

#define __builtin_ia32_punpckhbw256_mask(x,...) x

#define __builtin_ia32_punpcklwd128_mask(x,...) x

#define __builtin_ia32_punpckhwd128_mask(x,...) x

#define __builtin_ia32_pmulhw256_mask(x,...) x

#define __builtin_ia32_punpckhbw128_mask(x,...) x

#define __builtin_ia32_punpckhwd256_mask(x,...) x

#define __builtin_ia32_punpcklbw128_mask(x,...) x

#define __builtin_ia32_punpcklwd256_mask(x,...) x

#define __builtin_ia32_punpcklbw256_mask(x,...) x

#define __builtin_ia32_andnps256_mask(x,...) x

#define __builtin_ia32_andnps128_mask(x,...) x

#define __builtin_ia32_andps128_mask(x,...) x

#define __builtin_ia32_andps256_mask(x,...) x

#define __builtin_ia32_xorps256_mask(x,...) x

#define __builtin_ia32_xorps128_mask(x,...) x

#define __builtin_ia32_orpd256_mask(x,...) x

#define __builtin_ia32_orpd128_mask(x,...) x

#define __builtin_ia32_xorpd128_mask(x,...) x

#define __builtin_ia32_pmullq128_mask(x,...) x

#define __builtin_ia32_orps256_mask(x,...) x

#define __builtin_ia32_andnpd256_mask(x,...) x

#define __builtin_ia32_xorpd256_mask(x,...) x

#define __builtin_ia32_andpd128_mask(x,...) x

#define __builtin_ia32_pmullq256_mask(x,...) x

#define __builtin_ia32_orps128_mask(x,...) x

#define __builtin_ia32_andnpd128_mask(x,...) x

#define __builtin_ia32_andpd256_mask(x,...) x

#define __builtin_ia32_vpcmov_256(x,...) x

#define __builtin_ia32_vpcmov(x,...) x

typedef float my256 __attribute__((__vector_size__(32)));
typedef double my256d __attribute__((__vector_size__(32)));
typedef double my512d __attribute__((__vector_size__(64)));
typedef long long my256i __attribute__((__vector_size__(32)));
typedef long long my512i __attribute__((__vector_size__(64)));

extern my256 get256();
extern my256d get256d();
extern my256i get256i();
extern my512i get512i();
extern my512d get512d();

#endif
