#ifndef sstmac_replacements_fix_intrinsics_h
#define sstmac_replacements_fix_intrinsics_h

#define __is_aggregate(x) true
#define __has_unique_object_representations(x) true

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

#define __builtin_ia32_pandq512_mask(...) get512i()

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

#define __builtin_ia32_paddw128_mask(x,...) x

#define __builtin_ia32_blendmb_256_mask(x,...) x

#define __builtin_ia32_blendmw_256_mask(x,...) x

#define __builtin_ia32_pmullw256_mask(x,...) x 

#define __builtin_ia32_psubb128_mask(x,...) x

#define __builtin_ia32_psubsb512_mask(x,...) x

#define __builtin_ia32_psubsw512_mask(x,...) x

#define __builtin_ia32_psubw128_mask(x,...) x

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

#define __builtin_ia32_pminsq128_mask(x,...) x

#define __builtin_ia32_pminub128_mask(x,...) x

#define __builtin_ia32_pminuq256_mask(x,...) x

#define __builtin_ia32_pminub256_mask(x,...) x

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


typedef float my64 __attribute__((__vector_size__(8)));
typedef long long my64i __attribute__((__vector_size__(8)));
typedef double my64d __attribute__((__vector_size__(8)));
typedef float my128 __attribute__((__vector_size__(16)));
typedef long long my128i __attribute__((__vector_size__(16)));
typedef double my128d __attribute__((__vector_size__(16)));
typedef float my256 __attribute__((__vector_size__(32)));
typedef double my256d __attribute__((__vector_size__(32)));
typedef long long my256i __attribute__((__vector_size__(32)));
typedef float my512 __attribute__((__vector_size__(64)));
typedef double my512d __attribute__((__vector_size__(64)));
typedef long long my512i __attribute__((__vector_size__(64)));

typedef int mymask8;
typedef int mymask16;
typedef int mymask32;
typedef int mymask64;

extern unsigned short getunsigned_short();
extern long long getlong_long();
extern int getint();
extern my64 get64();
extern my64i get64i();
extern my64d get64d();
extern my128 get128();
extern my128i get128i();
extern my128d get128d();
extern my256 get256();
extern my256d get256d();
extern my256i get256i();
extern my512 get512();
extern my512i get512i();
extern my512d get512d();
extern mymask8 getmask8();
extern mymask16 getmask16();
extern mymask32 getmask32();
extern mymask64 getmask64();

#define __builtin_ia32_andss(x,...) x
#define __builtin_ia32_addss(x,...) x
#define __builtin_ia32_subss(x,...) x
#define __builtin_ia32_mulss(x,...) x
#define __builtin_ia32_divss(x,...) x
#define __builtin_ia32_xorss(x,...) x
#define __builtin_ia32_andps(x,...) x
#define __builtin_ia32_addps(x,...) x
#define __builtin_ia32_subps(x,...) x
#define __builtin_ia32_mulps(x,...) x
#define __builtin_ia32_divps(x,...) x
#define __builtin_ia32_xorps(x,...) x

#define __builtin_ia32_bsrsi(x,...) x
#define __builtin_ia32_rolqi(x,...) x
#define __builtin_ia32_rolhi(x,...) x
#define __builtin_ia32_rorqi(x,...) x
#define __builtin_ia32_rorhi(x,...) x
#define __builtin_ia32_bsrdi(x,...) x
#define __builtin_ia32_andnps(x,...) x
#define __builtin_ia32_orps(x,...) x
#define __builtin_ia32_movss(x,...) x
#define __builtin_ia32_cmpgtps(x,...) x
#define __builtin_ia32_cmpltps(x,...) x
#define __builtin_ia32_cmpgeps(x,...) x
#define __builtin_ia32_cmpleps(x,...) x
#define __builtin_ia32_cmpngeps(x,...) x
#define __builtin_ia32_cvtsi2ss(x,...) x
#define __builtin_ia32_cvtsi642ss(x,...) x



#define __builtin_ia32_movlhps(x,...) x
#define __builtin_ia32_movhlps(x,...) x
#define __builtin_ia32_shufps(x,...) x
#define __builtin_ia32_unpckhps(x,...) x
#define __builtin_ia32_unpcklps(x,...) x
#define __builtin_ia32_loadhps(x,...) x
#define __builtin_ia32_loadlps(x,...) x

#define __builtin_ia32_cmpngtps(x,...) x
#define __builtin_ia32_movsd(x,...) x
#define __builtin_ia32_shufpd(x,...) x
#define __builtin_ia32_vec_ext_v2di(...) getlong_long()
#define __builtin_ia32_addpd(x,...) x
#define __builtin_ia32_addsd(x,...) x

#define __builtin_ia32_subpd(...) get128d()
#define __builtin_ia32_subsd(...) get128d()
#define __builtin_ia32_mulpd(...) get128d()
#define __builtin_ia32_mulsd(...) get128d()
#define __builtin_ia32_divpd(...) get128d()
#define __builtin_ia32_divsd(...) get128d()
#define __builtin_ia32_sqrtpd(...) get128d()
#define __builtin_ia32_sqrtsd(...) get128d()
#define __builtin_ia32_minpd(...) get128d()
#define __builtin_ia32_minsd(...) get128d()
#define __builtin_ia32_maxpd(...) get128d()
#define __builtin_ia32_maxsd(...) get128d()
#define __builtin_ia32_andpd(...) get128d()
#define __builtin_ia32_andnpd(...) get128d()
#define __builtin_ia32_orpd(...) get128d()
#define __builtin_ia32_xorpd(...) get128d()
#define __builtin_ia32_cmpeqpd(...) get128d()
#define __builtin_ia32_cmpltpd(...) get128d()
#define __builtin_ia32_cmplepd(...) get128d()
#define __builtin_ia32_cmpgtpd(...) get128d()
#define __builtin_ia32_cmpgepd(...) get128d()
#define __builtin_ia32_cmpneqpd(...) get128d()
#define __builtin_ia32_cmpnltpd(...) get128d()
#define __builtin_ia32_cmpnlepd(...) get128d()
#define __builtin_ia32_cmpngtpd(...) get128d()
#define __builtin_ia32_cmpngepd(...) get128d()
#define __builtin_ia32_cmpordpd(...) get128d()
#define __builtin_ia32_cmpunordpd(...) get128d()
#define __builtin_ia32_cmpeqsd(...) get128d()
#define __builtin_ia32_cmpltsd(...) get128d()
#define __builtin_ia32_cmplesd(...) get128d()
#define __builtin_ia32_cmpneqsd(...) get128d()
#define __builtin_ia32_cmpnltsd(...) get128d()
#define __builtin_ia32_cmpnlesd(...) get128d()
#define __builtin_ia32_cmpordsd(...) get128d()
#define __builtin_ia32_cmpunordsd(...) get128d()
#define __builtin_ia32_loaddqu(...) get128i()
#define __builtin_ia32_movq128(...) get128i()
#define __builtin_ia32_cvtdq2ps(...) get128()
#define __builtin_ia32_cvtpd2dq(...) get128i()
#define __builtin_ia32_cvtpd2pi(...) get64()
#define __builtin_ia32_cvtpd2ps(...) get128()
#define __builtin_ia32_cvttpd2dq(...) get128i()
#define __builtin_ia32_cvttpd2pi(...) get64()
#define __builtin_ia32_cvtpi2pd(...) get128d()
#define __builtin_ia32_cvtps2dq(...) get128i()
#define __builtin_ia32_cvttps2dq(...) get128i()
#define __builtin_ia32_cvtsd2ss(...) get128()
#define __builtin_ia32_cvtsi2sd(...) get128d()
#define __builtin_ia32_cvtsi642sd(...) get128d()
#define __builtin_ia32_cvtss2sd(...) get128d()
#define __builtin_ia32_unpckhpd(...) get128d()
#define __builtin_ia32_unpcklpd(...) get128d()
#define __builtin_ia32_loadhpd(...) get128d()
#define __builtin_ia32_loadlpd(...) get128d()
#define __builtin_ia32_packsswb128(...) get128i()
#define __builtin_ia32_packssdw128(...) get128i()
#define __builtin_ia32_packuswb128(...) get128i()
#define __builtin_ia32_punpckhbw128(...) get128i()
#define __builtin_ia32_punpckhwd128(...) get128i()
#define __builtin_ia32_punpckhdq128(...) get128i()
#define __builtin_ia32_punpckhqdq128(...) get128i()
#define __builtin_ia32_punpcklbw128(...) get128i()
#define __builtin_ia32_punpcklwd128(...) get128i()
#define __builtin_ia32_punpckldq128(...) get128i()
#define __builtin_ia32_punpcklqdq128(...) get128i()
#define __builtin_ia32_paddb128(...) get128i()
#define __builtin_ia32_paddw128(...) get128i()
#define __builtin_ia32_paddd128(...) get128i()
#define __builtin_ia32_paddq128(...) get128i()
#define __builtin_ia32_paddsb128(...) get128i()
#define __builtin_ia32_paddsw128(...) get128i()
#define __builtin_ia32_paddusb128(...) get128i()
#define __builtin_ia32_paddusw128(...) get128i()
#define __builtin_ia32_psubb128(...) get128i()
#define __builtin_ia32_psubw128(...) get128i()
#define __builtin_ia32_psubd128(...) get128i()
#define __builtin_ia32_psubq128(...) get128i()
#define __builtin_ia32_psubsb128(...) get128i()
#define __builtin_ia32_psubsw128(...) get128i()
#define __builtin_ia32_psubusb128(...) get128i()
#define __builtin_ia32_psubusw128(...) get128i()
#define __builtin_ia32_pmaddwd128(...) get128i()
#define __builtin_ia32_pmulhw128(...) get128i()
#define __builtin_ia32_pmullw128(...) get128i()
#define __builtin_ia32_pmuludq(...) get64()
#define __builtin_ia32_pmuludq128(...) get128i()
#define __builtin_ia32_psllwi128(...) get128i()
#define __builtin_ia32_pslldi128(...) get128i()
#define __builtin_ia32_psllqi128(...) get128i()
#define __builtin_ia32_psrawi128(...) get128i()
#define __builtin_ia32_psradi128(...) get128i()
#define __builtin_ia32_psrldqi128(...) get128i()
#define __builtin_ia32_pslldqi128(...) get128i()
#define __builtin_ia32_psrlwi128(...) get128i()
#define __builtin_ia32_psrlqi128(...) get128i()
#define __builtin_ia32_psllw128(...) get128i()
#define __builtin_ia32_pslld128(...) get128i()
#define __builtin_ia32_psllq128(...) get128i()
#define __builtin_ia32_psraw128(...) get128i()
#define __builtin_ia32_psrad128(...) get128i()
#define __builtin_ia32_psrlw128(...) get128i()
#define __builtin_ia32_psrld128(...) get128i()
#define __builtin_ia32_psrlq128(...) get128i()
#define __builtin_ia32_pand128(...) get128i()
#define __builtin_ia32_pandn128(...) get128i()
#define __builtin_ia32_por128(...) get128i()
#define __builtin_ia32_pxor128(...) get128i()
#define __builtin_ia32_pcmpeqb128(...) get128i()
#define __builtin_ia32_pcmpeqw128(...) get128i()
#define __builtin_ia32_pcmpeqd128(...) get128i()
#define __builtin_ia32_pcmpgtb128(...) get128i()
#define __builtin_ia32_pcmpgtw128(...) get128i()
#define __builtin_ia32_pcmpgtd128(...) get128i()
#define __builtin_ia32_vec_ext_v8hi(...) getunsigned_short()
#define __builtin_ia32_vec_set_v8hi(...) get128i()
#define __builtin_ia32_pmaxsw128(...) get128i()
#define __builtin_ia32_pmaxub128(...) get128i()
#define __builtin_ia32_pminsw128(...) get128i()
#define __builtin_ia32_pminub128(...) get128i()
#define __builtin_ia32_pmulhuw128(...) get128i()
#define __builtin_ia32_pshufhw(...) get128i()
#define __builtin_ia32_pshuflw(...) get128i()
#define __builtin_ia32_pshufd(...) get128i()
#define __builtin_ia32_pavgb128(...) get128i()
#define __builtin_ia32_pavgw128(...) get128i()
#define __builtin_ia32_psadbw128(...) get128i()
#define __builtin_ia32_storeupd(...)
#define __builtin_ia32_storedqu(...)
#define __builtin_ia32_maskmovdqu(...)
#define __builtin_ia32_movnti(...)
#define __builtin_ia32_movnti64(...)
#define __builtin_ia32_movntdq(...)
#define __builtin_ia32_movntpd(...)
#define __builtin_ia32_clflush(...)
#define __builtin_ia32_lfence(...)
#define __builtin_ia32_mfence(...)

#define __builtin_ia32_sqrtss(...) get128()
#define __builtin_ia32_rcpss(...) get128()
#define __builtin_ia32_rsqrtss(...) get128()
#define __builtin_ia32_minss(...) get128()
#define __builtin_ia32_maxss(...) get128()
#define __builtin_ia32_sqrtps(...) get128()
#define __builtin_ia32_rcpps(...) get128()
#define __builtin_ia32_rsqrtps(...) get128()
#define __builtin_ia32_minps(...) get128()
#define __builtin_ia32_maxps(...) get128()
#define __builtin_ia32_cmpeqss(...) get128()
#define __builtin_ia32_cmpltss(...) get128()
#define __builtin_ia32_cmpless(...) get128()
#define __builtin_ia32_cmpneqss(...) get128()
#define __builtin_ia32_cmpnltss(...) get128()
#define __builtin_ia32_cmpnless(...) get128()
#define __builtin_ia32_cmpordss(...) get128()
#define __builtin_ia32_cmpunordss(...) get128()
#define __builtin_ia32_cmpeqps(...) get128()
#define __builtin_ia32_cmpneqps(...) get128()
#define __builtin_ia32_cmpnltps(...) get128()
#define __builtin_ia32_cmpnleps(...) get128()
#define __builtin_ia32_cmpordps(...) get128()
#define __builtin_ia32_cmpunordps(...) get128()
#define __builtin_ia32_cvtps2pi(...) get64()
#define __builtin_ia32_cvttps2pi(...) get64()
#define __builtin_ia32_cvtpi2ps(...) get128()
#define __builtin_ia32_packssdw(...) get64()
#define __builtin_ia32_packsswb(...) get64()
#define __builtin_ia32_vec_set_v4hi(...) get64()
#define __builtin_ia32_pmaxsw(...) get64()
#define __builtin_ia32_pmaxub(...) get64()
#define __builtin_ia32_pminsw(...) get64()
#define __builtin_ia32_pminub(...) get64()
#define __builtin_ia32_pmulhuw(...) get64()
#define __builtin_ia32_pshufw(...) get64()
#define __builtin_ia32_pavgb(...) get64()
#define __builtin_ia32_pavgw(...) get64()
#define __builtin_ia32_psadbw(...) get64()
#define __builtin_ia32_storehps(...)
#define __builtin_ia32_storelps(...)
#define __builtin_ia32_ldmxcsr(...)
#define __builtin_ia32_maskmovq(...)
#define __builtin_prefetch(...)
#define __builtin_ia32_movntq(...)
#define __builtin_ia32_sfence(...)
#define __builtin_ia32_pause(...)


#define __builtin_ia32_vec_ext_v4hi(...) 0
#define __builtin_ia32_vec_ext_v4sf(...) 0
#define __builtin_ia32_vec_ext_v4si(...) 0

#define __builtin_ia32_loadups(...) get128()
#define __builtin_ia32_loadupd(...) get128d()

#define __builtin_ia32_vec_ext_v2df(...) 0

#define __builtin_ia32_psrldi128(...) get128i()

#define __builtin_ia32_vec_init_v2si(...) get64()
#define __builtin_ia32_packuswb(...) get64()
#define __builtin_ia32_punpckhbw(...) get64()
#define __builtin_ia32_punpckhwd(...) get64()
#define __builtin_ia32_punpckhdq(...) get64()
#define __builtin_ia32_punpcklbw(...) get64()
#define __builtin_ia32_punpcklwd(...) get64()
#define __builtin_ia32_punpckldq(...) get64()
#define __builtin_ia32_paddb(...) get64()
#define __builtin_ia32_paddw(...) get64()
#define __builtin_ia32_paddd(...) get64()
#define __builtin_ia32_paddq(...) get64()
#define __builtin_ia32_paddsb(...) get64()
#define __builtin_ia32_paddsw(...) get64()
#define __builtin_ia32_paddusb(...) get64()
#define __builtin_ia32_paddusw(...) get64()
#define __builtin_ia32_psubb(...) get64()
#define __builtin_ia32_psubw(...) get64()
#define __builtin_ia32_psubd(...) get64()
#define __builtin_ia32_psubq(...) get64()
#define __builtin_ia32_psubsb(...) get64()
#define __builtin_ia32_psubsw(...) get64()
#define __builtin_ia32_psubusb(...) get64()
#define __builtin_ia32_psubusw(...) get64()
#define __builtin_ia32_pmaddwd(...) get64()
#define __builtin_ia32_pmulhw(...) get64()
#define __builtin_ia32_pmullw(...) get64()
#define __builtin_ia32_psllw(...) get64()
#define __builtin_ia32_psllwi(...) get64()
#define __builtin_ia32_pslld(...) get64()
#define __builtin_ia32_pslldi(...) get64()
#define __builtin_ia32_psllq(...) get64()
#define __builtin_ia32_psllqi(...) get64()
#define __builtin_ia32_psraw(...) get64()
#define __builtin_ia32_psrawi(...) get64()
#define __builtin_ia32_psrad(...) get64()
#define __builtin_ia32_psradi(...) get64()
#define __builtin_ia32_psrlw(...) get64()
#define __builtin_ia32_psrlwi(...) get64()
#define __builtin_ia32_psrld(...) get64()
#define __builtin_ia32_psrldi(...) get64()
#define __builtin_ia32_psrlq(...) get64()
#define __builtin_ia32_psrlqi(...) get64()
#define __builtin_ia32_pcmpeqb(...) get64()
#define __builtin_ia32_pcmpgtb(...) get64()
#define __builtin_ia32_pcmpeqw(...) get64()
#define __builtin_ia32_pcmpgtw(...) get64()
#define __builtin_ia32_pcmpeqd(...) get64()
#define __builtin_ia32_pcmpgtd(...) get64()
#define __builtin_ia32_vec_init_v4hi(...) get64()
#define __builtin_ia32_vec_init_v8qi(...) get64()
#define __builtin_ia32_emms(...)

#define __builtin_ia32_movshdup(x) x
#define __builtin_ia32_movsldup(x) x
#define __builtin_ia32_monitor(...)

#define __builtin_ia32_vfmaddsd(...) get128d()
#define __builtin_ia32_vfmsubpd(...) get128d()
#define __builtin_ia32_vfmsubsd(...) get128d()
#define __builtin_ia32_vfnmaddpd(...) get128d()
#define __builtin_ia32_vfnmaddsd(...) get128d()
#define __builtin_ia32_vfnmsubpd(...) get128d()
#define __builtin_ia32_vfnmsubsd(...) get128d()
#define __builtin_ia32_vfmsubaddpd(...) get128d()
#define __builtin_ia32_vfmsubpd256(...) get256d()
#define __builtin_ia32_vfnmaddpd256(...) get256d()
#define __builtin_ia32_vfnmsubpd256(...) get256d()
#define __builtin_ia32_vfmsubaddpd256(...) get256d()

#define __builtin_ia32_undef512(...) get512()
#define __builtin_ia32_pbroadcastd512_gpr_mask(...) get512i()
#define __builtin_ia32_pbroadcastq512_gpr_mask(...) get512i()
#define __builtin_ia32_pbroadcastq512_mem_mask(...) get512i()
#define __builtin_ia32_maxpd512_mask(...) get512d()
#define __builtin_ia32_maxps512_mask(...) get512()
#define __builtin_ia32_pmaxsd512_mask(...) get512i()
#define __builtin_ia32_pmaxud512_mask(...) get512i()
#define __builtin_ia32_pmaxsq512_mask(...) get512i()
#define __builtin_ia32_pmaxuq512_mask(...) get512i()
#define __builtin_ia32_minpd512_mask(...) get512d()
#define __builtin_ia32_minps512_mask(...) get512()
#define __builtin_ia32_pminsd512_mask(...) get512i()
#define __builtin_ia32_pminud512_mask(...) get512i()
#define __builtin_ia32_pminsq512_mask(...) get512i()
#define __builtin_ia32_pminuq512_mask(...) get512i()
#define __builtin_ia32_sqrtpd512_mask(...) get512d()
#define __builtin_ia32_rsqrt14pd512_mask(...) get512d()
#define __builtin_ia32_rsqrt14ps512_mask(...) get512()
#define __builtin_ia32_rcp14pd512_mask(...) get512d()
#define __builtin_ia32_rcp14ps512_mask(...) get512()
#define __builtin_ia32_rndscaleps_mask(...) get512()
#define __builtin_ia32_rndscalepd_mask(...) get512d()
#define __builtin_ia32_pabsq512_mask(...) get512i()
#define __builtin_ia32_pabsd512_mask(...) get512i()
#define __builtin_ia32_addpd512_mask(...) get512d()
#define __builtin_ia32_addps512_mask(...) get512()
#define __builtin_ia32_subpd512_mask(...) get512d()
#define __builtin_ia32_subps512_mask(...) get512()
#define __builtin_ia32_mulpd512_mask(...) get512d()
#define __builtin_ia32_mulps512_mask(...) get512()
#define __builtin_ia32_divpd512_mask(...) get512d()
#define __builtin_ia32_divps512_mask(...) get512()
#define __builtin_ia32_vfmaddpd512_mask(...) get512d()
#define __builtin_ia32_vfmaddpd512_mask3(...) get512d()
#define __builtin_ia32_vfmaddpd512_maskz(...) get512d()
#define __builtin_ia32_vfmaddps512_mask(...) get512()
#define __builtin_ia32_vfmaddps512_mask3(...) get512()
#define __builtin_ia32_vfmaddps512_maskz(...) get512()
#define __builtin_ia32_vfmaddsubpd512_mask(...) get512d()
#define __builtin_ia32_vfmaddsubpd512_mask3(...) get512d()
#define __builtin_ia32_vfmaddsubpd512_maskz(...) get512d()
#define __builtin_ia32_vfmaddsubps512_mask(...) get512()
#define __builtin_ia32_vfmaddsubps512_mask3(...) get512()
#define __builtin_ia32_vfmaddsubps512_maskz(...) get512()
#define __builtin_ia32_vfmsubpd512_mask3(...) get512d()
#define __builtin_ia32_vfmsubps512_mask3(...) get512()
#define __builtin_ia32_vfmsubaddpd512_mask3(...) get512d()
#define __builtin_ia32_vfmsubaddps512_mask3(...) get512()
#define __builtin_ia32_vfnmaddpd512_mask(...) get512d()
#define __builtin_ia32_vfnmaddps512_mask(...) get512()
#define __builtin_ia32_vfnmsubpd512_mask(...) get512d()
#define __builtin_ia32_vfnmsubpd512_mask3(...) get512d()
#define __builtin_ia32_vfnmsubps512_mask(...) get512()
#define __builtin_ia32_vfnmsubps512_mask3(...) get512()
#define __builtin_ia32_vpermt2vard512_mask(...) get512i()
#define __builtin_ia32_vpermt2varq512_mask(...) get512i()
#define __builtin_ia32_vpermt2varpd512_mask(...) get512d()
#define __builtin_ia32_vpermt2varps512_mask(...) get512()
#define __builtin_ia32_alignq512_mask(...) get512i()
#define __builtin_ia32_alignd512_mask(...) get512i()
#define __builtin_ia32_cmpps512_mask(...) getmask16()
#define __builtin_ia32_cmppd512_mask(...) getmask8()
#define __builtin_ia32_cvttps2udq512_mask(...) get512i()
#define __builtin_ia32_cvtdq2ps512_mask(...) get512()
#define __builtin_ia32_cvtudq2ps512_mask(...) get512()
#define __builtin_ia32_cvtpd2ps512_mask(...) get256()
#define __builtin_ia32_vcvtps2ph512_mask(...) get256i()
#define __builtin_ia32_vcvtph2ps512_mask(...) get512()
#define __builtin_ia32_cvttps2dq512_mask(...) get512i()
#define __builtin_ia32_cvttpd2dq512_mask(...) get256i()
#define __builtin_ia32_cvtps2dq512_mask(...) get512i()
#define __builtin_ia32_cvtpd2dq512_mask(...) get256i()
#define __builtin_ia32_cvtps2udq512_mask(...) get512i()
#define __builtin_ia32_cvtpd2udq512_mask(...) get256i()
#define __builtin_ia32_ptestmd512(...) getmask16()
#define __builtin_ia32_ptestmq512(...) getmask8()
#define __builtin_ia32_loadaps512_mask(...) get512()
#define __builtin_ia32_loadapd512_mask(...) get512d()
#define __builtin_ia32_pcmpeqd512_mask(...) getmask16()
#define __builtin_ia32_ucmpd512_mask(...) getmask16()
#define __builtin_ia32_pcmpeqq512_mask(...) getmask8()
#define __builtin_ia32_ucmpq512_mask(...) getmask8()
#define __builtin_ia32_cmpd512_mask(...) getmask16()
#define __builtin_ia32_cmpq512_mask(...) getmask8()
#define __builtin_ia32_pcmpgtd512_mask(...) getmask16()
#define __builtin_ia32_pcmpgtq512_mask(...) getmask8()

#define __builtin_ia32_pcmpeqd128_mask(...) getmask8()
#define __builtin_ia32_ucmpd128_mask(...) getmask8()
#define __builtin_ia32_pcmpeqd256_mask(...) getmask8()
#define __builtin_ia32_ucmpd256_mask(...) getmask8()
#define __builtin_ia32_pcmpeqq128_mask(...) getmask8()
#define __builtin_ia32_ucmpq128_mask(...) getmask8()
#define __builtin_ia32_pcmpeqq256_mask(...) getmask8()
#define __builtin_ia32_ucmpq256_mask(...) getmask8()
#define __builtin_ia32_cmpd128_mask(...) getmask8()
#define __builtin_ia32_cmpd256_mask(...) getmask8()
#define __builtin_ia32_cmpq128_mask(...) getmask8()
#define __builtin_ia32_cmpq256_mask(...) getmask8()
#define __builtin_ia32_pcmpgtd128_mask(...) getmask8()
#define __builtin_ia32_pcmpgtd256_mask(...) getmask8()
#define __builtin_ia32_pcmpgtq128_mask(...) getmask8()
#define __builtin_ia32_pcmpgtq256_mask(...) getmask8()
#define __builtin_ia32_cmpps256_mask(...) getmask8()
#define __builtin_ia32_cmppd256_mask(...) getmask8()
#define __builtin_ia32_cmpps128_mask(...) getmask8()
#define __builtin_ia32_cmppd128_mask(...) getmask8()
#define __builtin_ia32_vfmaddpd128_mask(...) get128d()
#define __builtin_ia32_vfmaddpd128_mask3(...) get128d()
#define __builtin_ia32_vfmaddpd128_maskz(...) get128d()
#define __builtin_ia32_vfmaddpd256_mask(...) get256d()
#define __builtin_ia32_vfmaddpd256_mask3(...) get256d()
#define __builtin_ia32_vfmaddpd256_maskz(...) get256d()
#define __builtin_ia32_vfmaddps128_mask(...) get128()
#define __builtin_ia32_vfmaddps128_mask3(...) get128()
#define __builtin_ia32_vfmaddps128_maskz(...) get128()
#define __builtin_ia32_vfmaddps256_mask(...) get256()
#define __builtin_ia32_vfmaddps256_mask3(...) get256()
#define __builtin_ia32_vfmaddps256_maskz(...) get256()
#define __builtin_ia32_vfmaddsubpd128_mask(...) get128d()
#define __builtin_ia32_vfmaddsubpd128_mask3(...) get128d()
#define __builtin_ia32_vfmaddsubpd128_maskz(...) get128d()
#define __builtin_ia32_vfmaddsubpd256_mask(...) get256d()
#define __builtin_ia32_vfmaddsubpd256_mask3(...) get256d()
#define __builtin_ia32_vfmaddsubpd256_maskz(...) get256d()
#define __builtin_ia32_vfmaddsubps128_mask(...) get128()
#define __builtin_ia32_vfmaddsubps128_mask3(...) get128()
#define __builtin_ia32_vfmaddsubps128_maskz(...) get128()
#define __builtin_ia32_vfmaddsubps256_mask(...) get256()
#define __builtin_ia32_vfmaddsubps256_mask3(...) get256()
#define __builtin_ia32_vfmaddsubps256_maskz(...) get256()
#define __builtin_ia32_vfmsubpd128_mask3(...) get128d()
#define __builtin_ia32_vfmsubpd256_mask3(...) get256d()
#define __builtin_ia32_vfmsubps128_mask3(...) get128()
#define __builtin_ia32_vfmsubps256_mask3(...) get256()
#define __builtin_ia32_vfmsubaddpd128_mask3(...) get128d()
#define __builtin_ia32_vfmsubaddpd256_mask3(...) get256d()
#define __builtin_ia32_vfmsubaddps128_mask3(...) get128()
#define __builtin_ia32_vfmsubaddps256_mask3(...) get256()
#define __builtin_ia32_vfnmaddpd128_mask(...) get128d()
#define __builtin_ia32_vfnmaddpd256_mask(...) get256d()
#define __builtin_ia32_vfnmaddps128_mask(...) get128()
#define __builtin_ia32_vfnmaddps256_mask(...) get256()
#define __builtin_ia32_vfnmsubpd128_mask(...) get128d()
#define __builtin_ia32_vfnmsubpd128_mask3(...) get128d()
#define __builtin_ia32_vfnmsubpd256_mask(...) get256d()
#define __builtin_ia32_vfnmsubpd256_mask3(...) get256d()
#define __builtin_ia32_vfnmsubps128_mask(...) get128()
#define __builtin_ia32_vfnmsubps128_mask3(...) get128()
#define __builtin_ia32_vfnmsubps256_mask(...) get256()
#define __builtin_ia32_vfnmsubps256_mask3(...) get256()
#define __builtin_ia32_compressdf128_mask(...) get128d()
#define __builtin_ia32_compressdf256_mask(...) get256d()
#define __builtin_ia32_compressdi128_mask(...) get128i()
#define __builtin_ia32_compressdi256_mask(...) get256i()
#define __builtin_ia32_compresssf128_mask(...) get128()
#define __builtin_ia32_compresssf256_mask(...) get256()
#define __builtin_ia32_compresssi128_mask(...) get128i()
#define __builtin_ia32_compresssi256_mask(...) get256i()
#define __builtin_ia32_cvtdq2ps128_mask(...) get128()
#define __builtin_ia32_cvtdq2ps256_mask(...) get256()
#define __builtin_ia32_cvtpd2dq128_mask(...) get128i()
#define __builtin_ia32_cvtpd2dq256_mask(...) get128i()
#define __builtin_ia32_cvtpd2ps_mask(...) get128()
#define __builtin_ia32_cvtpd2ps256_mask(...) get128()
#define __builtin_ia32_cvtpd2udq128_mask(...) get128i()
#define __builtin_ia32_cvtpd2udq256_mask(...) get128i()
#define __builtin_ia32_cvtps2dq128_mask(...) get128i()
#define __builtin_ia32_cvtps2dq256_mask(...) get256i()
#define __builtin_ia32_cvtps2pd128_mask(...) get128d()
#define __builtin_ia32_cvtps2pd256_mask(...) get256d()
#define __builtin_ia32_cvtps2udq128_mask(...) get128i()
#define __builtin_ia32_cvtps2udq256_mask(...) get256i()
#define __builtin_ia32_cvttpd2dq128_mask(...) get128i()
#define __builtin_ia32_cvttpd2dq256_mask(...) get128i()
#define __builtin_ia32_cvttpd2udq128_mask(...) get128i()
#define __builtin_ia32_cvttpd2udq256_mask(...) get128i()
#define __builtin_ia32_cvttps2dq128_mask(...) get128i()
#define __builtin_ia32_cvttps2dq256_mask(...) get256i()
#define __builtin_ia32_cvttps2udq128_mask(...) get128i()
#define __builtin_ia32_cvttps2udq256_mask(...) get256i()
#define __builtin_ia32_cvtudq2ps128_mask(...) get128()
#define __builtin_ia32_cvtudq2ps256_mask(...) get256()
#define __builtin_ia32_expanddf128_mask(...) get128d()
#define __builtin_ia32_expanddf256_mask(...) get256d()
#define __builtin_ia32_expanddi128_mask(...) get128i()
#define __builtin_ia32_expanddi256_mask(...) get256i()
#define __builtin_ia32_expandloaddf128_mask(...) get128d()
#define __builtin_ia32_expandloaddf256_mask(...) get256d()
#define __builtin_ia32_expandloaddi128_mask(...) get128i()
#define __builtin_ia32_expandloaddi256_mask(...) get256i()
#define __builtin_ia32_expandloadsf128_mask(...) get128()
#define __builtin_ia32_expandloadsf256_mask(...) get256()
#define __builtin_ia32_expandloadsi128_mask(...) get128i()
#define __builtin_ia32_expandloadsi256_mask(...) get256i()
#define __builtin_ia32_expandsf128_mask(...) get128()
#define __builtin_ia32_expandsf256_mask(...) get256()
#define __builtin_ia32_expandsi128_mask(...) get128i()
#define __builtin_ia32_expandsi256_mask(...) get256i()
#define __builtin_ia32_getexppd128_mask(...) get128d()
#define __builtin_ia32_getexppd256_mask(...) get256d()
#define __builtin_ia32_getexpps128_mask(...) get128()
#define __builtin_ia32_getexpps256_mask(...) get256()
#define __builtin_ia32_rndscalepd_128_mask(...) get128d()
#define __builtin_ia32_rndscalepd_256_mask(...) get256d()
#define __builtin_ia32_rndscaleps_128_mask(...) get128()
#define __builtin_ia32_rndscaleps_256_mask(...) get256()
#define __builtin_ia32_scalefpd128_mask(...) get128d()
#define __builtin_ia32_scalefpd256_mask(...) get256d()
#define __builtin_ia32_scalefps128_mask(...) get128()
#define __builtin_ia32_scalefps256_mask(...) get256()
#define __builtin_ia32_vpermi2vard128_mask(...) get128i()
#define __builtin_ia32_vpermi2vard256_mask(...) get256i()
#define __builtin_ia32_vpermi2varpd128_mask(...) get128d()
#define __builtin_ia32_vpermi2varpd256_mask(...) get256d()
#define __builtin_ia32_vpermi2varps128_mask(...) get128()
#define __builtin_ia32_vpermi2varps256_mask(...) get256()
#define __builtin_ia32_vpermi2varq128_mask(...) get128i()
#define __builtin_ia32_vpermi2varq256_mask(...) get256i()
#define __builtin_ia32_vpermt2vard128_mask(...) get128i()
#define __builtin_ia32_vpermt2vard128_maskz(...) get128i()
#define __builtin_ia32_vpermt2vard256_mask(...) get256i()
#define __builtin_ia32_vpermt2vard256_maskz(...) get256i()
#define __builtin_ia32_vpermt2varpd128_mask(...) get128d()
#define __builtin_ia32_vpermt2varpd128_maskz(...) get128d()
#define __builtin_ia32_vpermt2varpd256_mask(...) get256d()
#define __builtin_ia32_vpermt2varpd256_maskz(...) get256d()
#define __builtin_ia32_vpermt2varps128_mask(...) get128()
#define __builtin_ia32_vpermt2varps128_maskz(...) get128()
#define __builtin_ia32_vpermt2varps256_mask(...) get256()
#define __builtin_ia32_vpermt2varps256_maskz(...) get256()
#define __builtin_ia32_vpermt2varq128_mask(...) get128i()
#define __builtin_ia32_vpermt2varq128_maskz(...) get128i()
#define __builtin_ia32_vpermt2varq256_mask(...) get256i()
#define __builtin_ia32_vpermt2varq256_maskz(...) get256i()

#define __builtin_ia32_pcmpeqb512_mask(...) getmask64()
#define __builtin_ia32_ucmpb512_mask(...) getmask64()
#define __builtin_ia32_pcmpeqw512_mask(...) getmask32()
#define __builtin_ia32_ucmpw512_mask(...) getmask32()
#define __builtin_ia32_cmpb512_mask(...) getmask64()
#define __builtin_ia32_cmpw512_mask(...) getmask32()
#define __builtin_ia32_pcmpgtb512_mask(...) getmask64()
#define __builtin_ia32_pcmpgtw512_mask(...) getmask32()
#define __builtin_ia32_pabsb512_mask(...) get512i()
#define __builtin_ia32_pabsw512_mask(...) get512i()
#define __builtin_ia32_pmaxsb512_mask(...) get512i()
#define __builtin_ia32_pmaxsw512_mask(...) get512i()
#define __builtin_ia32_pmaxub512_mask(...) get512i()
#define __builtin_ia32_pmaxuw512_mask(...) get512i()
#define __builtin_ia32_pminsb512_mask(...) get512i()
#define __builtin_ia32_pminsw512_mask(...) get512i()
#define __builtin_ia32_pminub512_mask(...) get512i()
#define __builtin_ia32_pminuw512_mask(...) get512i()
#define __builtin_ia32_psubusb512_mask(...) get512i()
#define __builtin_ia32_psubusw512_mask(...) get512i()
#define __builtin_ia32_vpermi2varhi512_mask(...) get512i()
#define __builtin_ia32_vpermt2varhi512_mask(...) get512i()
#define __builtin_ia32_vpermt2varhi512_maskz(...) get512i()
#define __builtin_ia32_pmulhrsw512_mask(...) get512i()
#define __builtin_ia32_pmaddwd512_mask(...) get512i()
#define __builtin_ia32_pmovswb512_mask(...) get256i()
#define __builtin_ia32_pmovuswb512_mask(...) get256i()
#define __builtin_ia32_pmovwb512_mask(...) get256i()

#define __builtin_ia32_pcmpeqb128_mask(...) getmask16()
#define __builtin_ia32_ucmpb128_mask(...) getmask16()
#define __builtin_ia32_pcmpeqb256_mask(...) getmask32()
#define __builtin_ia32_ucmpb256_mask(...) getmask32()
#define __builtin_ia32_pcmpeqw128_mask(...) getmask8()
#define __builtin_ia32_ucmpw128_mask(...) getmask8()
#define __builtin_ia32_pcmpeqw256_mask(...) getmask16()
#define __builtin_ia32_ucmpw256_mask(...) getmask16()
#define __builtin_ia32_cmpb128_mask(...) getmask16()
#define __builtin_ia32_cmpb256_mask(...) getmask32()
#define __builtin_ia32_cmpw128_mask(...) getmask8()
#define __builtin_ia32_cmpw256_mask(...) getmask16()
#define __builtin_ia32_pcmpgtb128_mask(...) getmask16()
#define __builtin_ia32_pcmpgtb256_mask(...) getmask32()
#define __builtin_ia32_pcmpgtw128_mask(...) getmask8()
#define __builtin_ia32_pcmpgtw256_mask(...) getmask16()
#define __builtin_ia32_vpermi2varhi128_mask(...) get128i()
#define __builtin_ia32_vpermi2varhi256_mask(...) get256i()
#define __builtin_ia32_vpermt2varhi128_mask(...) get128i()
#define __builtin_ia32_vpermt2varhi128_maskz(...) get128i()
#define __builtin_ia32_vpermt2varhi256_mask(...) get256i()
#define __builtin_ia32_vpermt2varhi256_maskz(...) get256i()
#define __builtin_ia32_pmovswb128_mask(...) get128i()
#define __builtin_ia32_pmovswb256_mask(...) get128i()
#define __builtin_ia32_pmovuswb128_mask(...) get128i()
#define __builtin_ia32_pmovuswb256_mask(...) get128i()
#define __builtin_ia32_pmovwb128_mask(...) get128i()
#define __builtin_ia32_pmovwb256_mask(...) get128i()

#define __builtin_ia32_vfmaddss(...) get128()
#define __builtin_ia32_vfmsubps(...) get128()
#define __builtin_ia32_vfmsubss(...) get128()
#define __builtin_ia32_vfnmaddps(...) get128()
#define __builtin_ia32_vfnmaddss(...) get128()
#define __builtin_ia32_vfnmsubps(...) get128()
#define __builtin_ia32_vfnmsubss(...) get128()
#define __builtin_ia32_vfmsubaddps(...) get128()
#define __builtin_ia32_vfmsubps256(...) get256()
#define __builtin_ia32_vfnmaddps256(...) get256()
#define __builtin_ia32_vfnmsubps256(...) get256()
#define __builtin_ia32_vfmsubaddps256(...) get256()

#define __builtin_ia32_pabsb256(...) get256i()
#define __builtin_ia32_pabsw256(...) get256i()
#define __builtin_ia32_pabsd256(...) get256i()
#define __builtin_ia32_packsswb256(...) get256i()
#define __builtin_ia32_packssdw256(...) get256i()
#define __builtin_ia32_packuswb256(...) get256i()
#define __builtin_ia32_packusdw256(...) get256i()
#define __builtin_ia32_paddsb256(...) get256i()
#define __builtin_ia32_paddsw256(...) get256i()
#define __builtin_ia32_paddusb256(...) get256i()
#define __builtin_ia32_paddusw256(...) get256i()
#define __builtin_ia32_palignr256(...) get256i()
#define __builtin_ia32_pavgb256(...) get256i()
#define __builtin_ia32_pavgw256(...) get256i()
#define __builtin_ia32_pblendvb256(...) get256i()
//#define __builtin_shufflevector(...) 0
#define __builtin_ia32_phaddw256(...) get256i()
#define __builtin_ia32_phaddd256(...) get256i()
#define __builtin_ia32_phaddsw256(...) get256i()
#define __builtin_ia32_phsubw256(...) get256i()
#define __builtin_ia32_phsubd256(...) get256i()
#define __builtin_ia32_phsubsw256(...) get256i()
#define __builtin_ia32_pmaddubsw256(...) get256i()
#define __builtin_ia32_pmaddwd256(...) get256i()
#define __builtin_ia32_pmaxsb256(...) get256i()
#define __builtin_ia32_pmaxsw256(...) get256i()
#define __builtin_ia32_pmaxsd256(...) get256i()
#define __builtin_ia32_pmaxub256(...) get256i()
#define __builtin_ia32_pmaxuw256(...) get256i()
#define __builtin_ia32_pmaxud256(...) get256i()
#define __builtin_ia32_pminsb256(...) get256i()
#define __builtin_ia32_pminsw256(...) get256i()
#define __builtin_ia32_pminsd256(...) get256i()
#define __builtin_ia32_pminub256(...) get256i()
#define __builtin_ia32_pminuw256(...) get256i()
#define __builtin_ia32_pminud256(...) get256i()
#define __builtin_ia32_pmuldq256(...) get256i()
#define __builtin_ia32_pmulhrsw256(...) get256i()
#define __builtin_ia32_pmulhuw256(...) get256i()
#define __builtin_ia32_pmulhw256(...) get256i()
#define __builtin_ia32_pshufb256(...) get256i()
#define __builtin_ia32_psignb256(...) get256i()
#define __builtin_ia32_psignw256(...) get256i()
#define __builtin_ia32_psignd256(...) get256i()
#define __builtin_ia32_pslldqi256(...) get256i()
#define __builtin_ia32_psllwi256(...) get256i()
#define __builtin_ia32_psllw256(...) get256i()
#define __builtin_ia32_pslldi256(...) get256i()
#define __builtin_ia32_pslld256(...) get256i()
#define __builtin_ia32_psrawi256(...) get256i()
#define __builtin_ia32_psraw256(...) get256i()
#define __builtin_ia32_psradi256(...) get256i()
#define __builtin_ia32_psrad256(...) get256i()
#define __builtin_ia32_psrldqi256(...) get256i()
#define __builtin_ia32_psrlwi256(...) get256i()
#define __builtin_ia32_psrlw256(...) get256i()
#define __builtin_ia32_psrldi256(...) get256i()
#define __builtin_ia32_psrld256(...) get256i()
#define __builtin_ia32_psubsb256(...) get256i()
#define __builtin_ia32_psubsw256(...) get256i()
#define __builtin_ia32_psubusb256(...) get256i()
#define __builtin_ia32_psubusw256(...) get256i()
#define __builtin_ia32_movntdqa256(...) get256i()
#define __builtin_ia32_permvarsi256(...) get256i()
#define __builtin_ia32_permvarsf256(...) get256()
#define __builtin_ia32_permti256(...) get256i()
#define __builtin_ia32_maskloadd256(...) get256i()
#define __builtin_ia32_maskloadq256(...) get256i()
#define __builtin_ia32_maskloadd(...) get128i()
#define __builtin_ia32_maskloadq(...) get128i()
#define __builtin_ia32_psllv8si(...) get256i()
#define __builtin_ia32_psllv4si(...) get128i()
#define __builtin_ia32_psllv4di(...) get256i()
#define __builtin_ia32_psllv2di(...) get128i()
#define __builtin_ia32_psrav8si(...) get256i()
#define __builtin_ia32_psrav4si(...) get128i()
#define __builtin_ia32_psrlv8si(...) get256i()
#define __builtin_ia32_psrlv4si(...) get128i()
#define __builtin_ia32_psrlv4di(...) get256i()
#define __builtin_ia32_psrlv2di(...) get128i()
#define __builtin_ia32_gatherd_pd(...) get128d()
#define __builtin_ia32_gatherd_pd256(...) get256d()
#define __builtin_ia32_gatherq_pd(...) get128d()
#define __builtin_ia32_gatherq_pd256(...) get256d()
#define __builtin_ia32_gatherd_ps(...) get128()
#define __builtin_ia32_gatherd_ps256(...) get256()
#define __builtin_ia32_gatherq_ps(...) get128()
#define __builtin_ia32_gatherq_ps256(...) get128()
#define __builtin_ia32_gatherd_d(...) get128i()
#define __builtin_ia32_gatherd_d256(...) get256i()
#define __builtin_ia32_gatherq_d(...) get128i()
#define __builtin_ia32_gatherq_d256(...) get128i()
#define __builtin_ia32_gatherd_q(...) get128i()
#define __builtin_ia32_gatherd_q256(...) get256i()
#define __builtin_ia32_gatherq_q(...) get128i()
#define __builtin_ia32_gatherq_q256(...) get256i()

#define __builtin_ia32_movntdqa(...) get128i()

#define __builtin_ia32_vpopcntw_512(...) get512i()
#define __builtin_ia32_selectw_512(...) get512i()
#define __builtin_ia32_vpopcntb_512(...) get512i()
#define __builtin_ia32_vpshufbitqmb512_mask(...) getmask64()

#define __builtin_ia32_vpopcntq_512(...) get512i()
#define __builtin_ia32_selectq_512(...) get512i()
#define __builtin_ia32_vpopcntd_512(...) get512i()
#define __builtin_ia32_selectd_512(...) get512i()

#define __builtin_ia32_vpdpbusd256_mask(...) get256i()
#define __builtin_ia32_vpdpbusd256_maskz(...) get256i()
#define __builtin_ia32_vpdpbusds256_mask(...) get256i()
#define __builtin_ia32_vpdpbusds256_maskz(...) get256i()
#define __builtin_ia32_vpdpwssd256_mask(...) get256i()
#define __builtin_ia32_vpdpwssd256_maskz(...) get256i()
#define __builtin_ia32_vpdpwssds256_mask(...) get256i()
#define __builtin_ia32_vpdpwssds256_maskz(...) get256i()
#define __builtin_ia32_vpdpbusd128_mask(...) get128i()
#define __builtin_ia32_vpdpbusd128_maskz(...) get128i()
#define __builtin_ia32_vpdpbusds128_mask(...) get128i()
#define __builtin_ia32_vpdpbusds128_maskz(...) get128i()
#define __builtin_ia32_vpdpwssd128_mask(...) get128i()
#define __builtin_ia32_vpdpwssd128_maskz(...) get128i()
#define __builtin_ia32_vpdpwssds128_mask(...) get128i()
#define __builtin_ia32_vpdpwssds128_maskz(...) get128i()

#define __builtin_ia32_vpopcntq_128(...) get128i()
#define __builtin_ia32_selectq_128(...) get128i()
#define __builtin_ia32_vpopcntd_128(...) get128i()
#define __builtin_ia32_selectd_128(...) get128i()
#define __builtin_ia32_vpopcntq_256(...) get256i()
#define __builtin_ia32_selectq_256(...) get256i()
#define __builtin_ia32_vpopcntd_256(...) get256i()
#define __builtin_ia32_selectd_256(...) get256i()

#define __builtin_ia32_vpdpbusd512_mask(...) get512i()
#define __builtin_ia32_vpdpbusd512_maskz(...) get512i()
#define __builtin_ia32_vpdpbusds512_mask(...) get512i()
#define __builtin_ia32_vpdpbusds512_maskz(...) get512i()
#define __builtin_ia32_vpdpwssd512_mask(...) get512i()
#define __builtin_ia32_vpdpwssd512_maskz(...) get512i()
#define __builtin_ia32_vpdpwssds512_mask(...) get512i()
#define __builtin_ia32_vpdpwssds512_maskz(...) get512i()

#define __builtin_ia32_vpopcntw_256(...) get256i()
#define __builtin_ia32_selectw_256(...) get256i()
#define __builtin_ia32_vpopcntw_128(...) get128i()
#define __builtin_ia32_selectw_128(...) get128i()
#define __builtin_ia32_vpopcntb_256(...) get256i()
#define __builtin_ia32_vpopcntb_128(...) get128i()
#define __builtin_ia32_vpshufbitqmb256_mask(...) getmask32()
#define __builtin_ia32_vpshufbitqmb128_mask(...) getmask16()


#define __builtin_ia32_compresshi128_mask(...) get128i()
#define __builtin_ia32_compressqi128_mask(...) get128i()
#define __builtin_ia32_expandhi128_mask(...) get128i()
#define __builtin_ia32_expandqi128_mask(...) get128i()
#define __builtin_ia32_expandloadhi128_mask(...) get128i()
#define __builtin_ia32_expandloadqi128_mask(...) get128i()
#define __builtin_ia32_compresshi256_mask(...) get256i()
#define __builtin_ia32_compressqi256_mask(...) get256i()
#define __builtin_ia32_expandhi256_mask(...) get256i()
#define __builtin_ia32_expandqi256_mask(...) get256i()
#define __builtin_ia32_expandloadhi256_mask(...) get256i()
#define __builtin_ia32_expandloadqi256_mask(...) get256i()
#define __builtin_ia32_vpshldq256_mask(...) get256i()
#define __builtin_ia32_vpshldq128_mask(...) get128i()
#define __builtin_ia32_vpshldd256_mask(...) get256i()
#define __builtin_ia32_vpshldd128_mask(...) get128i()
#define __builtin_ia32_vpshldw256_mask(...) get256i()
#define __builtin_ia32_vpshldw128_mask(...) get128i()
#define __builtin_ia32_vpshrdq256_mask(...) get256i()
#define __builtin_ia32_vpshrdq128_mask(...) get128i()
#define __builtin_ia32_vpshrdd256_mask(...) get256i()
#define __builtin_ia32_vpshrdd128_mask(...) get128i()
#define __builtin_ia32_vpshrdw256_mask(...) get256i()
#define __builtin_ia32_vpshrdw128_mask(...) get128i()
#define __builtin_ia32_vpshldvq256_mask(...) get256i()
#define __builtin_ia32_vpshldvq256_maskz(...) get256i()
#define __builtin_ia32_vpshldvq128_mask(...) get128i()
#define __builtin_ia32_vpshldvq128_maskz(...) get128i()
#define __builtin_ia32_vpshldvd256_mask(...) get256i()
#define __builtin_ia32_vpshldvd256_maskz(...) get256i()
#define __builtin_ia32_vpshldvd128_mask(...) get128i()
#define __builtin_ia32_vpshldvd128_maskz(...) get128i()
#define __builtin_ia32_vpshldvw256_mask(...) get256i()
#define __builtin_ia32_vpshldvw256_maskz(...) get256i()
#define __builtin_ia32_vpshldvw128_mask(...) get128i()
#define __builtin_ia32_vpshldvw128_maskz(...) get128i()
#define __builtin_ia32_vpshrdvq256_mask(...) get256i()
#define __builtin_ia32_vpshrdvq256_maskz(...) get256i()
#define __builtin_ia32_vpshrdvq128_mask(...) get128i()
#define __builtin_ia32_vpshrdvq128_maskz(...) get128i()
#define __builtin_ia32_vpshrdvd256_mask(...) get256i()
#define __builtin_ia32_vpshrdvd256_maskz(...) get256i()
#define __builtin_ia32_vpshrdvd128_mask(...) get128i()
#define __builtin_ia32_vpshrdvd128_maskz(...) get128i()
#define __builtin_ia32_vpshrdvw256_mask(...) get256i()
#define __builtin_ia32_vpshrdvw256_maskz(...) get256i()
#define __builtin_ia32_vpshrdvw128_mask(...) get128i()
#define __builtin_ia32_vpshrdvw128_maskz(...) get128i()

#define __builtin_ia32_compressstorehi128_mask(...)
#define __builtin_ia32_compressstoreqi128_mask(...)
#define __builtin_ia32_compressstorehi256_mask(...)
#define __builtin_ia32_compressstoreqi256_mask(...)

#define __builtin_ia32_aesenc256(...) get256i()
#define __builtin_ia32_aesenc512(...) get512i()
#define __builtin_ia32_aesdec256(...) get256i()
#define __builtin_ia32_aesdec512(...) get512i()
#define __builtin_ia32_aesenclast256(...) get256i()
#define __builtin_ia32_aesenclast512(...) get512i()
#define __builtin_ia32_aesdeclast256(...) get256i()
#define __builtin_ia32_aesdeclast512(...) get512i()

#define __builtin_ia32_vgf2p8affineinvqb_v16qi(...) get128i()
#define __builtin_ia32_selectb_128(...) get128i()
#define __builtin_ia32_vgf2p8affineinvqb_v32qi(...) get256i()
#define __builtin_ia32_selectb_256(...) get256i()
#define __builtin_ia32_vgf2p8affineinvqb_v64qi(...) get512i()
#define __builtin_ia32_selectb_512(...) get512i()
#define __builtin_ia32_vgf2p8affineqb_v16qi(...) get128i()
#define __builtin_ia32_vgf2p8affineqb_v32qi(...) get256i()
#define __builtin_ia32_vgf2p8affineqb_v64qi(...) get512i()
#define __builtin_ia32_vgf2p8mulb_v16qi(...) get128i()
#define __builtin_ia32_vgf2p8mulb_v32qi(...) get256i()
#define __builtin_ia32_vgf2p8mulb_v64qi(...) get512i()

#define __builtin_ia32_incsspd(...)
#define __builtin_ia32_incsspq(...)
#define __builtin_ia32_saveprevssp(...)
#define __builtin_ia32_rstorssp(...)
#define __builtin_ia32_wrssd(...)
#define __builtin_ia32_wrssq(...)
#define __builtin_ia32_wrussd(...)
#define __builtin_ia32_wrussq(...)
#define __builtin_ia32_setssbsy(...)
#define __builtin_ia32_clrssbsy(...)

#define __builtin_ia32_rdsspd(...) __VA_ARGS__
#define __builtin_ia32_rdsspq(...) __VA_ARGS__

#define __builtin_ia32_llwpcb(...)
#define __builtin_ia32_slwpcb(...) __VA_ARGS__

#define get__v4df() get256d()
#define get__v32qu() get256i()
#define get__v16hu() get256i()
#define get__v16hi() get256i()
#define get__v8si() get256i()
#define get__v4di() get256i()
#define __builtin_convertvector(a,b) get##b()

#define __builtin_ia32_vfmaddps(...) get128()
#define __builtin_ia32_vfmaddpd(...) get128d()
#define __builtin_ia32_vfmaddss3(...) get128()
#define __builtin_ia32_vfmaddsd3(...) get128d()
#define __builtin_ia32_vfmaddsubps(...) get128()
#define __builtin_ia32_vfmaddsubpd(...) get128d()
#define __builtin_ia32_vfmaddps256(...) get256()
#define __builtin_ia32_vfmaddpd256(...) get256d()
#define __builtin_ia32_vfmaddsubps256(...) get256()
#define __builtin_ia32_vfmaddsubpd256(...) get256d()

#define get__v8df() get512d()
#define get__v16si() get512i()
#define get__v8di() get512i()
#define get__v2df() get128d()
#define get__v32hi() get512i()
#define get__v32hu() get512i()

#define __builtin_ia32_compresshi512_mask(...) get512i()
#define __builtin_ia32_compressqi512_mask(...) get512i()
#define __builtin_ia32_expandhi512_mask(...) get512i()
#define __builtin_ia32_expandqi512_mask(...) get512i()
#define __builtin_ia32_expandloadhi512_mask(...) get512i()
#define __builtin_ia32_expandloadqi512_mask(...) get512i()
#define __builtin_ia32_vpshldq512_mask(...) get512i()
#define __builtin_ia32_vpshldd512_mask(...) get512i()
#define __builtin_ia32_vpshldw512_mask(...) get512i()
#define __builtin_ia32_vpshrdq512_mask(...) get512i()
#define __builtin_ia32_vpshrdd512_mask(...) get512i()
#define __builtin_ia32_vpshrdw512_mask(...) get512i()
#define __builtin_ia32_vpshldvq512_mask(...) get512i()
#define __builtin_ia32_vpshldvq512_maskz(...) get512i()
#define __builtin_ia32_vpshldvd512_mask(...) get512i()
#define __builtin_ia32_vpshldvd512_maskz(...) get512i()
#define __builtin_ia32_vpshldvw512_mask(...) get512i()
#define __builtin_ia32_vpshldvw512_maskz(...) get512i()
#define __builtin_ia32_vpshrdvq512_mask(...) get512i()
#define __builtin_ia32_vpshrdvq512_maskz(...) get512i()
#define __builtin_ia32_vpshrdvd512_mask(...) get512i()
#define __builtin_ia32_vpshrdvd512_maskz(...) get512i()
#define __builtin_ia32_vpshrdvw512_mask(...) get512i()
#define __builtin_ia32_vpshrdvw512_maskz(...) get512i()
#define __builtin_ia32_compressstorehi512_mask(...)
#define __builtin_ia32_compressstoreqi512_mask(...)

#define get__v64qu() get512i()
#define get__v2di() get128i()
#define get__v4si() get128i()
#define get__v8hi() get128i()
#define get__v8hu() get128i()
#define get__v16qu() get128i()

#define __builtin_ia32_clwb(...)

#define __builtin_ia32_clflushopt(...)



#define __builtin_ia32_pblendw128(x,...) x
#define __builtin_ia32_blendps(x,...) x
#define __builtin_ia32_blendpd(x,...) x
#define __builtin_ia32_pcmpeqq(x,...) x
#define __builtin_ia32_pmulld128(x,...) x
#define __builtin_ia32_vec_set_v16qi(x,...) x
#define __builtin_ia32_vec_set_v4si(x,...) x
#define __builtin_ia32_vec_set_v2di(x,...) x
#define __builtin_ia32_pmovsxbd128(x,...) x
#define __builtin_ia32_pmovsxwd128(x,...) x
#define __builtin_ia32_pmovsxbq128(x,...) x
#define __builtin_ia32_pmovsxdq128(x,...) x
#define __builtin_ia32_pmovsxwq128(x,...) x
#define __builtin_ia32_pmovsxbw128(x,...) get128i()

#define __builtin_ia32_palignr128(x,...) x
#define __builtin_ia32_palignr(x,...) x
#define __builtin_ia32_extrqi(x,...) x
#define __builtin_ia32_insertqi(x,...) x
#define __builtin_ia32_roundpd(x,...) x
#define __builtin_ia32_roundsd(x,...) x
#define __builtin_ia32_roundps(x,...) x
#define __builtin_ia32_roundss(x,...) x
#define __builtin_ia32_dpps(x,...) x
#define __builtin_ia32_dppd(x,...) x
#define __builtin_ia32_insertps128(x,...) x
#define __builtin_ia32_mpsadbw128(x,...) x
#define __builtin_ia32_pcmpistrm128(x,...) x
#define __builtin_ia32_pcmpestrm128(x,...) x

#define __builtin_ia32_pcmpgtq(x,...) x
#define __builtin_ia32_addpd256(x,...) x
#define __builtin_ia32_addps256(x,...) x
#define __builtin_ia32_andpd256(x,...) x
#define __builtin_ia32_aeskeygenassist128(x,...) x
#define __builtin_ia32_pclmulqdq128(x,...) x
#define __builtin_ia32_andps256(x,...) x
#define __builtin_ia32_andnpd256(x,...) x
#define __builtin_ia32_andnps256(x,...) x
#define __builtin_ia32_blendpd256(x,...) x
#define __builtin_ia32_blendps256(x,...) x
#define __builtin_ia32_divpd256(x,...) x
#define __builtin_ia32_divps256(x,...) x
#define __builtin_ia32_mulpd256(x,...) x
#define __builtin_ia32_mulps256(x,...) x
#define __builtin_ia32_orpd256(x,...) x
#define __builtin_ia32_dpps256(x,...) x
#define __builtin_ia32_orps256(x,...) x
#define __builtin_ia32_shufpd256(x,...) x
#define __builtin_ia32_shufps256(x,...) x
#define __builtin_ia32_subpd256(x,...) x
#define __builtin_ia32_subps256(x,...) x
#define __builtin_ia32_cmppd256(x,...) x
#define __builtin_ia32_xorpd256(x,...) x
#define __builtin_ia32_xorps256(x,...) x
#define __builtin_ia32_cmppd(x,...) x
#define __builtin_ia32_cmpps(x,...) x
#define __builtin_ia32_cmpps256(x,...) x
#define __builtin_ia32_cmpsd(x,...) x
#define __builtin_ia32_vpermilpd(x,...) x
#define __builtin_ia32_cmpss(x,...) x
#define __builtin_ia32_vpermilpd256(x,...) x
#define __builtin_ia32_vpermilps(x,...) x
#define __builtin_ia32_vpermilps256(x,...) x
#define __builtin_ia32_vperm2f128_pd256(x,...) x
#define __builtin_ia32_vperm2f128_ps256(x,...) x
#define __builtin_ia32_vperm2f128_si256(x,...) x
#define __builtin_ia32_vextractf128_pd256(...) get128d()
#define __builtin_ia32_vextractf128_ps256(...) get128()
#define __builtin_ia32_vextractf128_si256(...) get128i()
#define __builtin_ia32_vinsertf128_pd256(x,...) x
#define __builtin_ia32_vinsertf128_ps256(x,...) x
#define __builtin_ia32_vbroadcastss(...) get128()
#define __builtin_ia32_vinsertf128_si256(x,...) x
#define __builtin_ia32_vbroadcastsd256(...) get256d()
#define __builtin_ia32_movshdup256(x,...) x
#define __builtin_ia32_vbroadcastss256(...) get256()
#define __builtin_ia32_movsldup256(x,...) x
#define __builtin_ia32_movddup256(x,...) x
#define __builtin_ia32_loadupd256(...) get256d()
#define __builtin_ia32_unpckhpd256(x,...) x
#define __builtin_ia32_unpcklpd256(x,...) x
#define __builtin_ia32_roundpd256(x,...) x
#define __builtin_ia32_roundps256(x,...) x
#define __builtin_ia32_loadups256(...) get256()
#define __builtin_ia32_unpckhps256(x,...) x
#define __builtin_ia32_unpcklps256(x,...) x
#define __builtin_ia32_loaddqu256(...) get256i()
#define __builtin_ia32_pd_pd256(...) get128d()
#define __builtin_ia32_mpsadbw256(x,...) x
#define __builtin_ia32_ps_ps256(...) get128()
#define __builtin_ia32_paddb256(x,...) x
#define __builtin_ia32_paddw256(x,...) x
#define __builtin_ia32_si_si256(...) get128i()
#define __builtin_ia32_pd256_pd(...) get256d()
#define __builtin_ia32_ps256_ps(...) get256()
#define __builtin_ia32_si256_si(...) get256i()
#define __builtin_ia32_paddq256(x,...) x
#define __builtin_ia32_andsi256(x,...) x
#define __builtin_ia32_andnotsi256(x,...) x
#define __builtin_ia32_pblendw256(x,...) x
#define __builtin_ia32_paddd256(...) get256i()
#define __builtin_ia32_pcmpeqw256(x,...) x
#define __builtin_ia32_pcmpeqd256(x,...) x
#define __builtin_ia32_pcmpeqb256(...) get256i()
#define __builtin_ia32_pcmpgtb256(x,...) x
#define __builtin_ia32_pcmpgtw256(x,...) x
#define __builtin_ia32_pcmpgtd256(x,...) x
#define __builtin_ia32_pcmpeqq256(...) get256i()
#define __builtin_ia32_pmullw256(x,...) x
#define __builtin_ia32_pmulld256(x,...) x
#define __builtin_ia32_por256(x,...) x
#define __builtin_ia32_pshufd256(x,...) x
#define __builtin_ia32_pcmpgtq256(...) get256i()
#define __builtin_ia32_pshuflw256(x,...) x
#define __builtin_ia32_psubb256(x,...) x
#define __builtin_ia32_psubw256(x,...) x
#define __builtin_ia32_pshufhw256(...) get256i()
#define __builtin_ia32_psubq256(x,...) x
#define __builtin_ia32_punpckhbw256(x,...) x
#define __builtin_ia32_punpckhwd256(x,...) x
#define __builtin_ia32_psubd256(...) get256i()
#define __builtin_ia32_punpckhqdq256(x,...) x
#define __builtin_ia32_punpcklbw256(x,...) x
#define __builtin_ia32_punpckhdq256(...) get256i()
#define __builtin_ia32_punpckldq256(x,...) x
#define __builtin_ia32_punpcklqdq256(x,...) x
#define __builtin_ia32_punpcklwd256(...) get256i()
#define __builtin_ia32_vbroadcastss_ps(x,...) x
#define __builtin_ia32_pxor256(...) get256i()
#define __builtin_ia32_pblendd128(x,...) x
#define __builtin_ia32_pblendd256(x,...) x
#define __builtin_ia32_vbroadcastss_ps256(...) get256()
#define __builtin_ia32_vbroadcastsd_pd256(...) get256d()
#define __builtin_ia32_vbroadcastsi256(...) get256i()
#define __builtin_ia32_pbroadcastb128(x,...) x
#define __builtin_ia32_pbroadcastb256(...) get256i()
#define __builtin_ia32_pbroadcastw256(...) get256i()
#define __builtin_ia32_pbroadcastd256(...) get256i()
#define __builtin_ia32_pbroadcastw128(x,...) x
#define __builtin_ia32_pbroadcastd128(x,...) x
#define __builtin_ia32_pbroadcastq128(x,...) x
#define __builtin_ia32_permdf256(x,...) x
#define __builtin_ia32_pbroadcastq256(...) get256i()
#define __builtin_ia32_insert128i256(x,...) x
#define __builtin_ia32_gathersiv2df(x,...) x
#define __builtin_ia32_permdi256(...) get256i()
#define __builtin_ia32_gathersiv4df(x,...) x
#define __builtin_ia32_gatherdiv2df(x,...) x
#define __builtin_ia32_gatherdiv4df(x,...) x
#define __builtin_ia32_extract128i256(...) get128i()
#define __builtin_ia32_gathersiv8sf(x,...) x
#define __builtin_ia32_gathersiv4sf(...) get128()
#define __builtin_ia32_gatherdiv4sf256(x,...) x
#define __builtin_ia32_gatherdiv4sf(...) get128()
#define __builtin_ia32_gathersiv4di(x,...) x
#define __builtin_ia32_gatherdiv2di(x,...) x
#define __builtin_ia32_gathersiv2di(...) get128i()
#define __builtin_ia32_gathersiv4si(x,...) x
#define __builtin_ia32_gatherdiv4di(...) get256i()
#define __builtin_ia32_gatherdiv4si(x,...) x
#define __builtin_ia32_gathersiv8si(...) get256i()
#define __builtin_ia32_movapd512_mask(x,...) x
#define __builtin_ia32_gatherdiv4si256(...) get128i()
#define __builtin_ia32_broadcastsd512(...) get512d()
#define __builtin_ia32_broadcastss512(...) get512()
#define __builtin_ia32_movdqa64_512_mask(x,...) x
#define __builtin_ia32_movaps512_mask(...) get512()
#define __builtin_ia32_psllv16si_mask(x,...) x
#define __builtin_ia32_movdqa32_512_mask(...) get512i()
#define __builtin_ia32_psrlv16si_mask(x,...) x
#define __builtin_ia32_psrav16si_mask(...) get512i()
#define __builtin_ia32_psrav8di_mask(x,...) x
#define __builtin_ia32_psllv8di_mask(...) get512i()
#define __builtin_ia32_psllqi512_mask(x,...) x
#define __builtin_ia32_psrlv8di_mask(...) get512i()
#define __builtin_ia32_psllq512_mask(...) get512i()
#define __builtin_ia32_prolq512_mask(x,...) x
#define __builtin_ia32_psrlqi512_mask(...) get512i()
#define __builtin_ia32_psrlq512_mask(...) get512i()
#define __builtin_ia32_psraq512_mask(x,...) x
#define __builtin_ia32_psraqi512_mask(...) get512i()
#define __builtin_ia32_pslldi512_mask(...) get512i()
#define __builtin_ia32_pslld512_mask(...) get512i()
#define __builtin_ia32_prold512_mask(x,...) x
#define __builtin_ia32_psrldi512_mask(...) get512i()
#define __builtin_ia32_psrld512_mask(...) get512i()
#define __builtin_ia32_psrad512_mask(x,...) x
#define __builtin_ia32_psradi512_mask(...) get512i()
#define __builtin_ia32_pternlogq512_maskz(x,...) x
#define __builtin_ia32_pternlogd512_mask(x,...) x
#define __builtin_ia32_pternlogd512_maskz(x,...) x
#define __builtin_ia32_pternlogq512_mask(...) get512i()
#define __builtin_ia32_sqrtss_round(x,...) x
#define __builtin_ia32_sqrtsd_round(...) get128d()
#define __builtin_ia32_pmovsxbd512_mask(...) get512i()
#define __builtin_ia32_pmovsxbq512_mask(...) get512i()
#define __builtin_ia32_pmovsxwd512_mask(...) get512i()
#define __builtin_ia32_pmovsxwq512_mask(...) get512i()
#define __builtin_ia32_pmovsxdq512_mask(...) get512i()
#define __builtin_ia32_pmovzxbd512_mask(...) get512i()
#define __builtin_ia32_pmovzxbq512_mask(...) get512i()
#define __builtin_ia32_pmovzxwd512_mask(...) get512i()
#define __builtin_ia32_pmovzxwq512_mask(...) get512i()
#define __builtin_ia32_scalefpd512_mask(x,...) x
#define __builtin_ia32_pmovzxdq512_mask(...) get512i()
#define __builtin_ia32_scalefsd_round(x,...) x
#define __builtin_ia32_scalefss_round(x,...) x
#define __builtin_ia32_scalefps512_mask(...) get512()
#define __builtin_ia32_pbroadcastd512(...) get512i()
#define __builtin_ia32_pbroadcastq512(...) get512i()
#define __builtin_ia32_broadcastf32x4_512(...) get512()
#define __builtin_ia32_broadcasti32x4_512(...) get512i()
#define __builtin_ia32_broadcastf64x4_512(...) get512d()
#define __builtin_ia32_pshufd512_mask(x,...) x
#define __builtin_ia32_broadcasti64x4_512(...) get512i()
#define __builtin_ia32_shuf_i32x4_mask(x,...) x
#define __builtin_ia32_shuf_i64x2_mask(...) get512i()
#define __builtin_ia32_shuf_f32x4_mask(x,...) x
#define __builtin_ia32_shuf_f64x2_mask(...) get512d()
#define __builtin_ia32_cvtusi2sd64(x,...) x
#define __builtin_ia32_cvtsi2sd64(x,...) x
#define __builtin_ia32_cvttpd2udq512_mask(...) get256i()
#define __builtin_ia32_cvtsi2ss32(x,...) x
#define __builtin_ia32_cvtusi2ss64(x,...) x
#define __builtin_ia32_cvtsi2ss64(x,...) x
#define __builtin_ia32_cvtusi2ss32(...) get128()
#define __builtin_ia32_extractf64x4_mask(...) get256d()
#define __builtin_ia32_extractf32x4_mask(...) get128()
#define __builtin_ia32_extracti64x4_mask(...) get256i()
#define __builtin_ia32_inserti32x4_mask(x,...) x
#define __builtin_ia32_insertf32x4_mask(x,...) x
#define __builtin_ia32_extracti32x4_mask(...) get128i()
#define __builtin_ia32_inserti64x4_mask(...) get512i()
#define __builtin_ia32_vpermilvarpd512_mask(x,...) x
#define __builtin_ia32_insertf64x4_mask(...) get512d()
#define __builtin_ia32_vpermilpd512_mask(x,...) x
#define __builtin_ia32_vpermilvarps512_mask(...) get512()
#define __builtin_ia32_vpermilps512_mask(...) get512()
#define __builtin_ia32_permdi512_mask(...) get512i()
#define __builtin_ia32_permdf512_mask(...) get512d()
#define __builtin_ia32_shufps512_mask(...) get512()
#define __builtin_ia32_shufpd512_mask(...) get512d()
#define __builtin_ia32_fixupimmpd512_maskz(x,...) x
#define __builtin_ia32_fixupimmps512_mask(x,...) x
#define __builtin_ia32_fixupimmps512_maskz(x,...) x
#define __builtin_ia32_fixupimmpd512_mask(...) get512d()
#define __builtin_ia32_fixupimmsd_maskz(x,...) x
#define __builtin_ia32_fixupimmss_mask(x,...) x
#define __builtin_ia32_fixupimmss_maskz(x,...) x
#define __builtin_ia32_fixupimmsd_mask(...) get128d()
#define __builtin_ia32_movsldup512_mask(x,...) x
#define __builtin_ia32_movshdup512_mask(...) get512()
#define __builtin_ia32_punpckhqdq512_mask(x,...) x
#define __builtin_ia32_punpckhdq512_mask(...) get512i()
#define __builtin_ia32_punpcklqdq512_mask(x,...) x
#define __builtin_ia32_punpckldq512_mask(...) get512i()








#define __builtin_ia32_vec_ext_v16qi(...) (0)
#define __builtin_ia32_vcvtss2usi64(...) (0)
#define __builtin_ia32_vcvtss2si64(...) (0)
#define __builtin_ia32_vcvttss2usi64(...) (0)
#define __builtin_ia32_vcvttss2si64(...) (0)

#define __builtin_ia32_vcvtss2usi32(...) (0)
#define __builtin_ia32_vcvtss2si32(...) (0)
#define __builtin_ia32_vcvttss2usi32(...) (0)
#define __builtin_ia32_vcvttss2si32(...) (0)
#define __builtin_ia32_vcvtsd2usi64(...) (0)
#define __builtin_ia32_vcvtsd2si64(...) (0)
#define __builtin_ia32_vcvttsd2usi64(...) (0)
#define __builtin_ia32_vcvttsd2si64(...) (0)
#define __builtin_ia32_vcvtsd2usi32(...) (0)
#define __builtin_ia32_vcvtsd2si32(...) (0)
#define __builtin_ia32_movddup512_mask(x,...) x
#define __builtin_ia32_unpcklpd512_mask(x,...) x
#define __builtin_ia32_unpckhpd512_mask(x,...) x
#define __builtin_ia32_vcvttsd2usi32(...) (0)
#define __builtin_ia32_vcvttsd2si32(...) (0)
#define __builtin_ia32_unpckhps512_mask(x,...) x
#define __builtin_ia32_cvtsd2ss_round(x,...) x
#define __builtin_ia32_cvtss2sd_round(x,...) x
#define __builtin_ia32_movntdq512(x,...) x
#define __builtin_ia32_cvtps2pd512_mask(...) get512d()
#define __builtin_ia32_movntps512(x,...) x
#define __builtin_ia32_movntpd512(x,...) x
#define __builtin_ia32_getexpss128_round(x,...) x
#define __builtin_ia32_getexpsd128_round(x,...) x
#define __builtin_ia32_getexpps512_mask(x,...) x
#define __builtin_ia32_getexppd512_mask(x,...) x
#define __builtin_ia32_getmantpd512_mask(x,...) x
#define __builtin_ia32_getmantps512_mask(x,...) x
#define __builtin_ia32_getmantsd_round(x,...) x
#define __builtin_ia32_getmantss_round(x,...) x
#define __builtin_ia32_rndscaless_round(x,...) x
#define __builtin_ia32_rndscalesd_round(x,...) x
#define __builtin_ia32_gathersiv16sf(x,...) x
#define __builtin_ia32_gathersiv8df(x,...) x
#define __builtin_ia32_cmpsd_mask(...) getmask8()
#define __builtin_ia32_cmpss_mask(...) getmask8()
#define __builtin_ia32_gatherdiv16sf(x,...) x
#define __builtin_ia32_gatherdiv8df(x,...) x
#define __builtin_ia32_gathersiv16si(x,...) x
#define __builtin_ia32_gathersiv8di(x,...) x
#define __builtin_ia32_gatherdiv16si(x,...) x
#define __builtin_ia32_gatherdiv8di(x,...) x
#define __builtin_ia32_scattersiv16sf(x,...) x
#define __builtin_ia32_scattersiv8df(x,...) x
#define __builtin_ia32_scatterdiv16sf(x,...) x
#define __builtin_ia32_scatterdiv8df(x,...) x
#define __builtin_ia32_scattersiv16si(x,...) x
#define __builtin_ia32_scattersiv8di(x,...) x
#define __builtin_ia32_expanddf512_maskz(x,...) x
#define __builtin_ia32_expandsf512_maskz(x,...) x
#define __builtin_ia32_scatterdiv16si(x,...) x
#define __builtin_ia32_scatterdiv8di(x,...) x
#define __builtin_ia32_expanddi512_maskz(x,...) x
#define __builtin_ia32_expandloaddi512_maskz(...) get512i()
#define __builtin_ia32_expandsi512_maskz(x,...) x
#define __builtin_ia32_unpcklps512_mask(x,...) x
#define __builtin_ia32_vfmaddsd3_round(x,...) x
#define __builtin_ia32_vfmaddss3_round(x,...) x
#define __builtin_ia32_kmov16(x,...) x
#define __builtin_ia32_exp2pd_mask(x,...) x
#define __builtin_ia32_exp2ps_mask(x,...) x
#define __builtin_ia32_rcp28pd_mask(x,...) x
#define __builtin_ia32_rcp28ps_mask(x,...) x
#define __builtin_ia32_rcp28sd_round(x,...) x
#define __builtin_ia32_rcp28ss_round(x,...) x
#define __builtin_ia32_rsqrt28pd_mask(x,...) x
#define __builtin_ia32_rsqrt28ps_mask(x,...) x
#define __builtin_ia32_rsqrt28sd_round(x,...) x
#define __builtin_ia32_rsqrt28ss_round(x,...) x
#define __builtin_ia32_gatherpfdpd(x,...) x
#define __builtin_ia32_gatherpfdps(x,...) x
#define __builtin_ia32_gatherpfqpd(x,...) x
#define __builtin_ia32_gatherpfqps(x,...) x
#define __builtin_ia32_pcmpistri128(...) (0)
#define __builtin_ia32_pcmpestri128(...) (0)
#define __builtin_ia32_pcmpistria128(...) (0)
#define __builtin_ia32_pcmpistric128(...) (0)
#define __builtin_ia32_pcmpistrio128(...) (0)
#define __builtin_ia32_pcmpistris128(...) (0)
#define __builtin_ia32_pcmpistriz128(...) (0)
#define __builtin_ia32_pcmpestria128(...) (0)
#define __builtin_ia32_pcmpestric128(...) (0)
#define __builtin_ia32_pcmpestrio128(...) (0)
#define __builtin_ia32_pcmpestris128(...) (0)
#define __builtin_ia32_pcmpestriz128(...) (0)
#define __builtin_ia32_vcomiss(...) (0)
#define __builtin_ia32_vcomisd(...) (0)
#define __builtin_ia32_sha1rnds4(x,...) x
#define __builtin_ia32_vcvtps2ph(x,...) x
#define __builtin_ia32_xabort(x,...) x
#define __builtin_ia32_vprotbi(x,...) x
#define __builtin_ia32_vprotwi(x,...) x
#define __builtin_ia32_vpcomltub(x,...) x
#define __builtin_ia32_vpcomleub(x,...) x
#define __builtin_ia32_vpcomgtub(x,...) x
#define __builtin_ia32_vprotdi(x,...) x
#define __builtin_ia32_vprotqi(x,...) x
#define __builtin_ia32_vcvtps2ph256(...) get128i()
#define __builtin_ia32_vpcomgeub(x,...) x
#define __builtin_ia32_vpcomequb(x,...) x
#define __builtin_ia32_vpcomnequb(x,...) x
#define __builtin_ia32_vpcomfalseub(x,...) x
#define __builtin_ia32_vpcomtrueub(x,...) x
#define __builtin_ia32_vpcomltuw(x,...) x
#define __builtin_ia32_vpcomleuw(x,...) x
#define __builtin_ia32_vpcomgtuw(x,...) x
#define __builtin_ia32_vpcomgeuw(x,...) x
#define __builtin_ia32_vpcomequw(x,...) x
#define __builtin_ia32_vpcomnequw(x,...) x
#define __builtin_ia32_vpcomfalseuw(x,...) x
#define __builtin_ia32_vpcomtrueuw(x,...) x
#define __builtin_ia32_vpcomltud(x,...) x
#define __builtin_ia32_vpcomleud(x,...) x
#define __builtin_ia32_vpcomgtud(x,...) x
#define __builtin_ia32_vpcomgeud(x,...) x
#define __builtin_ia32_vpcomequd(x,...) x
#define __builtin_ia32_vpcomnequd(x,...) x
#define __builtin_ia32_vpcomfalseud(x,...) x
#define __builtin_ia32_vpcomtrueud(x,...) x
#define __builtin_ia32_vpcomltuq(x,...) x
#define __builtin_ia32_vpcomleuq(x,...) x
#define __builtin_ia32_vpcomgtuq(x,...) x
#define __builtin_ia32_vpcomgeuq(x,...) x
#define __builtin_ia32_vpcomequq(x,...) x
#define __builtin_ia32_vpcomnequq(x,...) x
#define __builtin_ia32_vpcomfalseuq(x,...) x
#define __builtin_ia32_vpcomtrueuq(x,...) x
#define __builtin_ia32_vpcomltb(x,...) x
#define __builtin_ia32_vpcomleb(x,...) x
#define __builtin_ia32_vpcomgtb(x,...) x
#define __builtin_ia32_vpcomgeb(x,...) x
#define __builtin_ia32_vpcomeqb(x,...) x
#define __builtin_ia32_vpcomneqb(x,...) x
#define __builtin_ia32_vpcomfalseb(x,...) x
#define __builtin_ia32_vpcomtrueb(x,...) x
#define __builtin_ia32_vpcomltw(x,...) x
#define __builtin_ia32_vpcomlew(x,...) x
#define __builtin_ia32_vpcomgtw(x,...) x
#define __builtin_ia32_vpcomgew(x,...) x
#define __builtin_ia32_vpcomeqw(x,...) x
#define __builtin_ia32_vpcomneqw(x,...) x
#define __builtin_ia32_vpcomfalsew(x,...) x
#define __builtin_ia32_vpcomtruew(x,...) x
#define __builtin_ia32_vpcomltd(x,...) x
#define __builtin_ia32_vpcomled(x,...) x
#define __builtin_ia32_vpcomgtd(x,...) x
#define __builtin_ia32_vpcomged(x,...) x
#define __builtin_ia32_vpcomeqd(x,...) x
#define __builtin_ia32_vpcomneqd(x,...) x
#define __builtin_ia32_vpcomfalsed(x,...) x
#define __builtin_ia32_vpcomtrued(x,...) x
#define __builtin_ia32_vpcomltq(x,...) x
#define __builtin_ia32_vpcomleq(x,...) x
#define __builtin_ia32_vpcomgtq(x,...) x
#define __builtin_ia32_vpcomgeq(x,...) x
#define __builtin_ia32_vpcomeqq(x,...) x
#define __builtin_ia32_vpcomneqq(x,...) x
#define __builtin_ia32_vpcomfalseq(x,...) x
#define __builtin_ia32_vpcomtrueq(x,...) x
#define __builtin_ia32_lwpval32(x,...) x
#define __builtin_ia32_lwpval64(x,...) x
#define __builtin_ia32_vpermil2pd(x,...) x
#define __builtin_ia32_vpermil2pd256(x,...) x
#define __builtin_ia32_vpermil2ps(x,...) x
#define __builtin_ia32_vpermil2ps256(x,...) x
#define __builtin_ia32_lwpins32(x,...) x
#define __builtin_ia32_lwpins64(x,...) x
#define __builtin_ia32_bextri_u32(x,...) x
#define __builtin_ia32_bextri_u64(x,...) x
#define __builtin_ia32_rdseed_hi_step(...) (0)
#define __builtin_ia32_rdseed_si_step(...) (0)
#define __builtin_ia32_rdseed_di_step(...) (0)
#define __builtin_ia32_scatterpfdps(...) 
#define __builtin_ia32_scatterpfdpd(...) 
#define __builtin_ia32_scatterpfqpd(...) 
#define __builtin_ia32_scatterpfqps(...) 
#define __builtin_ia32_expandloaddf512_maskz(...) get512d()
#define __builtin_ia32_expandloadsf512_maskz(...) get512()
#define __builtin_ia32_expandloadsi512_maskz(...) get512i()

struct clangFloat128Fix {};

#endif

