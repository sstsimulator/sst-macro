#ifndef sstmac_stdio_h
#define sstmac_stdio_h

#include_next <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

FILE* sstmac_stdout();
FILE* sstmac_stderr();

#ifdef __cplusplus
}
#endif

#ifndef SSTMAC_NO_REPLACEMENTS
#undef stdout
#define stdout sstmac_stdout()
#undef stderr
#define stderr sstmac_stderr()
#define printf(...) fprintf(sstmac_stdout(), __VA_ARGS__)
#endif

#endif

