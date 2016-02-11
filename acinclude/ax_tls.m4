AC_DEFUN([AX_TLS], [
  AC_MSG_CHECKING(for thread local storage)
  AC_CACHE_VAL(ac_cv_tls, [
          AC_TRY_COMPILE(
              [#include <stdlib.h>
               static void
               func(void) {
               static __thread int x;
               exit(1);
               }],
               [],
               [ac_cv_tls=__thread],
               ac_cv_tls=none
           )])
   
  AC_MSG_RESULT($ac_cv_tls)

  AS_IF([test "$ac_cv_tls" != "none"],
    [$1],
    [$2]
  )
  
])
