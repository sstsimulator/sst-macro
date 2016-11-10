

AC_DEFUN([CHECK_CPP11], [

AH_TEMPLATE([HAVE_CPP11],
            [Define to use C++11 language features])

#make sure if needed C++11 flags are automatically configured
AX_CXX_COMPILE_STDCXX_11(noext, optional)

])

