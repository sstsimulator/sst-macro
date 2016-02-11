

AC_DEFUN([CHECK_INTEGER_TYPES], [
# Get the size of the C types for encoding in the basic datatypes and for
# the specific-sized integers
AC_CHECK_SIZEOF(char)
AC_CHECK_SIZEOF(unsigned char)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(unsigned short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(unsigned int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(unsigned long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(unsigned long long)
AC_CHECK_SIZEOF(float)
AC_CHECK_SIZEOF(double)
AC_CHECK_SIZEOF(long double)
AC_CHECK_SIZEOF(void *)

# Note that aint_size must be used instead of void_p where the desired check
# is on the size of MPI_Aint
aint_size=$ac_cv_sizeof_void_p
MPI_AINT=int
for type in int long long_long short ; do
    eval len=\$ac_cv_sizeof_$type
    if test "$len" = "$aint_size" ; then
        MPI_AINT=`echo $type | sed -e 's/_/ /'`
    fi
done
AC_SUBST(MPI_AINT)
])
