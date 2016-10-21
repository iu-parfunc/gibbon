#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(sexpr, 1.3, mjsottile@me.com)
AM_INIT_AUTOMAKE(foreign)
AC_OUTPUT([Makefile src/Makefile examples/Makefile tests/Makefile])
AC_CONFIG_SRCDIR([examples/binmode.c])
AC_CONFIG_HEADER([config.h])

## FROM: http://lists.gnu.org/archive/html/automake/2008-09/msg00075.html
SX_CPPFLAGS="$CPPFLAGS -DNODEBUG"
SX_CFLAGS="$CFLAGS -O3 -Wall"
AC_ARG_ENABLE(debug,
   [AS_HELP_STRING([--enable-debug],[turn on debug options and code (disabled by default)])],
   [SX_CPPFLAGS="" SX_CFLAGS="$CFLAGS -g -O0 -Wall"],
   [])
 

# Export flags
AC_SUBST([SFSEXP_CPPFLAGS], $SX_CPPFLAGS)
AC_SUBST([SFSEXP_CFLAGS], $SX_CFLAGS)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_RANLIB

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([fcntl.h stddef.h stdlib.h string.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_REALLOC
AC_FUNC_STRTOD
AC_CHECK_FUNCS([gettimeofday pow sqrt])

AC_OUTPUT
