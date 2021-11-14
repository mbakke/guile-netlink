#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-netlink], [0.1], [julien@lepiller.eu])
AM_INIT_AUTOMAKE([-Wall -Werror foreign])

# Checks for programs.
GUILE_PKG([3.0 2.2])
GUILE_PROGS
GUILE_SITE_DIR

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES(Makefile)
AC_PROG_AWK

AC_OUTPUT
