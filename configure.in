dnl
dnl Configuring gauche-gtk
dnl  process this file with "autoconf -I `gauche-config --ac`" to
dnl  generate 'configure'.
dnl $Id: configure.in,v 1.19 2007/01/13 01:36:29 maruska Exp $
AC_PREREQ(2.54)
AC_INIT(Gauche-gtk, 0.4.1, shiro@acm.org)
AC_REVISION($Revision: 1.19 $)
AC_CONFIG_HEADER(src/gtk-config.h)
AC_GAUCHE_INIT_EXT
AC_CANONICAL_SYSTEM

# options for configure scripts
AC_ARG_ENABLE(gtkgl,
   AC_HELP_STRING([--enable-gtkgl], [Enable gtkglext interface.]), [
   AC_DEFINE(HAVE_GTKGL, 1)
   GTKGL_SO="gauche-gtkgl.so"
   GTKGL_LIB="-lgtkgl";
   GTKGL_SCM="gtk/gtkgl.scm";
   PANGOFT2_LIB=""
])
AC_ARG_ENABLE(glgd,
   AC_HELP_STRING([--enable-glgd], [Enable glgd interface.]), [
   AC_DEFINE(HAVE_GTKGL, 1)
   AC_DEFINE(HAVE_GLGD, 1)
   GTKGL_SO="gauche-gtkgl.so"
   GTKGL_LIB="-lgtkgl";
   GTKGL_SCM="gtk/gtkgl.scm"
   PANGOFT2_LIB="";
   GLGD_SO="gauche-glgd.so"
   GLGD_LIB="-lglgd";
   GLGD_SCM="gtk/glgd.scm"
])
AC_ARG_ENABLE(glgd-pango,
   AC_HELP_STRING([--enable-glgd-pango], [Enable glgd interface with Pango.]), [
   AC_DEFINE(HAVE_GTKGL, 1)
   AC_DEFINE(HAVE_GLGD, 1)
   AC_DEFINE(HAVE_GLGD_PANGO, 1)
   GTKGL_SO="gauche-gtkgl.so"
   GTKGL_LIB="-lgtkgl";
   GTKGL_SCM="gtk/gtkgl.scm"
   PANGOFT2_LIB="-lpangoft2-1.0";
   GLGD_SO="gauche-glgd.so"
   GLGD_LIB="-lglgd";
   GLGD_SCM="gtk/glgd.scm"
])
AC_SUBST(GTKGL_SO)
AC_SUBST(GTKGL_LIB)
AC_SUBST(GTKGL_SCM)
AC_SUBST(PANGOFT2_LIB)
AC_SUBST(GLGD_SO)
AC_SUBST(GLGD_LIB)
AC_SUBST(GLGD_SCM)

# Sets the default value of INSTALL_TYPE macro.
AC_GAUCHE_INSTALL_TYPE(sys)

# Get compiler parameters which Gauche has been compiled with.
AC_GAUCHE_CC
AC_GAUCHE_FLAGS

# Check for other programs.

# Check for libraries
AC_PATH_XTRA

# Set LDFLAGS to generate shared library.
AC_GAUCHE_FIX_LIBS

# Prepare file list.
gen_files=`cat src/GDKFILES src/GDKPIXBUFFILES src/GTKFILES src/PANGOFILES`
GEN_OBJS="`echo $gen_files | sed 's/\.h/.$(OBJEXT)/g'`"
AC_SUBST(GEN_OBJS)
GEN_SRCS="`echo $gen_files | sed 's/\.h/.c/g'`"
AC_SUBST(GEN_SRCS)
GEN_STUBS="`echo $gen_files | sed 's/\.h/.stub/g'`"
AC_SUBST(GEN_STUBS)

# Output
AC_GAUCHE_MAKE_GPD
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile src/Makefile lib/Makefile gtkgl/Makefile glgd/Makefile)
