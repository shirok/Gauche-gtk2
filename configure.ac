dnl
dnl Configuring gauche-gtk2
dnl  process this file with autoconf to generate 'configure'.
dnl
AC_PREREQ(2.54)
AC_INIT(Gauche-gtk2, 0.6, shiro@acm.org)

AC_CONFIG_HEADER(src/gtk-config.h)
AC_CANONICAL_SYSTEM

dnl Replace @LOCAL_PATHS@ by --with-local option.
AC_ARG_WITH(local,
  AC_HELP_STRING([--with-local=PATH:PATH...],
                 [For each PATH, add PATH/include to the include search
paths and PATH/lib to the library search paths.  Useful if you have some
libraries installed in non-standard places. ]),
  [
    case $with_local in
      yes|no|"") ;;  #no effect
      *) LOCAL_PATHS=$with_local ;;
    esac
  ])
AC_SUBST(LOCAL_PATHS)

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

dnl Set up gauche related commands.  The commands are set by scanning
dnl PATH.  You can override them by "GOSH=/my/gosh ./configure" etc.
AC_PATH_PROG([GOSH], gosh)
AC_PATH_PROG([GAUCHE_CONFIG], gauche-config)
AC_PATH_PROG([GAUCHE_PACKAGE], gauche-package)
AC_PATH_PROG([GAUCHE_INSTALL], gauche-install)
AC_PATH_PROG([GAUCHE_CESCONV], gauche-cesconv)

dnl Usually these parameters are set by AC_PROG_CC, but we'd rather use
dnl the same one as Gauche has been compiled with.
SOEXT=`$GAUCHE_CONFIG --so-suffix`
OBJEXT=`$GAUCHE_CONFIG --object-suffix`
EXEEXT=`$GAUCHE_CONFIG --executable-suffix`
AC_SUBST(SOEXT)
AC_SUBST(OBJEXT)
AC_SUBST(EXEEXT)

ac_default_prefix=`$GAUCHE_CONFIG --prefix`

GAUCHE_PKGINCDIR=`$GAUCHE_CONFIG --pkgincdir`
GAUCHE_PKGLIBDIR=`$GAUCHE_CONFIG --pkglibdir`
GAUCHE_PKGARCHDIR=`$GAUCHE_CONFIG --pkgarchdir`
AC_SUBST(GAUCHE_PKGINCDIR)
AC_SUBST(GAUCHE_PKGLIBDIR)
AC_SUBST(GAUCHE_PKGARCHDIR)

# Check for other programs.

# Check for libraries
AC_PATH_XTRA

# Prepare file list.
gen_files=`cat src/GDKFILES src/GDKPIXBUFFILES src/GTKFILES src/PANGOFILES`
GEN_OBJS="`echo $gen_files | sed 's/\.h/.$(OBJEXT)/g'`"
AC_SUBST(GEN_OBJS)
GEN_SRCS="`echo $gen_files | sed 's/\.h/.c/g'`"
AC_SUBST(GEN_SRCS)
GEN_STUBS="`echo $gen_files | sed 's/\.h/.stub/g'`"
AC_SUBST(GEN_STUBS)

dnl Creating gpd (gauche package description) file
GAUCHE_PACKAGE_CONFIGURE_ARGS="`echo ""$ac_configure_args"" | sed 's/[\\""\`\$]/\\\&/g'`"
AC_MSG_NOTICE([creating ${PACKAGE_NAME}.gpd])
$GAUCHE_PACKAGE make-gpd "$PACKAGE_NAME" \
  -version "$PACKAGE_VERSION" \
  -configure "./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS"

# Output
echo $PACKAGE_VERSION > VERSION
AC_OUTPUT(Makefile src/Makefile lib/Makefile gtkgl/Makefile glgd/Makefile)
