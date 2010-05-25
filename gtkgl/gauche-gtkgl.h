/*
 * gauche-gtkgl.h - Gauche+GtkGLExt extension
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: gauche-gtkgl.h,v 1.5 2007/01/13 01:36:30 maruska Exp $
 */

#ifndef GAUCHE_GTKGL_H
#define GAUCHE_GTKGL_H

#include <gtkglext-1.0/gdk/gdkgl.h>
#include <gtkglext-1.0/gtk/gtkgl.h>
#include "../src/gauche-gtk.h"
#include "../src/gtk-config.h"

#ifdef HAVE_GTKGL


SCM_DECL_BEGIN

/*====================================================================
 * Basic data converters
 */

extern int *Scm_ListToGdkGLAttribList(ScmObj attrib);

/*====================================================================
 * gdkgl classes
 */

/* GdkGLConfig */

SCM_CLASS_DECL(Scm_GdkGLConfigClass);
#define SCM_CLASS_GDK_GL_CONFIG     (&Scm_GdkGLConfigClass)
#define SCM_GDK_GL_CONFIG_P(obj)    (Scm_TypeP(obj, SCM_CLASS_GDK_GL_CONFIG))
#define SCM_GDK_GL_CONFIG(obj)      SCM_GOBJECT_UNBOX(GDK_GL_CONFIG, obj)
#define SCM_MAKE_GDK_GL_CONFIG(obj) SCM_GOBJECT_BOX(obj)
#define SCM_GDK_GL_CONFIG_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GDK_GL_CONFIG_P(obj))

/* GdkGLContext */

SCM_CLASS_DECL(Scm_GdkGLContextClass);
#define SCM_CLASS_GDK_GL_CONTEXT     (&Scm_GdkGLContextClass)
#define SCM_GDK_GL_CONTEXT_P(obj)    (Scm_TypeP(obj, SCM_CLASS_GDK_GL_CONTEXT))
#define SCM_GDK_GL_CONTEXT(obj)      SCM_GOBJECT_UNBOX(GDK_GL_CONTEXT, obj)
#define SCM_MAKE_GDK_GL_CONTEXT(obj) SCM_GOBJECT_BOX(obj)
#define SCM_GDK_GL_CONTEXT_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GDK_GL_CONTEXT_P(obj))

/* GdkGLDrawable */

SCM_CLASS_DECL(Scm_GdkGLDrawableClass);
#define SCM_CLASS_GDK_GL_DRAWABLE     (&Scm_GdkGLDrawableClass)
#define SCM_GDK_GL_DRAWABLE_P(obj)    (Scm_TypeP(obj, SCM_CLASS_GDK_GL_DRAWABLE))
#define SCM_GDK_GL_DRAWABLE(obj)      SCM_GOBJECT_UNBOX(GDK_GL_DRAWABLE, obj)
#define SCM_MAKE_GDK_GL_DRAWABLE(obj) SCM_GOBJECT_BOX(obj)
#define SCM_GDK_GL_DRAWABLE_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GDK_GL_DRAWABLE_P(obj))

/* GdkGLPixmap */

SCM_CLASS_DECL(Scm_GdkGLPixmapClass);
#define SCM_CLASS_GDK_GL_PIXMAP     (&Scm_GdkGLPixmapClass)
#define SCM_GDK_GL_PIXMAP_P(obj)    (Scm_TypeP(obj, SCM_CLASS_GDK_GL_PIXMAP))
#define SCM_GDK_GL_PIXMAP(obj)      SCM_GOBJECT_UNBOX(GDK_GL_PIXMAP, obj)
#define SCM_MAKE_GDK_GL_PIXMAP(obj) SCM_GOBJECT_BOX(obj)
#define SCM_GDK_GL_PIXMAP_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GDK_GL_PIXMAP_P(obj))

/* GdkGLWindow */

SCM_CLASS_DECL(Scm_GdkGLWindowClass);
#define SCM_CLASS_GDK_GL_WINDOW     (&Scm_GdkGLWindowClass)
#define SCM_GDK_GL_WINDOW_P(obj)    (Scm_TypeP(obj, SCM_CLASS_GDK_GL_WINDOW))
#define SCM_GDK_GL_WINDOW(obj)      SCM_GOBJECT_UNBOX(GDK_GL_WINDOW, obj)
#define SCM_MAKE_GDK_GL_WINDOW(obj) SCM_GOBJECT_BOX(obj)
#define SCM_GDK_GL_WINDOW_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GDK_GL_WINDOW_P(obj))

SCM_DECL_END

#endif /*HAVE_GTKGL*/
#endif /*GAUCHE_GTK_H*/

