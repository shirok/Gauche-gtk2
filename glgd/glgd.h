/*
 * glgd.h
 *
 * OpenGL Graph Display library header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGD_H__
#define __GLGD_H__

/* Trace output functions (implemented in glgdGraph.c) */
int         glgdVerbosity(int verbosity);
int         glgdTrace(int level, const char *fmt, ...);

#include <gtk/gtk.h>
#include <gtk/gtkgl.h>
#include <gdk/gdkkeysyms.h>
#include <gauche.h>

#ifdef HAVE_GLGD_PANGO
#include <pango/pangoft2.h>
#endif  /* HAVE_GLGD_PANGO */

#include "glgdDefines.h"
#include "glgdTypes.h"
#include "glgdBitfield.h"
#include "glgdStroke.h"
#include "glgdTexture.h"
#include "glgdQuat.h"
#include "glgdMatrix.h"
#include "glgdCam.h"
#include "glgdDraw.h"
#include "glgdNode.h"
#include "glgdLink.h"
#include "glgdGraph.h"

#endif  /* __GLGD_H__ */
