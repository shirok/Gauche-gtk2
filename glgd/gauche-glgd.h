/*
 * gauche-glgd.h - Gauche+openGLGraphDisplay extension
 *
 *  Copyright(C) 2004 by Shawn Taras (staras@cementedminds.com)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 */

#ifndef GAUCHE_GLGD_H
#define GAUCHE_GLGD_H

#include <gtkglext-1.0/gdk/gdkgl.h>
#include <gtkglext-1.0/gtk/gtkgl.h>
#include <GL/gl.h>
#include "../src/gauche-gtk.h"
#include "../src/gtk-config.h"
#include "glgd.h"

#ifdef HAVE_GTKGL


SCM_DECL_BEGIN

/*====================================================================
 * glgd classes
 */

/*-------------------------
 *  Scm_GLGDNode
 */
typedef struct _Scm_GLGDNode
{
    SCM_HEADER;
    glgdNode    *node;
} Scm_GLGDNode;

SCM_CLASS_DECL(Scm_GLGDNodeClass);
#define SCM_CLASS_GLGD_NODE           (&Scm_GLGDNodeClass)
#define SCM_GLGD_NODE_P(obj)          (Scm_TypeP(obj, SCM_CLASS_GLGD_NODE))
#define SCM_GLGD_NODE(obj)            (Scm_GLGDNodeUnbox((Scm_GLGDNode *)obj))
#define SCM_MAKE_GLGD_NODE(obj)       ((ScmObj)Scm_GLGDNodeBox(obj))
#define SCM_GLGD_NODE_OR_NULL_P(obj)  (SCM_FALSEP(obj)||SCM_GLGD_NODE_P(obj))

/*-------------------------
 * Scm_GLGDLink
 */
typedef struct _Scm_GLGDLink
{
    SCM_HEADER;
    glgdLink    *link;
} Scm_GLGDLink;

SCM_CLASS_DECL(Scm_GLGDLinkClass);
#define SCM_CLASS_GLGD_LINK           (&Scm_GLGDLinkClass)
#define SCM_GLGD_LINK_P(obj)          (Scm_TypeP(obj, SCM_CLASS_GLGD_LINK))
#define SCM_GLGD_LINK(obj)            (Scm_GLGDLinkUnbox((Scm_GLGDLink *)obj))
#define SCM_MAKE_GLGD_LINK(obj)       ((ScmObj)Scm_GLGDLinkBox(obj))
#define SCM_GLGD_LINK_OR_NULL_P(obj)  (SCM_FALSEP(obj)||SCM_GLGD_LINK_P(obj))

/*-------------------------
 * Scm_GLGDLinkList
 */
typedef struct _Scm_GLGDLinkList
{
    SCM_HEADER;
    glgdLinkList    *list;
} Scm_GLGDLinkList;

SCM_CLASS_DECL(Scm_GLGDLinkListClass);
#define SCM_CLASS_GLGD_LINKLIST          (&Scm_GLGDLinkListClass)
#define SCM_GLGD_LINKLIST_P(obj)         (Scm_TypeP(obj, SCM_CLASS_GLGD_LINKLIST))
#define SCM_GLGD_LINKLIST(obj)           (Scm_GLGDLinkListUnbox((Scm_GLGDLinkList *)obj))
#define SCM_MAKE_GLGD_LINKLIST(obj)      ((ScmObj)Scm_GLGDLinkListBox(obj))
#define SCM_GLGD_LINKLIST_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GLGD_LINKLIST_P(obj))

/*-------------------------
 * Scm_GLGDGraph
 */
typedef struct _Scm_GLGDGraph
{
    SCM_HEADER;
    glgdGraph   *graph;
} Scm_GLGDGraph;

SCM_CLASS_DECL(Scm_GLGDGraphClass);
#define SCM_CLASS_GLGD_GRAPH          (&Scm_GLGDGraphClass)
#define SCM_GLGD_GRAPH_P(obj)         (Scm_TypeP(obj, SCM_CLASS_GLGD_GRAPH))
#define SCM_GLGD_GRAPH(obj)           (Scm_GLGDGraphUnbox((Scm_GLGDGraph *)obj))
#define SCM_MAKE_GLGD_GRAPH(obj)      ((ScmObj)Scm_GLGDGraphBox(obj))
#define SCM_GLGD_GRAPH_OR_NULL_P(obj) (SCM_FALSEP(obj)||SCM_GLGD_GRAPH_P(obj))

/*-------------------------
 * Function Prototypes
 */
glgdNode            *Scm_GLGDNodeUnbox(Scm_GLGDNode *Scm_node);
Scm_GLGDNode        *Scm_GLGDNodeBox(glgdNode *node);
glgdLink            *Scm_GLGDLinkUnbox(Scm_GLGDLink *Scm_link);
Scm_GLGDLink        *Scm_GLGDLinkBox(glgdLink *link);
glgdLinkList        *Scm_GLGDLinkListUnbox(Scm_GLGDLinkList *Scm_list);
Scm_GLGDLinkList    *Scm_GLGDLinkListBox(glgdLinkList *list);
glgdGraph           *Scm_GLGDGraphUnbox(Scm_GLGDGraph *Scm_graph);
Scm_GLGDGraph       *Scm_GLGDGraphBox(glgdGraph *graph);

SCM_DECL_END

#endif /*HAVE_GTKGL*/
#endif /*GAUCHE_GLGD_H*/
