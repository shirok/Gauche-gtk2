/*
 * gauche-glgd.c - Gauche+openGLGraphDisplay extension
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
 *
 *  $Id: gauche-glgd.c,v 1.9 2007/01/13 01:36:30 maruska Exp $
 */

#include "gauche-glgd.h"

#ifdef HAVE_GTKGL

extern void Scm_Init_glgdlib(ScmModule *);

/*=============================================================
 * Scm_GLGDNode
 */
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GLGDNodeClass, NULL);

glgdNode
*Scm_GLGDNodeUnbox(Scm_GLGDNode *Scm_node)
{
    return Scm_node->node;
}

Scm_GLGDNode
*Scm_GLGDNodeBox(glgdNode *node)
{
    Scm_GLGDNode    *Scm_node;
    
    Scm_node = SCM_NEW(Scm_GLGDNode);
    SCM_SET_CLASS(Scm_node, SCM_CLASS_GLGD_NODE);
    Scm_node->node = (glgdNode *)node;
    
    return Scm_node;
}

/*=============================================================
 * Scm_GLGDLink
 */
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GLGDLinkClass, NULL);

glgdLink
*Scm_GLGDLinkUnbox(Scm_GLGDLink *Scm_link)
{
    return Scm_link->link;
}

Scm_GLGDLink
*Scm_GLGDLinkBox(glgdLink *link)
{
    Scm_GLGDLink    *Scm_link;
    
    Scm_link = SCM_NEW(Scm_GLGDLink);
    SCM_SET_CLASS(Scm_link, SCM_CLASS_GLGD_LINK);
    Scm_link->link = (glgdLink *)link;
    
    return Scm_link;
}

/*==========================================================
 * Scm_GLGDLinkList
 */
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GLGDLinkListClass, NULL);

glgdLinkList
*Scm_GLGDLinkListUnbox(Scm_GLGDLinkList *Scm_list)
{
    return Scm_list->list;
}

Scm_GLGDLinkList
*Scm_GLGDLinkListBox(glgdLinkList *list)
{
    Scm_GLGDLinkList    *Scm_list;
    
    Scm_list = SCM_NEW(Scm_GLGDLinkList);
    SCM_SET_CLASS(Scm_list, SCM_CLASS_GLGD_LINKLIST);
    Scm_list->list = (glgdLinkList *)list;
    
    return Scm_list;
}

/*==========================================================
 * Scm_GLGDGraph
 */
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GLGDGraphClass, NULL);

glgdGraph
*Scm_GLGDGraphUnbox(Scm_GLGDGraph *Scm_graph)
{
    return Scm_graph->graph;
}

Scm_GLGDGraph
*Scm_GLGDGraphBox(glgdGraph *graph)
{
    Scm_GLGDGraph   *Scm_graph;
    
    Scm_graph = SCM_NEW(Scm_GLGDGraph);
    SCM_SET_CLASS(Scm_graph, SCM_CLASS_GLGD_GRAPH);
    Scm_graph->graph = (glgdGraph *)graph;
    
    return Scm_graph;
}

/*==========================================================
 * Initialization
 */
void
Scm_Init_gauche_glgd(void)
{
    ScmModule   *mod;
    
    SCM_INIT_EXTENSION(gauche_glgd);
    mod = SCM_MODULE(SCM_FIND_MODULE("gtk.glgd", TRUE));
    Scm_InitBuiltinClass(SCM_CLASS_GLGD_NODE, "<glgd-node>", NULL,
                         sizeof(Scm_GLGDNode), mod);
    Scm_InitBuiltinClass(SCM_CLASS_GLGD_LINK, "<glgd-link>", NULL,
                         sizeof(Scm_GLGDLink), mod);
    Scm_InitBuiltinClass(SCM_CLASS_GLGD_GRAPH, "<glgd-graph>", NULL,
                         sizeof(Scm_GLGDGraph), mod);
    Scm_Init_glgdlib(mod);
}

#endif /*HAVE_GTKGL*/
