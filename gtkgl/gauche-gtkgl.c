/*
 * gauche-gtkgl.h - Gauche+Gtkglarea extension
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
 */

#include "gauche-gtkgl.h"

#ifdef HAVE_GTKGL

/*====================================================================
 * Basic data converters
 */

int *Scm_ListToGdkGLAttribList(ScmObj attrib)
{
    int len = Scm_Length(attrib), *alist, i;
    ScmObj ap;
    if (len < 0 || (len % 2) != 0) {
        Scm_Error("attribute list must have even number of elements, but got %S",
                  attrib);
    }
    alist = SCM_NEW_ATOMIC2(int*, sizeof(int)*(len+1));
    i = 0;
    SCM_FOR_EACH(ap, attrib) {
        if (!SCM_EXACTP(SCM_CAR(ap))) {
            Scm_Error("integer expected in attibute list, but got %S",
                      SCM_CAR(ap));
        }
        alist[i++] = Scm_GetInteger(SCM_CAR(ap));
    }
    alist[i] = GDK_GL_ATTRIB_LIST_NONE;
    return alist;
}

/*
 * Initialization
 */

extern void Scm_Init_gdkgllib(ScmModule *);
extern void Scm_Init_gtkgllib(ScmModule *);

void Scm_Init_gauche_gtkgl(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_gtkgl);
    mod = SCM_MODULE(SCM_FIND_MODULE("gtk.gtkgl", TRUE));
    Scm_Init_gdkgllib(mod);
    Scm_Init_gtkgllib(mod);
}

#endif /*HAVE_GTKGL*/
