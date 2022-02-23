/*
 * gldgNode.c
 *
 * OpenGL Graph Display node module implementation
 *
 * Written by: Shawn Taras
 */
#include <stdlib.h>
#include <math.h>
#include <GL/gl.h>
#include "glgd.h"
#include "gauche-glgd.h"

/*
 * Defines
 */
#define _BW                             (1.0)   /* Border width for node box*/

/*
 * Static local (to this module) variables
 */
static glgdColor        s_nodeColorDefault = {0.45, 0.45, 0.45, 1.00};

/*
 * Static local (to this module) functions
 */
static GLdouble
glgdNodeTotal(glgdNode *node)
{
    GLdouble    total;
    glgdNode    *n;

    total = 0.0;
    n = node;
    while (n)
    {
        total += 1.0;

        n = n->next;
    }

    return total;
}

static GLboolean
glgdNodeDrawBox(glgdNode *node, glgdVec2 dim, GLenum renderMode)
{
    int         slen;
    glgdVec2    tpos;
    glgdVec4    col;
    glgdStroke  *stroke;

    if (renderMode == GL_SELECT)
    {
        glPushName(node->id);
    }
    col[0] = node->col[0];
    col[1] = node->col[1];
    col[2] = node->col[2];
    col[3] = node->col[3];
    if (node->flags & GLGDNODE_FLAG_HILITE)
    {
        col[0] *= GLGDDRAW_LIGHTEN;
        col[1] *= GLGDDRAW_LIGHTEN;
        col[2] *= GLGDDRAW_LIGHTEN;
    }
    if (node->flags & GLGDNODE_FLAG_DIM)
    {
        col[0] *= GLGDDRAW_DARKEN;
        col[1] *= GLGDDRAW_DARKEN;
        col[2] *= GLGDDRAW_DARKEN;
    }
    if (node->flags & GLGDNODE_FLAG_SELECTED)
    {
        col[0] = 1.0;
        col[1] = 1.0;
        col[2] = 0.0;
    }

    glgdDrawBox(GLGDDRAW_BOXTYPE_UP, node->pos, dim, col, _BW);
    if (renderMode == GL_SELECT)
    {
        glPopName();
    }

#ifndef HAVE_GLGD_PANGO
    /* Draw node label */
    stroke = glgdStrokeGetCurrent();
    if (stroke)
    {
        slen = strlen(node->label) * stroke->pointSize[0];
        tpos[0] = node->pos[0] + GLGD_HALF(dim[0] - slen);
        tpos[1] = node->pos[1] + GLGD_THREEQUARTER(dim[1]);
        glgdStrokePosSet(stroke, tpos);
        glgdStrokePrint(stroke, node->label);
    }
#endif  /* HAVE_GLGD_PANGO */

    return GL_FALSE;
}

static void
glgdNodeExtentsUpdate(glgdVec2 pos, glgdVec2 dim, glgdVec4 extents)
{
    if (pos[0] < extents[0]) extents[0] = pos[0];
    if (pos[1] < extents[1]) extents[1] = pos[1];
    if (pos[0] + dim[0] > extents[2])
    {
        extents[2] = pos[0] + dim[0];
    }
    if (pos[1] + dim[1] > extents[3])
    {
        extents[3] = pos[1] + dim[1];
    }
}

/*
 * External public functions
 */
glgdNode
*glgdNodeCreate(void)
{
    glgdNode    *node;

    node = (glgdNode *)GLGD_MALLOC(sizeof(glgdNode));
    if (node)
    {
        glgdNodeInit(node);
    }

    return node;
}

glgdNode
*glgdNodeDestroy(glgdNode *node)
{
    glgdNode    *next;

    if (node->next)
    {
        node->next = glgdNodeDestroy(node->next);
    }

    GLGD_FREE(node);

    return (glgdNode *)NULL;
}

glgdNode
*glgdNodeByID(glgdNode *nodeList, int id)
{
    glgdNode        *n;

    if (nodeList)
    {
        n = nodeList;
        while (n)
        {
            if (n->id == id)
            {
                return n;
            }

            n = n->next;
        }
    }

    return NULL;
}

GLboolean
glgdNodeInit(glgdNode *node)
{
    if (node != NULL)
    {
        node->flags = GLGDNODE_FLAG_INITIALIZED;
        node->label[0] = '\0';
        node->id = -1;
        node->pos[0] = 0.0;
        node->pos[1] = 0.0;
        node->col[0] = s_nodeColorDefault[0];
        node->col[1] = s_nodeColorDefault[1];
        node->col[2] = s_nodeColorDefault[2];
        node->col[3] = s_nodeColorDefault[3];
        glgdNodeAttributeClear(node);
        glgdNodeAttributeSet(node, GLGD_ATTR_FORCEVISIBLE);
        node->data = NULL;
        node->next = NULL;
        node->prev = NULL;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodeDraw
(
    glgdNode    *node,
    glgdVec2    dim,
    ScmObj      prFn,
    GLenum      renderMode
)
{
    if (node != NULL)
    {
        if (prFn != NULL)
        {
            Scm_ApplyRec(prFn, SCM_LIST1(SCM_OBJ(SCM_MAKE_GLGD_NODE(node))));
        }

        glgdNodeDrawBox(node, dim, renderMode);

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodeLabelSet(glgdNode *node, const char *label)
{
    if (node != NULL)
    {
        strncpy(node->label, label, GLGDNODE_LABEL_MAX-1);
        node->label[GLGDNODE_LABEL_MAX-1] = '\0';

        return GL_TRUE;
    }

    return GL_FALSE;
}

ScmObj
glgdNodeLabelGet(glgdNode *node)
{
    if (node != NULL)
    {
        return SCM_MAKE_STR_COPYING(node->label);
    }

    return SCM_FALSE;
}

GLboolean
glgdNodeDataSet(glgdNode *node, ScmObj data)
{
    if (node != NULL)
    {
        node->data = data;
    }

    return GL_FALSE;
}

ScmObj
glgdNodeDataGet(glgdNode *node)
{
    if (node != NULL)
    {
        return node->data;
    }

    return SCM_FALSE;
}

GLboolean
glgdNodeIDSet(glgdNode *node, int id)
{
    if (node != NULL)
    {
        node->id = id;

        return GL_TRUE;
    }

    return GL_FALSE;
}

int
glgdNodeIDGet(glgdNode *node)
{
    if (node)
    {
        return node->id;
    }

    return -1;
}

GLboolean
glgdNodeInfoSet(glgdNode *node, const char *label, int id)
{
    if (glgdNodeLabelSet(node, label) == GL_TRUE)
    {
        return glgdNodeIDSet(node, id);
    }

    return GL_FALSE;
}

GLboolean
glgdNodeTranslate(glgdNode *node, glgdVec2 xlat, glgdVec2 dim, glgdVec4 extents)
{
    glgdNode    *n;

    if (node && xlat)
    {
        n = node;
        while (n)
        {
            n->pos[0] += xlat[0];
            n->pos[1] += xlat[1];

            glgdNodeExtentsUpdate(n->pos, dim, extents);

            n = n->next;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodeFlagsSet(glgdNode *node, GLuint flagMask, glgdFlagOp op)
{
    if (node && op < GLGD_FLAGOP_COUNT)
    {
        if (op == GLGD_FLAGOP_CLEAR)
        {
            node->flags &= ~flagMask;
        }
        else if (op == GLGD_FLAGOP_SET)
        {
            node->flags |= flagMask;
        }
        else if (op == GLGD_FLAGOP_TOGGLE)
        {
            node->flags ^= flagMask;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodeIsSelected(glgdNode *node)
{
    if (node != NULL)
    {
        if (node->flags & GLGDNODE_FLAG_SELECTED)
        {
            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdNodeIsTouched(glgdNode *node)
{
    if (node != NULL)
    {
        if (node->flags & GLGDNODE_FLAG_TOUCHED)
        {
            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

void
glgdNodeColorDefault(GLdouble r, GLdouble g, GLdouble b, GLdouble a)
{
    s_nodeColorDefault[0] = r;
    s_nodeColorDefault[1] = g;
    s_nodeColorDefault[2] = b;
    s_nodeColorDefault[3] = a;
}

GLboolean
glgdNodeColorSetByList(glgdNode *node,
                       GLdouble r, GLdouble g, GLdouble b, GLdouble a)
{
    if (node)
    {
        node->col[0] = r;
        node->col[1] = g;
        node->col[2] = b;
        node->col[3] = a;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodePosSet(glgdNode *node, glgdVec2 pos, glgdVec2 dim, glgdVec4 extents)
{
    if (node != NULL)
    {
        node->pos[0] = pos[0];
        node->pos[1] = pos[1];

        glgdNodeExtentsUpdate(node->pos, dim, extents);

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodePosSetByList(glgdNode *node,
                     GLdouble x, GLdouble y, glgdVec2 dim, glgdVec4 extents)
{
    if (node != NULL)
    {
        node->pos[0] = x;
        node->pos[1] = y;

        glgdNodeExtentsUpdate(node->pos, dim, extents);

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdNodeAttributeClear(glgdNode *node)
{
    int         i;

    if (node != NULL)
    {
        return glgdBitfieldClear(&node->attributes);
    }

    return GL_FALSE;
}

GLboolean
glgdNodeAttributeSet(glgdNode *node, int attrNdx)
{
    if (node)
    {
        return glgdBitfieldSet(&node->attributes, attrNdx);
    }

    return GL_FALSE;
}

GLboolean
glgdNodeAttributeReset(glgdNode *node, int attrNdx)
{
    if (node)
    {
        return glgdBitfieldReset(&node->attributes, attrNdx);
    }

    return GL_FALSE;
}

GLboolean
glgdNodeAttributeIsSet(glgdNode *node, int attrNdx)
{
    if (node)
    {
        return glgdBitfieldIsSet(&node->attributes, attrNdx);
    }

    return GL_FALSE;
}
