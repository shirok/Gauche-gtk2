/*
 * gldgLink.c
 *
 * OpenGL Graph Display link module implementation
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
#define _EIGHTH(x)                      ((x) * 0.125)
#define _QUARTER(x)                     ((x) * 0.250)
#define _HALF(x)                        ((x) * 0.500)
#define _THREEQUARTER(x)                ((x) * 0.750)

/*
 * External public functions
 */
glgdLink
*glgdLinkCreate(void)
{
    glgdLink    *link;
    
    link = (glgdLink *)GLGD_MALLOC(sizeof(glgdLink));
    if (link)
    {
        glgdLinkInit(link);
    }
    
    return link;
}

glgdLink
*glgdLinkDestroy(glgdLink *link)
{
    if (link != NULL)
    {
        GLGD_FREE(link);
    }
        
    return (glgdLink *)NULL;
}

glgdLink
*glgdLinkByNdx(glgdLink *head, int ndx)
{
    int         n;
    glgdLink    *link;
    
    if (head != NULL)
    {
        n = 0;
        link = head;
        while (link)
        {
            if (n == ndx)
            {
                return link;
            }
            
            link = link->next;
            n++;
        }
    }
    
    return NULL;
}

int
glgdLinkNdx(glgdLink *head, glgdLink *link)
{
    int         n;
    glgdLink    *l;
    
    if (head != NULL)
    {
        n = 0;
        l = head;
        while (l)
        {
            if (l == link)
            {
                return n;
            }
            
            l = l->next;
            n++;
        }
    }

    return -1;
}

GLboolean
glgdLinkInit(glgdLink *link)
{
    if (link != NULL)
    {
        link->flags = GLGDLINK_FLAG_INITIALIZED;
        link->src = NULL;
        link->dst = NULL;
        link->next = NULL;
        link->prev = NULL;
    
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkDraw(glgdLink *link, glgdVec2 dim, GLenum renderMode)
{
    GLdouble    mid;
    glgdVec2    parentPos;
    glgdVec2    childPos;
    glgdNode    *node, *child;
    
    if (link != NULL)
    {
        node = link->src;
        child = link->dst;

        if (link->flags & GLGDLINK_FLAG_LOOPBACK)
        {
            glPushAttrib(GL_CURRENT_BIT);
            glColor3d(0.6, 0.0, 0.2);
                
            if (node->pos[0] < child->pos[0])
            {
                parentPos[0] = node->pos[0] + dim[0] - GLGD_EIGHTH(dim[0]);
                parentPos[1] = node->pos[1] + dim[1];

                childPos[0] = child->pos[0] + dim[0];
                childPos[1] = child->pos[1] + GLGD_HALF(dim[1]);
                if (renderMode == GL_SELECT)
                {
                    glPushName(node->id);
                }
                glBegin(GL_LINES);
                    glVertex2d(parentPos[0], parentPos[1]);
                    glVertex2d(parentPos[0], childPos[1]);
                glEnd();
                if (renderMode == GL_SELECT)
                {
                    glPopName();
                    glPushName(child->id);
                }
                glBegin(GL_LINES);
                    glVertex2d(parentPos[0], childPos[1]);
                    glVertex2d(childPos[0], childPos[1]);
                glEnd();
                if (renderMode == GL_SELECT)
                {
                    glPopName();
                }
            }
            else
            {
                parentPos[0] = node->pos[0] + dim[0];
                parentPos[1] = node->pos[1] + GLGD_HALF(dim[1]);

                childPos[0] = child->pos[0] + dim[0];
                childPos[1] = child->pos[1] + GLGD_HALF(dim[1]);
                if (renderMode == GL_SELECT)
                {
                    glPushName(node->id);
                }
                glBegin(GL_LINE_STRIP);
                    glVertex2d(parentPos[0], parentPos[1]);
                    glVertex2d(parentPos[0] + 8.0, parentPos[1]);
                    glVertex2d(parentPos[0] + 8.0, childPos[1]);
                glEnd();
                if (renderMode == GL_SELECT)
                {
                    glPopName();
                }
                if (renderMode == GL_SELECT)
                {
                    glPushName(child->id);
                }
                glBegin(GL_LINES);
                    glVertex2d(parentPos[0] + 8.0, childPos[1]);
                    glVertex2d(childPos[0], childPos[1]);
                glEnd();
                if (renderMode == GL_SELECT)
                {
                    glPopName();
                }
            }
            glPopAttrib();
        }
        else
        {
            parentPos[0] = node->pos[0] + GLGD_EIGHTH(dim[0]);
            parentPos[1] = node->pos[1];

            childPos[0] = child->pos[0];
            childPos[1] = child->pos[1] + GLGD_HALF(dim[1]);

            if (renderMode == GL_SELECT)
            {
                glPushName(node->id);
            }
            glBegin(GL_LINES);
                glVertex2d(parentPos[0], parentPos[1]);
                glVertex2d(parentPos[0], childPos[1]);
            glEnd();
            if (renderMode == GL_SELECT)
            {
                glPopName();
                glPushName(child->id);
            }
            glBegin(GL_LINES);
                glVertex2d(parentPos[0], childPos[1]);
                glVertex2d(childPos[0], childPos[1]);
            glEnd();
            if (renderMode == GL_SELECT)
            {
                glPopName();
            }
        }

        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkSet(glgdLink *link, glgdNode *src, glgdNode *dst)
{
    if (link != NULL)
    {
        link->src = src;
        link->dst = dst;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkAdd(glgdLink *head, glgdLink *link)
{
    glgdLink        *l;
    
    if (head && link)
    {
        l = head;
        while (l->next)
        {
            l = l->next;
        }
        
        l->next = link;
        link->prev = l;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkDel(glgdLink *head, glgdLink *link)
{
    glgdLink        *l;
    
    if (head && link)
    {
        l = head;
        while (l->next && l->next != link)
        {
            l = l->next;
        }
        
        if (l->next == link)
        {
            l->next = l->next->next;
            if (l->next)
            {
                l->next->prev = l;
            }
            
            return GL_TRUE;
        }
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkDump(glgdLink *head)
{
    int         ndx;
    glgdLink    *l;
    
    ndx = 0;
    l = head;
    while (l)
    {
        printf("%03d: %s->%s\n", ndx, l->src->label, l->dst->label);
        
        l = l->next;
        ndx++;
    }
    
    return GL_TRUE;
}

GLboolean
glgdLinkFlagsSet(glgdLink *link, GLuint flagMask, glgdFlagOp op)
{
    if (link && op < GLGD_FLAGOP_COUNT)
    {
        if (op == GLGD_FLAGOP_CLEAR)
        {
            link->flags &= ~flagMask;
        }
        else if (op == GLGD_FLAGOP_SET)
        {
            link->flags |= flagMask;
        }
        else if (op == GLGD_FLAGOP_TOGGLE)
        {
            link->flags ^= flagMask;
        }
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

glgdLinkList
*glgdLinkListCreate(void)
{
    glgdLinkList    *list;
    
    list = (glgdLinkList *)GLGD_MALLOC(sizeof(glgdLinkList));
    if (list)
    {
        glgdLinkListInit(list);
    }
    
    return list;
}

glgdLinkList
*glgdLinkListDestroy(glgdLinkList *list)
{
    if (list != NULL)
    {
        GLGD_FREE(list);
    }
        
    return (glgdLinkList *)NULL;
}

glgdLinkList
*glgdLinkListByNdx(glgdLinkList *head, int ndx)
{
    int             n;
    glgdLinkList    *list;
    
    if (head != NULL)
    {
        n = 0;
        list = head;
        while (list)
        {
            if (n == ndx)
            {
                return list;
            }
            
            list = list->next;
            n++;
        }
    }
    
    return NULL;
}

int
glgdLinkListNdx(glgdLinkList *head, glgdLinkList *list)
{
    int             n;
    glgdLinkList    *l;
    
    if (head != NULL)
    {
        n = 0;
        l = head;
        while (l)
        {
            if (l == list)
            {
                return n;
            }
            
            l = l->next;
            n++;
        }
    }

    return -1;
}

GLboolean
glgdLinkListInit(glgdLinkList *list)
{
    if (list != NULL)
    {
        list->flags = GLGDLINKLIST_FLAG_INITIALIZED;
        list->pos[0] = 0.0;
        list->pos[1] = 0.0;
        list->linkHead = NULL;
        list->next = NULL;
        list->prev = NULL;
    
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkListAdd(glgdLinkList *head, glgdLinkList *list)
{
    glgdLinkList    *l;
    
    if (head && list)
    {
        l = head;
        while (l->next)
        {
            l = l->next;
        }
        
        l->next = list;
        list->prev = l;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkListDel(glgdLinkList *head, glgdLinkList *list)
{
    glgdLinkList    *l;
    
    if (head && list)
    {
        l = head;
        while (l->next && l->next != list)
        {
            l = l->next;
        }
        
        if (l->next == list)
        {
            l->next = l->next->next;
            if (l->next)
            {
                l->next->prev = l;
            }
            
            return GL_TRUE;
        }
    }
    
    return GL_FALSE;
}

GLboolean
glgdLinkListDump(glgdLinkList *head)
{
    int             ndx;
    glgdLinkList    *l;
    
    ndx = 0;
    l = head;
    while (l)
    {
        printf("%03d: [%6.1f,%6.1f]\n", ndx, l->pos[0], l->pos[1]);
        printf("--------------------\n");
        glgdLinkDump(l->linkHead);
        
        l = l->next;
        ndx++;
    }
    
    return GL_TRUE;
}

GLboolean
glgdLinkListFlagsSet(glgdLinkList *list, GLuint flagMask, glgdFlagOp op)
{
    if (list && op < GLGD_FLAGOP_COUNT)
    {
        if (op == GLGD_FLAGOP_CLEAR)
        {
            list->flags &= ~flagMask;
        }
        else if (op == GLGD_FLAGOP_SET)
        {
            list->flags |= flagMask;
        }
        else if (op == GLGD_FLAGOP_TOGGLE)
        {
            list->flags ^= flagMask;
        }
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}
