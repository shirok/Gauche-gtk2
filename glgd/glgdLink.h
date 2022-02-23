/*
 * gldgLink.h
 *
 * OpenGL Graph Display link module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDLINK_H__
#define __GLGDLINK_H__

SCM_DECL_BEGIN

/*
 * Defines
 */
#define GLGDLINK_FLAG_INITIALIZED           (0x0001)
#define GLGDLINK_FLAG_LOOPBACK              (0x0002)
#define GLGDLINK_FLAG_LONER                 (0x0004)

#define GLGDLINKLIST_FLAG_INITIALIZED       (0x0001)
#define GLGDLINKLIST_FLAG_VISIBLE           (0x0002)

/*
 * Type Definitions
 */
typedef struct _glgdLink
{
    GLuint                  flags;
    glgdNode                *src;
    glgdNode                *dst;
    struct _glgdLink        *next;
    struct _glgdLink        *prev;
} glgdLink;

typedef struct _glgdLinkList
{
    GLuint                  flags;
    glgdVec2                pos;
    glgdLink                *linkHead;
    struct _glgdLinkList    *next;
    struct _glgdLinkList    *prev;
} glgdLinkList;

/*
 * <glgdLink> API
 */
glgdLink    *glgdLinkCreate(void);
glgdLink    *glgdLinkDestroy(glgdLink *link);
glgdLink    *glgdLinkByNdx(glgdLink *head, int ndx);
int         glgdLinkNdx(glgdLink *head, glgdLink *link);
GLboolean   glgdLinkInit(glgdLink *link);
GLboolean   glgdLinkDraw(glgdLink *link, glgdVec2 dim, GLenum renderMode);

GLboolean   glgdLinkSet(glgdLink *link, glgdNode *src, glgdNode *dst);
GLboolean   glgdLinkAdd(glgdLink *head, glgdLink *link);
GLboolean   glgdLinkDel(glgdLink *head, glgdLink *link);
GLboolean   glgdLinkDump(glgdLink *head);

GLboolean   glgdLinkFlagsSet(glgdLink *link, GLuint flagMask, glgdFlagOp op);

/*
 * <glgdLinkList> API
 */
glgdLinkList    *glgdLinkListCreate(void);
glgdLinkList    *glgdLinkListDestroy(glgdLinkList *list);
glgdLinkList    *glgdLinkListByNdx(glgdLinkList *head, int ndx);
int             glgdLinkListNdx(glgdLinkList *head, glgdLinkList *list);
GLboolean       glgdLinkListInit(glgdLinkList *list);

GLboolean       glgdLinkListAdd(glgdLinkList *head, glgdLinkList *list);
GLboolean       glgdLinkListDel(glgdLinkList *head, glgdLinkList *list);
GLboolean       glgdLinkListDump(glgdLinkList *head);

GLboolean       glgdLinkListFlagsSet(glgdLinkList *list,
                                     GLuint flagMask, glgdFlagOp op);

SCM_DECL_END

#endif  /* __GLGDLINK_H__ */
