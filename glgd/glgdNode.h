/*
 * gldgNode.h
 *
 * OpenGL Graph Display node module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDNODE_H__
#define __GLGDNODE_H__

SCM_DECL_BEGIN

/*
 * Defines
 */
#define GLGDNODE_FLAG_INITIALIZED       (0x0001)
#define GLGDNODE_FLAG_TOUCHED           (0x0002)
#define GLGDNODE_FLAG_HILITE            (0x0004)
#define GLGDNODE_FLAG_DIM               (0x0008)
#define GLGDNODE_FLAG_SELECTED          (0x0010)

#define GLGDNODE_LABEL_MAX              (64)

/*
 * Type Definitions
 */
typedef struct _glgdNode
{
    GLuint              flags;
    char                label[GLGDNODE_LABEL_MAX];
    int                 id;
    glgdVec2            pos;
    glgdColor           col;
    glgdBitfield        attributes;
    ScmObj              data;
    struct _glgdNode    *next;
    struct _glgdNode    *prev;
} glgdNode;

/*
 * Module API
 */
glgdNode    *glgdNodeCreate(void);
glgdNode    *glgdNodeDestroy(glgdNode *node);
glgdNode    *glgdNodeByID(glgdNode *nodeList, int id);
GLboolean   glgdNodeInit(glgdNode *node);

GLboolean   glgdNodeDraw(glgdNode *node, glgdVec2 dim, ScmObj prFn,
                         GLenum renderMode);
GLboolean   glgdNodeLabelSet(glgdNode *node, const char *label);
ScmObj      glgdNodeLabelGet(glgdNode *node);
GLboolean   glgdNodeDataSet(glgdNode *node, ScmObj data);
ScmObj      glgdNodeDataGet(glgdNode *node);
GLboolean   glgdNodeIDSet(glgdNode *node, int id);
int         glgdNodeIDGet(glgdNode *node);
GLboolean   glgdNodeInfoSet(glgdNode *node, const char *label, int id);
GLboolean   glgdNodeTranslate(glgdNode *nodeList, glgdVec2 xlat,
                              glgdVec2 dim, glgdVec4 extents);
GLboolean   glgdNodeFlagsSet(glgdNode *node, GLuint flagMask, glgdFlagOp op);
GLboolean   glgdNodeIsSelected(glgdNode *node);
GLboolean   glgdNodeIsTouched(glgdNode *node);

void        glgdNodeColorDefault(GLdouble r, GLdouble g, GLdouble b, GLdouble a);
GLboolean   glgdNodeColorSetByList(glgdNode *node,
                                   GLdouble r, GLdouble g, GLdouble b, GLdouble a);
GLboolean   glgdNodePosSet(glgdNode *node, glgdVec2 pos, glgdVec2 dim,
                           glgdVec4 extents);
GLboolean   glgdNodePosSetByList(glgdNode *node, GLdouble x, GLdouble y,
                                 glgdVec2 dim, glgdVec4 extents);

GLboolean   glgdNodeAttributeClear(glgdNode *node);
GLboolean   glgdNodeAttributeSet(glgdNode *node, int attrNdx);
GLboolean   glgdNodeAttributeReset(glgdNode *node, int attrNdx);
GLboolean   glgdNodeAttributeIsSet(glgdNode *node, int attrNdx);

SCM_DECL_END

#endif  /* __GLGDNODE_H__ */
