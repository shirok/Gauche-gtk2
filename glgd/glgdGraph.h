/*
 * gldgGraph.h
 *
 * OpenGL Graph Display module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDGRAPH_H__
#define __GLGDGRAPH_H__

SCM_DECL_BEGIN

/*
 * Enumerations
 */
typedef enum
{
    GLGDGRAPH_FN_MOUSE_LEFT = 0,
    GLGDGRAPH_FN_MOUSE_MIDDLE,
    GLGDGRAPH_FN_MOUSE_RIGHT,
    GLGDGRAPH_FN_MOUSE_SCROLL,
    GLGDGRAPH_FN_MOUSE_HOVER,
    GLGDGRAPH_FN_KEY,
    GLGDGRAPH_FN_PRERENDER,
        
    GLGDGRAPH_FN_COUNT
} glgdGraphFnEnum;

/*
 * Defines
 */
#define GLGDGRAPH_FLAG_INITIALIZED    (0x0001)
#define GLGDGRAPH_FLAG_CTRLHELD       (0x0002)
#define GLGDGRAPH_FLAG_ESCPRESSED     (0x0004)
#define GLGDGRAPH_FLAG_PANGOBOLD      (0x0008)

#define GLGDGRAPH_NODEMARGIN_DEFAULT  ( 16.0)
#define GLGDGRAPH_NODEWIDTH_DEFAULT   (106.0)
#define GLGDGRAPH_NODEHEIGHT_DEFAULT  ( 23.0)

/*
 * Type Definitions
 */
typedef struct _glgdGraph
{
    GLbitfield     flags;
    int            nodeCount;
    int            linkCount;
    GLdouble       frameTime;
    GLdouble       margin;
    glgdVec2       dim;
    glgdVec4       extents;     /* [minX, minY, maxX, maxY] */
    glgdColor      lineColor;
    glgdCam        ctrlCam;
    glgdStroke     stroke;
    glgdBitfield   attributes;
    glgdNode       *nodeHead;
    glgdLinkList   *linkListHead;
    glgdNode       *hoverNode;
    glgdLink       *hoverLink;
    GTimer         *timer;
    GtkWidget      *gtkWindow;
    GtkWidget      *gtkGLDrawArea;
    ScmObj         fn[GLGDGRAPH_FN_COUNT];
#ifdef HAVE_GLGD_PANGO
    PangoContext   *pangoFT2Context;
    glgdTexture    textTexture;
    PangoLayout    *layout;
#endif  /* HAVE_GLGD_PANGO */
} glgdGraph;

/*
 * Module API
 */
glgdGraph *glgdGraphCreate(void);
glgdGraph *glgdGraphDestroy(glgdGraph *graph);
GLboolean glgdGraphInit(glgdGraph *graph);
GLboolean glgdGraphFini(glgdGraph *graph);
GLboolean glgdGraphDraw(glgdGraph *graph);
GLboolean glgdGraphFrame(glgdGraph *graph);
GLboolean glgdGraphInvalidate(glgdGraph *graph);
GLboolean glgdGraphReshape(glgdGraph *graph);
GLboolean glgdGraphConnect(glgdGraph *graph, GtkWidget *glDrawArea);
GLboolean glgdGraphTranslate(glgdGraph *graph, GLdouble x, GLdouble y);
GLboolean glgdGraphCenter(glgdGraph *graph);
GLboolean glgdGraphAutoOrganize(glgdGraph *graph, glgdVec2 pos);
GLboolean glgdGraphAutoOrganizeXY(glgdGraph *graph, GLdouble x, GLdouble y);

glgdNode  *glgdGraphNodeByID(glgdGraph *graph, int nodeID);
glgdNode  *glgdGraphNodeSelected(glgdGraph *graph, int selectNdx);
int       glgdGraphNodeSelectCount(glgdGraph *graph);
int       glgdGraphNodeCount(glgdGraph *graph);
GLboolean glgdGraphNodeAdd(glgdGraph *graph, glgdNode *node);
GLboolean glgdGraphNodeListFlag(glgdGraph *graph, GLuint flagMask,
                                glgdFlagOp flagOp);
GLboolean glgdGraphLinkListAdd(glgdGraph *graph, glgdLinkList *list);
GLboolean glgdGraphLinkListDump(glgdGraph *graph);
GLboolean glgdGraphLinkAdd(glgdGraph *graph,
                           glgdLinkList *list, glgdLink *link);
int       glgdGraphLinkNdx(glgdGraph *graph, glgdLink *link);
glgdLink  *glgdGraphLinkByNdx(glgdGraph *graph, int linkNdx);
GLboolean glgdGraphCallbackSet(glgdGraph *graph,
                               glgdGraphFnEnum type, ScmObj fn);
GLboolean glgdGraphFlagsSet(glgdGraph *graph, GLuint flagMask, glgdFlagOp op);
GLboolean glgdGraphDimSet(glgdGraph *graph, glgdVec2 dim);
GLboolean glgdGraphDimSetByList(glgdGraph *graph, GLdouble w, GLdouble h);
GLboolean glgdGraphDimGet(glgdGraph *graph, glgdVec2 dim);
GLboolean glgdGraphMarginSet(glgdGraph *graph, GLdouble margin);
GLdouble  glgdGraphMarginGet(glgdGraph *graph);
GLboolean glgdGraphLineColorSet(glgdGraph *graph, glgdColor col);
GLboolean glgdGraphLineColorSetByList(glgdGraph *graph,
                                      GLdouble r, GLdouble g,
                                      GLdouble b, GLdouble a);
GLboolean glgdGraphLineColorGet(glgdGraph *graph, glgdColor col);

GLboolean glgdGraphAttributeClear(glgdGraph *graph);
GLboolean glgdGraphAttributeSet(glgdGraph *graph, int attrNdx);
GLboolean glgdGraphAttributeToggle(glgdGraph *graph, int attrNdx);
GLboolean glgdGraphAttributeReset(glgdGraph *graph, int attrNdx);
GLboolean glgdGraphAttributeIsSet(glgdGraph *graph, int attrNdx);

/* OBSOLETED - do not use */
GLboolean glgdGraphConnect3(glgdGraph *graph,
                            GtkWidget *toplevel, ScmObj glDrawArea);

SCM_DECL_END

#endif  /* __GLGDGRAPH_H__ */
