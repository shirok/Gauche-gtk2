/*
 * gldgGraph.c
 *
 * OpenGL Graph Display module implementation
 *
 * Written by: Shawn Taras
 */
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <GL/gl.h>
#include "gauche-glgd.h"
#include "glgd.h"

/*
 * Defines
 */
#define GLGDGRAPH_DRAW_EXTENTS          (0)
#define GLGDGRAPH_CAMORBIT              (0)
#define GLGDGRAPH_NODENAME              (1)
#define GLGDGRAPH_LINKNAME              (2)
#ifdef __MINGW32__
#define _MAXFLT                         (HUGE_VAL)
#else  /*!__MINGW32__*/
#define _MAXFLT                         (HUGE)
#endif /*!__MINGW32__*/
#define _PANGO_DPI                      (72)
#define _PANGO_SCALE                    (3)
#define _TEXW                           (1024)
#define _TEXH                           (1024)

/*
 * Static local (to this module) variables
 */
static glgdColor        s_lineColor = {0.3, 0.3, 0.3, 1.0};
static glgdColor        s_strokeColor = {0.1, 0.1, 0.1, 1.0};
static glgdVec2         s_strokePointSize = {6.0, 12.0};
static GLdouble         s_lineWidth = 1.0;
static GLint            s_blendFunc[2] = {GL_ONE, GL_ZERO};
static GLint            s_verbosity = 0;

/*
 * Static local (to this module) functions
 */
static int
glgdGraphChildInfo(glgdGraph *graph, glgdLink *link, int *childNdx)
{
    int             childCount;
    glgdLinkList    *list;
    glgdLink        *l;

    if (childNdx)
    {
        *childNdx = -1;
    }
    childCount = 0;
    list = graph->linkListHead;
    while (list)
    {
        l = list->linkHead;
        while (l)
        {
            if (childNdx && l == link)
            {
                *childNdx = childCount;
            }
            if (l->src == link->src)
            {
                childCount++;
            }

            l = l->next;
        }

        list = list->next;
    }

    glgdTrace(1, "glgdGraphChildInfo(graph, %s->%s, %d) = %d\n",
              link->src->label, link->dst->label, *childNdx, childCount);

    return childCount;
}

static GLboolean
glgdGraphAutoOrganizeLinkList(glgdGraph       *graph,
                              glgdLinkList    *list,
                              glgdVec2        pos,
                              glgdVec4        extents)
{
    int         childCount;
    int         childNdx;
    GLdouble    offset, width;
    glgdLink    *link;
    glgdVec2    vec;
    GLboolean   nextRow, nextCol;
    GLboolean   srcValid, dstValid;

    if (graph != NULL)
    {
        glgdGraphNodeListFlag(graph, GLGDNODE_FLAG_TOUCHED,
                              GLGD_FLAGOP_CLEAR);
        link = list->linkHead;
        while (link)
        {
            nextRow = GL_FALSE;

            srcValid = glgdBitfieldCompare(&graph->attributes,
                                           &link->src->attributes);
            dstValid = glgdBitfieldCompare(&graph->attributes,
                                           &link->dst->attributes);
            if (srcValid)
            {
                /* Source node */
                if (srcValid)
                {
                    if (glgdNodeIsTouched(link->src))
                    {
                        if (glgdNodeIsTouched(link->dst) && dstValid)
                        {
                            glgdTrace(1, "LOOP: %s @ (%g,%g) to %s @ (%g,%g)\n",
                                link->src->label,
                                link->src->pos[0], link->src->pos[1],
                                link->dst->label,
                                link->dst->pos[0], link->dst->pos[1]);

                            /* This link loops back to a parent node */
                            glgdLinkFlagsSet(link, GLGDLINK_FLAG_LOOPBACK,
                                             GLGD_FLAGOP_SET);
                        }
                        else
                        {
                            pos[0] = link->src->pos[0];
                        }
                    }
                    else
                    {
                        glgdTrace(1, "SRC: %s @ (%g,%g)\n", link->src->label,
                                  pos[0], pos[1]);

                        glgdNodePosSet(link->src, pos, graph->dim, extents);
                        glgdNodeFlagsSet(link->src, GLGDNODE_FLAG_TOUCHED,
                                         GLGD_FLAGOP_SET);

                        nextRow = GL_TRUE;
                    }
                }

                /* Destination node */
                vec[0] = pos[0] + GLGD_QUARTER(graph->dim[0]);
                vec[1] = pos[1] - 1.25 * graph->dim[1];
                if (dstValid)
                {
                    if (glgdNodeIsTouched(link->dst) == GL_FALSE)
                    {
                        glgdTrace(1, "DST: %s @ (%g,%g)\n",
                                  link->dst->label, vec[0], vec[1]);

                        glgdNodePosSet(link->dst, vec, graph->dim, extents);
                        glgdNodeFlagsSet(link->dst, GLGDNODE_FLAG_TOUCHED,
                                         GLGD_FLAGOP_SET);

                        nextRow = GL_TRUE;
                    }
                }
            }
            link = link->next;

            if (nextRow)
            {
                pos[1] = vec[1];
            }
        }
        glgdGraphNodeListFlag(graph, GLGDNODE_FLAG_TOUCHED, GLGD_FLAGOP_CLEAR);

        return GL_TRUE;
    }

    return GL_FALSE;
}

static void
glgdGraphPushAttributes(void)
{
    glPushAttrib(GL_ENABLE_BIT | GL_HINT_BIT | GL_LINE_BIT);
    glGetIntegerv(GL_BLEND_SRC, &s_blendFunc[0]);
    glGetIntegerv(GL_BLEND_DST, &s_blendFunc[1]);

    /* Common attributes for primitive drawing */
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    glLineWidth((GLfloat)s_lineWidth);
}

static void
glgdGraphPopAttributes(void)
{
    glPopAttrib();
    glBlendFunc(s_blendFunc[0], s_blendFunc[1]);
}

#ifdef HAVE_GLGD_PANGO
static GLboolean
glgdGraphNodeDrawLabel(glgdGraph *graph, glgdNode *node)
{
    int                     i;
    GLint                   width;
    GLuint                  texture;
    GLfloat                 s0, s1, t0, t1;
    GLfloat                 a;
    guint32                 alpha, rgb, *t;
    guint8                  *row, *row_end;
    PangoContext            *pangoContext;
    PangoFontDescription    *fontDesc;
    PangoLayout             *layout;
    PangoRectangle          extents;
    FT_Bitmap               bitmap;
    glgdVec2                center, pnt[2];
    glgdStroke              *stroke;
    glgdTexture             *tex;

    if (graph && graph->pangoFT2Context)
    {
        stroke = &graph->stroke;
        tex = &graph->textTexture;
        if (tex->width <= 0 || tex->height <= 0)
        {
            glgdTrace(1, "Invalid texture dimension (%d,%d)\n", tex->width,
                tex->height);

            return GL_FALSE;
        }

        /* Pango font description */
        width = 10 * _PANGO_SCALE;
        pangoContext = gtk_widget_get_pango_context(graph->gtkWindow);
        fontDesc = pango_context_get_font_description(pangoContext);
        pango_font_description_set_size(fontDesc, PANGO_SCALE * width);
        pango_font_description_set_weight(fontDesc, PANGO_WEIGHT_NORMAL);
        pango_context_set_font_description(graph->pangoFT2Context, fontDesc);

        /* Text layout */
        width = (int)graph->dim[0] * _PANGO_SCALE;
        layout = graph->layout;
        pango_layout_set_width(layout, PANGO_SCALE * width);
        pango_layout_set_alignment(layout, PANGO_ALIGN_CENTER);
        pango_layout_set_text(layout, node->label, -1);
        pango_layout_get_extents(layout, NULL, &extents);
        if (extents.width == 0 || extents.height == 0)
        {
            glgdTrace(1, "Invalid extents (%d,%d)\n", extents.width,
                extents.height);

            return GL_FALSE;
        }

        /* Bitmap creation */
        bitmap.rows = PANGO_PIXELS(extents.height);
        bitmap.width = PANGO_PIXELS(extents.width);
        if (bitmap.width > tex->width || bitmap.rows > tex->height)
        {
            return GL_FALSE;
        }

        bitmap.pitch = bitmap.width;
        bitmap.buffer = GLGD_MALLOC(bitmap.rows * bitmap.width);
        bitmap.num_grays = 256;
        bitmap.pixel_mode = ft_pixel_mode_grays;

        memset(bitmap.buffer, 0, bitmap.rows * bitmap.width);
        pango_ft2_render_layout(&bitmap, layout, PANGO_PIXELS(-extents.x), 0);

#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
        rgb =((guint32)(stroke->col[0] * 255.0))         |
            (((guint32)(stroke->col[1] * 255.0)) << 8)   |
            (((guint32)(stroke->col[2] * 255.0)) << 16);
#else
        rgb =(((guint32)(stroke->col[0] * 255.0)) << 24)    |
            (((guint32)(stroke->col[1] * 255.0)) << 16) |
            (((guint32)(stroke->col[2] * 255.0)) << 8);
#endif

        /* Bitmap transfer to <glgdTexture> */
        a = stroke->col[3];
        alpha = (guint32)(255.0 * a);
        row = bitmap.buffer + bitmap.rows * bitmap.width;
        row_end = bitmap.buffer;
        t = (guint32 *)tex->texels;
        if (graph->flags & GLGDGRAPH_FLAG_PANGOBOLD)
        {
            do
            {
                row -= bitmap.width;
                for (i=0; i<bitmap.width; i++)
                {
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
                    if (row[i] > 0)
                        *t++ = rgb | (alpha << 24);
                    else
                        *t++ = rgb;
#else
                    if (row[i] > 0)
                        *t++ = rgb | alpha;
                    else
                        *t++ = rgb;
#endif
                }
            }
            while (row != row_end);
        }
        else
        {
            do
            {
                row -= bitmap.width;
                for (i=0; i<bitmap.width; i++)
                {
#if !defined(GL_VERSION_1_2) && G_BYTE_ORDER == G_LITTLE_ENDIAN
                    *t++ = rgb | ((guint32)(a * row[i]) << 24);
#else
                    *t++ = rgb | (guint32)(a * row[i]);
#endif
                }
            }
            while (row != row_end);
        }

        glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
        glBindTexture(GL_TEXTURE_2D, tex->name);
#if !defined(GL_VERSION_1_2)
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, bitmap.width, bitmap.rows,
            GL_RGBA, GL_UNSIGNED_BYTE, tex->texels);
#else
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, bitmap.width, bitmap.rows,
            GL_RGBA, GL_UNSIGNED_INT_8_8_8_8, tex->texels);
#endif

        /* <glgdTexture> render */
        s0 = 0.0;
        s1 = (GLdouble)bitmap.width / (GLdouble)tex->width;
        t0 = 0.0;
        t1 = (GLdouble)bitmap.rows / (GLdouble)tex->height;

        center[0] = node->pos[0] + GLGD_HALF(graph->dim[0]);
        center[1] = node->pos[1] + GLGD_HALF(graph->dim[1]);
        pnt[0][0] = center[0] - GLGD_HALF(bitmap.width / _PANGO_SCALE);
        pnt[0][1] = center[1] - GLGD_HALF(bitmap.rows / _PANGO_SCALE);
        pnt[1][0] = center[0] + GLGD_HALF(bitmap.width / _PANGO_SCALE);
        pnt[1][1] = center[1] + GLGD_HALF(bitmap.rows / _PANGO_SCALE);
        GLGD_FREE(bitmap.buffer);

        glColor3d(stroke->col[0], stroke->col[1], stroke->col[2]);
        glEnable(GL_TEXTURE_2D);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glBindTexture(GL_TEXTURE_2D, tex->name);
        glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
        glBegin(GL_QUADS);
            glTexCoord2f(s0, t0);
            glVertex3f(pnt[0][0], pnt[0][1], 0.0);

            glTexCoord2f(s0, t1);
            glVertex3f(pnt[0][0], pnt[1][1], 0.0);

            glTexCoord2f(s1, t1);
            glVertex3f(pnt[1][0], pnt[1][1], 0.0);

            glTexCoord2f(s1, t0);
            glVertex3f(pnt[1][0], pnt[0][1], 0.0);
        glEnd();
        glDisable(GL_BLEND);
        glDisable(GL_TEXTURE_2D);

        return GL_TRUE;
    }

    return GL_FALSE;
}
#endif  /* HAVE_GLGD_PANGO */

static GLboolean
glgdGraphNodeRender
(
    glgdGraph   *graph,
    glgdNode    *node,
    ScmObj      fn,
    GLenum      renderMode
)
{
    if (renderMode == GL_SELECT)
    {
        glPushName(GLGDGRAPH_NODENAME);
    }
    glgdNodeDraw(node, graph->dim, fn, renderMode);
    if (renderMode == GL_SELECT)
    {
        glPopName();
    }

#ifdef HAVE_GLGD_PANGO
    glgdGraphNodeDrawLabel(graph, node);
#endif  /* HAVE_GLGD_PANGO */

    return GL_TRUE;
}

static GLboolean
glgdGraphRender(glgdGraph *graph, GLenum renderMode)
{
    int             linkNdx;
    int             nodeDrawCount;
    glgdLinkList    *list;
    glgdLink        *link;
    glgdNode        *src;
    glgdNode        *dst;
    ScmObj          fn;

    if (graph != NULL)
    {
        fn = NULL;
        if (renderMode == GL_RENDER)
        {
            fn = graph->fn[GLGDGRAPH_FN_PRERENDER];
        }

        glgdGraphNodeListFlag(graph, GLGDNODE_FLAG_TOUCHED, GLGD_FLAGOP_CLEAR);

        linkNdx = 0;
        list = graph->linkListHead;
        while (list)
        {
            link = list->linkHead;
            while (link)
            {
                src = link->src;
                dst = link->dst;
                nodeDrawCount = 0;
                if (glgdBitfieldCompare(&graph->attributes, &src->attributes))
                {
                    /* Draw the src node */
                    if (glgdNodeIsTouched(src) == GL_FALSE)
                    {
                        glgdGraphNodeRender(graph, src, fn, renderMode);
                        glgdNodeFlagsSet(src, GLGDNODE_FLAG_TOUCHED,
                            GLGD_FLAGOP_SET);
                    }
                    nodeDrawCount++;

                    if (!(link->flags & GLGDLINK_FLAG_LONER) &&
                        glgdBitfieldCompare(&graph->attributes, &dst->attributes))
                    {
                        if (glgdNodeIsTouched(dst) == GL_FALSE)
                        {
                            glgdGraphNodeRender(graph, dst, fn, renderMode);
                            glgdNodeFlagsSet(dst, GLGDNODE_FLAG_TOUCHED,
                                GLGD_FLAGOP_SET);
                        }
                        nodeDrawCount++;
                    }
                }

                if (nodeDrawCount == 2)
                {
                    /* Draw the connecting link */
                    if (renderMode == GL_SELECT)
                    {
                        glPushName(GLGDGRAPH_LINKNAME);
                        glPushName(linkNdx);
                    }
                    glColor4d(graph->lineColor[0], graph->lineColor[1],
                        graph->lineColor[2], graph->lineColor[3]);
                    glgdLinkDraw(link, graph->dim, renderMode);
                    if (renderMode == GL_SELECT)
                    {
                        glPopName();
                        glPopName();
                    }
                }

                link = link->next;
                linkNdx++;
            }

            list = list->next;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

static void
glgdGraphComputeHoverData(glgdGraph *graph, GLdouble mx, GLdouble my)
{
    int         i, j;
    GLint       nameCount;
    GLint       hitCount;
    GLuint      selectBuf[64];
    GLuint      *ptr;

    if (graph->nodeHead)
    {
        glSelectBuffer(64, selectBuf);
        glRenderMode(GL_SELECT);

        glInitNames();

        glgdGraphPushAttributes();
        glgdCamBeginPick(&graph->ctrlCam, mx, my);
        glgdGraphRender(graph, GL_SELECT);
        glgdCamEnd(&graph->ctrlCam);
        glgdGraphPopAttributes();

        glFlush();

        hitCount = glRenderMode(GL_RENDER);
        if (hitCount > 0)
        {
            ptr = &selectBuf[0];
            for (i=0; i<hitCount; i++)
            {
                nameCount = *ptr;
                ptr++;
                glgdTrace(3, "%3d: nameCount: %d\n", i, nameCount);
                glgdTrace(3, "     zMin: %g\n", (GLdouble)*ptr/0x7fffffff);
                ptr++;
                glgdTrace(3, "     zMax: %g\n", (GLdouble)*ptr/0x7fffffff);
                ptr++;

                if (*ptr == GLGDGRAPH_NODENAME)
                {
                    graph->hoverNode = glgdNodeByID(graph->nodeHead, ptr[1]);
                    graph->hoverLink = NULL;
                }
                else if (*ptr == GLGDGRAPH_LINKNAME)
                {
                    graph->hoverLink = glgdGraphLinkByNdx(graph, ptr[1]);
                    if (nameCount > 2)
                    {
                        graph->hoverNode = glgdNodeByID(graph->nodeHead, ptr[2]);
                    }
                }

                if (s_verbosity >= 3)
                {
                    for (j=0; j<nameCount; j++)
                    {
                        glgdTrace(3, "  name[%1d]: %d\n", j, ptr[j]);
                    }
                }

                ptr += nameCount;
            }
        }
        else
        {
            graph->hoverNode = NULL;
            graph->hoverLink = NULL;
        }
    }
}

static gboolean
glgdGraphMouseButtonCB(GtkWidget *widget, GdkEventButton *event, gpointer *data)
{
    ScmObj      fn;
    glgdGraph   *graph;

    graph = (glgdGraph *)data;
    if (graph == NULL)
    {
        return FALSE;
    }

    switch (event->type)
    {
        case GDK_BUTTON_PRESS:
            glgdCamMouseSet(&graph->ctrlCam, event->x, event->y);
        break;

        case GDK_BUTTON_RELEASE:
            glgdCamMouseSet(&graph->ctrlCam, -1.0, -1.0);
        break;

        default:
            return FALSE;
    }

    gdk_window_invalidate_rect(widget->window, &widget->allocation, FALSE);

    fn = graph->fn[GLGDGRAPH_FN_MOUSE_LEFT];
    if (fn && event->button == 1)
    {
        Scm_ApplyRec4(fn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventButton(event)));
    }
    fn = graph->fn[GLGDGRAPH_FN_MOUSE_MIDDLE];
    if (fn && event->button == 2)
    {
        Scm_ApplyRec4(fn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventButton(event)));
    }
    fn = graph->fn[GLGDGRAPH_FN_MOUSE_RIGHT];
    if (fn && event->button == 3)
    {
        Scm_ApplyRec4(fn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventButton(event)));
    }

    return TRUE;
}

static gboolean
glgdGraphMouseMotionCB(GtkWidget *widget, GdkEventMotion *event, gpointer *data)
{
    int                 ix, iy;
    GLdouble            x, y;
    GdkModifierType     state;
    ScmObj              fn;
    glgdGraph           *graph;

    graph = (glgdGraph *)data;
    if (graph == NULL)
    {
        return FALSE;
    }

    /* Process the GDK_POINTER_MOTION_HINT_MASK events */
    if (event->is_hint)
    {
        gdk_window_get_pointer(event->window, &ix, &iy, &state);
        x = (GLdouble)ix;
        y = (GLdouble)iy;
    }
    else
    {
        x = event->x;
        y = event->y;
        state = event->state;
    }

    if (state & GDK_BUTTON1_MASK)
    {
        if (state & GDK_BUTTON2_MASK)
        {
            if (graph->flags & GLGDGRAPH_FLAG_CTRLHELD)
            {
                glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_ZOOM,
                    x, y, graph->frameTime);
            }
        }
#if GLGDGRAPH_CAMORBIT
        else
        {
            if (graph->flags & GLGDGRAPH_FLAG_CTRLHELD)
            {
                glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_ORBIT,
                    x, y, graph->frameTime);
            }
        }
#endif  /* GLGDGRAPH_CAMORBIT */
    }
    else if (state & GDK_BUTTON2_MASK)
    {
        if (graph->flags & GLGDGRAPH_FLAG_CTRLHELD)
        {
            glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_PAN,
                x, y, graph->frameTime);
        }
#if !GLGDGRAPH_CAMORBIT
        else
        {
            glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_PAN,
                x, y, graph->frameTime);
        }
#endif  /* !GLGDGRAPH_CAMORBIT */
    }
    else if (state & GDK_BUTTON3_MASK)
    {
        if (graph->flags & GLGDGRAPH_FLAG_CTRLHELD)
        {
            glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_ZOOM,
                x, y, graph->frameTime);
        }
    }

    fn = graph->fn[GLGDGRAPH_FN_MOUSE_HOVER];
    if (fn)
    {
        Scm_ApplyRec4(fn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventMotion(event)));
    }

    gdk_window_invalidate_rect(widget->window, &widget->allocation, FALSE);

    return TRUE;
}

static gboolean
glgdGraphMouseScrollCB(GtkWidget *widget, GdkEventScroll *event, gpointer *data)
{
    glgdGraph   *graph;
    ScmObj      fn;

    graph = (glgdGraph *)data;
    if (graph == NULL)
    {
        return FALSE;
    }

    switch (event->direction)
    {
        case GDK_SCROLL_UP:
            glgdCamMouseSet(&graph->ctrlCam, event->x, event->y);
            glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_ZOOM,
                event->x + 50.0, event->y, graph->frameTime);
        break;

        case GDK_SCROLL_DOWN:
            glgdCamMouseSet(&graph->ctrlCam, event->x, event->y);
            glgdCamUpdate(&graph->ctrlCam, GLGDCAM_MODE_ZOOM,
                event->x - 50.0, event->y, graph->frameTime);
        break;

        default:
            return FALSE;
    }

    gdk_window_invalidate_rect(widget->window, &widget->allocation, FALSE);

    fn = graph->fn[GLGDGRAPH_FN_MOUSE_SCROLL];
    if (fn)
    {
        Scm_ApplyRec4(fn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventScroll(event)));
    }

    return TRUE;
}

static gboolean
glgdGraphKeyCB(GtkWidget *widget, GdkEventKey *event, gpointer data)
{
    glgdGraph   *graph;
    ScmObj      keyFn;

    graph = (glgdGraph *)data;
    if (graph == NULL)
    {
        return FALSE;
    }

    switch (event->keyval)
    {
        case GDK_Control_L:
        case GDK_Control_R:
            if (event->type == GDK_KEY_PRESS)
            {
                graph->flags |= GLGDGRAPH_FLAG_CTRLHELD;
            }
            else if (event->type == GDK_KEY_RELEASE)
            {
                graph->flags &= ~GLGDGRAPH_FLAG_CTRLHELD;
            }
        break;

        case GDK_Escape:
            if (event->type == GDK_KEY_PRESS)
            {
                graph->flags |= GLGDGRAPH_FLAG_ESCPRESSED;
            }
        break;

        default:
            return FALSE;
    }

    gdk_window_invalidate_rect(widget->window, &widget->allocation, FALSE);

    keyFn = graph->fn[GLGDGRAPH_FN_KEY];
    if (keyFn != NULL)
    {
        Scm_ApplyRec4(keyFn,
                      SCM_OBJ(SCM_MAKE_GLGD_GRAPH(graph)),
                      SCM_OBJ(SCM_MAKE_GLGD_NODE(graph->hoverNode)),
                      SCM_OBJ(SCM_MAKE_GLGD_LINK(graph->hoverLink)),
                      SCM_OBJ(Scm_MakeGdkEventKey(event)));
    }

    return TRUE;
}

/* NB: this is here only because we need glgdGraphConnect3 to keep
   backward compatibility; once glgdGraphConnect3 is gone, we can merge
   this into glgdGraphConnect. */
static GLboolean
glgdGraphConnectInt(glgdGraph *graph, GtkWidget *gtkWindow,
                    GtkWidget *glDrawArea)
{
    if (graph && gtkWindow && glDrawArea)
    {
        /* Add mouse and keyboard events to the GL draw area */
        gtk_widget_add_events(glDrawArea,
                              GDK_POINTER_MOTION_MASK |
                              GDK_POINTER_MOTION_HINT_MASK |
                              GDK_BUTTON_PRESS_MASK |
                              GDK_BUTTON_RELEASE_MASK |
                              GDK_SCROLL_MASK |
                              GDK_VISIBILITY_NOTIFY_MASK);

        /* Connect signals to callback routines */
        g_signal_connect(G_OBJECT(glDrawArea), "button_press_event",
                         G_CALLBACK(glgdGraphMouseButtonCB), graph);
        g_signal_connect(G_OBJECT(glDrawArea), "button_release_event",
                         G_CALLBACK(glgdGraphMouseButtonCB), graph);
        g_signal_connect(G_OBJECT(glDrawArea), "motion_notify_event",
                         G_CALLBACK(glgdGraphMouseMotionCB), graph);
        g_signal_connect(G_OBJECT(glDrawArea), "scroll_event",
                         G_CALLBACK(glgdGraphMouseScrollCB), graph);

        /* GTK_CAN_FOCUS allows the <glDrawArea> to receive key events */
        GTK_WIDGET_SET_FLAGS(glDrawArea, GTK_CAN_FOCUS);
        g_signal_connect(G_OBJECT(glDrawArea), "key_press_event",
                         G_CALLBACK(glgdGraphKeyCB), graph);
        g_signal_connect(G_OBJECT(glDrawArea), "key_release_event",
                         G_CALLBACK(glgdGraphKeyCB), graph);

        graph->gtkWindow = gtkWindow;
        graph->gtkGLDrawArea = glDrawArea;

#ifdef HAVE_GLGD_PANGO
        graph->pangoFT2Context = pango_ft2_get_context(_PANGO_DPI, _PANGO_DPI);
        if (graph->pangoFT2Context == NULL)
        {
            printf("pango_ft2_get_context(%d,%d) failed\n", _PANGO_DPI,
                   _PANGO_DPI);
            glgdGraphFini(graph);
            return GL_FALSE;
        }
        graph->layout = pango_layout_new(graph->pangoFT2Context);
#endif  /* HAVE_GLGD_PANGO */

        return GL_TRUE;
    }

    return GL_FALSE;
}

/*
 * External public functions
 */
glgdGraph
*glgdGraphCreate(void)
{
    glgdGraph   *graph;

    graph = (glgdGraph *)GLGD_MALLOC(sizeof(glgdGraph));
    if (graph)
    {
        glgdGraphInit(graph);
    }

    return graph;
}

glgdGraph
*glgdGraphDestroy(glgdGraph *graph)
{
    glgdGraphFini(graph);
    GLGD_FREE(graph);

    return (glgdGraph *)NULL;
}

GLboolean
glgdGraphInit(glgdGraph *graph)
{
    int         i;

    if (graph != NULL)
    {
        graph->flags = GLGDGRAPH_FLAG_INITIALIZED;
        graph->nodeCount = 0;
        graph->linkCount = 0;
        graph->frameTime = 1.0 / 30.0;
        graph->margin = GLGDGRAPH_NODEMARGIN_DEFAULT;
        graph->dim[0] = GLGDGRAPH_NODEWIDTH_DEFAULT;
        graph->dim[1] = GLGDGRAPH_NODEHEIGHT_DEFAULT;
        graph->extents[0] = +_MAXFLT;
        graph->extents[1] = +_MAXFLT;
        graph->extents[2] = -_MAXFLT;
        graph->extents[3] = -_MAXFLT;
        glgdGraphLineColorSet(graph, s_lineColor);
        glgdCamInit(&graph->ctrlCam);
        glgdStrokeInit(&graph->stroke);
        graph->stroke.flags |= GLGDSTROKE_FLAG_INVERT;
        glgdStrokeColorSet(&graph->stroke, s_strokeColor);
        glgdStrokePointSizeSet(&graph->stroke, s_strokePointSize);
        glgdBitfieldInit(&graph->attributes);
        graph->nodeHead = NULL;
        graph->linkListHead = NULL;
        graph->hoverNode = NULL;
        graph->hoverLink = NULL;
        graph->timer = g_timer_new();
        graph->gtkWindow = NULL;
        graph->gtkGLDrawArea = NULL;

        for (i=0; i<GLGDGRAPH_FN_COUNT; i++)
        {
            graph->fn[i] = NULL;
        }
#ifdef HAVE_GLGD_PANGO
        graph->pangoFT2Context = NULL;
        glgdTextureInit(&graph->textTexture);
#endif  /* HAVE_GLGD_PANGO */

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphFini(glgdGraph *graph)
{
    if (graph != NULL)
    {
        if (graph->nodeHead)
        {
            glgdNodeDestroy(graph->nodeHead);
        }
        glgdStrokeFini(&graph->stroke);
        glgdBitfieldFini(&graph->attributes);
        g_timer_destroy(graph->timer);

#ifdef HAVE_GLGD_PANGO
        glgdTextureFini(&graph->textTexture);
        if (graph->pangoFT2Context != NULL)
        {
            g_object_unref(G_OBJECT(graph->pangoFT2Context));
            pango_ft2_shutdown_display();
            graph->pangoFT2Context = NULL;
        }
        if (graph->layout != NULL) {
            g_object_unref(G_OBJECT(graph->layout));
            graph->layout = NULL;
        }
#endif  /* HAVE_GLGD_PANGO */

        graph->flags = GLGDGRAPH_FLAG_INITIALIZED;
        graph->nodeCount = 0;
        graph->linkCount = 0;
        graph->frameTime = 1.0 / 30.0;
        graph->margin = GLGDGRAPH_NODEMARGIN_DEFAULT;
        graph->dim[0] = GLGDGRAPH_NODEWIDTH_DEFAULT;
        graph->dim[1] = GLGDGRAPH_NODEHEIGHT_DEFAULT;
        graph->extents[0] = +_MAXFLT;
        graph->extents[1] = +_MAXFLT;
        graph->extents[2] = -_MAXFLT;
        graph->extents[3] = -_MAXFLT;
        glgdGraphLineColorSet(graph, s_lineColor);
        glgdCamInit(&graph->ctrlCam);
        glgdStrokeInit(&graph->stroke);
        graph->stroke.flags |= GLGDSTROKE_FLAG_INVERT;
        glgdStrokeColorSet(&graph->stroke, s_strokeColor);
        glgdStrokePointSizeSet(&graph->stroke, s_strokePointSize);
        glgdBitfieldInit(&graph->attributes);
        graph->nodeHead = NULL;
        graph->linkListHead = NULL;
        graph->hoverNode = NULL;
        graph->hoverLink = NULL;
        graph->timer = g_timer_new();
        graph->gtkWindow = NULL;
        graph->gtkGLDrawArea = NULL;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphDraw(glgdGraph *graph)
{
    int             ix, iy;
    GLdouble        aspect;
    GLdouble        w, h;
    GLdouble        x, y;
    GdkModifierType state;
    glgdStroke      *last;
    glgdLink        *link;

    if (graph != NULL)
    {
        if (graph->gtkGLDrawArea &&
            graph->ctrlCam.winDim[0] == 0.0 &&
            graph->ctrlCam.winDim[1] == 0.0)
        {
            w = (GLdouble)graph->gtkGLDrawArea->allocation.width;
            h = (GLdouble)graph->gtkGLDrawArea->allocation.height;

            glViewport(0, 0,
                graph->gtkGLDrawArea->allocation.width,
                graph->gtkGLDrawArea->allocation.height);

            /* Submit the window dimension to the stroke font */
            glgdStrokeWindowDimSetByList(&graph->stroke, w, h);

            /* Set up the camera frustum wrt window dimensions */
            aspect = h / w;
            glgdCamFrustum(&graph->ctrlCam,
                           -1.0, 1.0, -aspect, aspect, 4.0, 8000.0);
            glgdCamWinDimSet(&graph->ctrlCam, w, h);
            glgdCamFrame(&graph->ctrlCam,
                graph->extents[0], graph->extents[2] + graph->margin,
                graph->extents[1], graph->extents[3] + graph->margin);
        }
#ifdef HAVE_GLGD_PANGO
        if (graph->textTexture.texels == NULL)
        {
            if (glgdTextureSetup(&graph->textTexture, _TEXW, _TEXH) == GL_FALSE)
            {
                printf("glgdTextureSetup(%d,%d) failed\n", _TEXW, _TEXH);
                return GL_FALSE;
            }
        }
#endif  /* HAVE_GLGD_PANGO */

        if (graph->nodeHead)
        {
            last = glgdStrokeGetCurrent();
            glgdStrokeSetCurrent(&graph->stroke);
            glgdGraphPushAttributes();
            glgdCamBegin(&graph->ctrlCam);
            glgdGraphRender(graph, GL_RENDER);
            glgdStrokeSetCurrent(last);
#if GLGDGRAPH_DRAW_EXTENTS
            /* Draw Extents */
            {
                glgdVec2    xy;
                glgdVec2    wh;

                xy[0] = graph->extents[0];
                xy[1] = graph->extents[1];
                wh[0] = graph->extents[2] - xy[0];
                wh[1] = graph->extents[3] - xy[1];

                glgdDrawBoundary(xy, wh, g_colorYellow);

                xy[0] = -graph->ctrlCam.camPos[0] - 2.0;
                xy[1] = -graph->ctrlCam.camPos[1] - 2.0;
                wh[0] = 4.0;
                wh[1] = 4.0;
                glgdDrawRect(xy, wh, g_colorRed);
            }
#endif  /* GLGDGRAPH_DRAW_EXTENTS */
            glgdCamEnd(&graph->ctrlCam);
            glgdGraphPopAttributes();

            if (graph->gtkWindow)
            {
                gdk_window_get_pointer(graph->gtkWindow->window,
                    &ix, &iy, &state);
                x = (GLdouble)ix;
                y = (GLdouble)iy;
                glgdGraphComputeHoverData(graph, x, y);
            }
        }

        g_timer_stop(graph->timer);
        graph->frameTime = g_timer_elapsed(graph->timer, NULL);
        g_timer_start(graph->timer);

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphFrame(glgdGraph *graph)
{
    if (graph != NULL)
    {
        return glgdCamFrame(&graph->ctrlCam,
            graph->extents[0], graph->extents[2] + graph->margin,
            graph->extents[1], graph->extents[3] + graph->margin);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphInvalidate(glgdGraph *graph)
{
    if (graph != NULL)
    {
        if (graph->gtkGLDrawArea)
        {
            gdk_window_invalidate_rect(graph->gtkGLDrawArea->window,
                &graph->gtkGLDrawArea->allocation, FALSE);

            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdGraphReshape(glgdGraph *graph)
{
    if (graph != NULL)
    {
        /* <glgdGraphDraw> will recompute the viewport */
        graph->ctrlCam.winDim[0] = 0.0;
        graph->ctrlCam.winDim[1] = 0.0;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphConnect(glgdGraph *graph, GtkWidget *glDrawArea)
{
    return glgdGraphConnectInt(graph,
                               gtk_widget_get_toplevel(glDrawArea),
                               glDrawArea);
}

GLboolean
glgdGraphConnect3(glgdGraph *graph, GtkWidget *gtkWindow, ScmObj glDrawArea)
{
    if (!SCM_GTK_WIDGET_P(glDrawArea)) {
        Scm_Error("<gtk-widget> required, but got %S", glDrawArea);
    }
    return glgdGraphConnectInt(graph, gtkWindow, SCM_GTK_WIDGET(glDrawArea));
}

GLboolean
glgdGraphTranslate(glgdGraph *graph, GLdouble x, GLdouble y)
{
    glgdVec2    xlat;

    if (graph != NULL)
    {
        graph->extents[0] = +_MAXFLT;
        graph->extents[1] = +_MAXFLT;
        graph->extents[2] = -_MAXFLT;
        graph->extents[3] = -_MAXFLT;

        xlat[0] = x;
        xlat[1] = y;

        return glgdNodeTranslate(graph->nodeHead, xlat, graph->dim,
                    graph->extents);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphCenter(glgdGraph *graph)
{
    glgdVec2        xlat;

    if (graph != NULL)
    {
        xlat[0] = -GLGD_HALF(graph->extents[0] + graph->extents[2]);
        xlat[1] = -GLGD_HALF(graph->extents[1] + graph->extents[3]);

        return glgdGraphTranslate(graph, xlat[0], xlat[1]);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAutoOrganize(glgdGraph *graph, glgdVec2 pos)
{
    GLboolean       rc;
    glgdVec2        org;
    glgdLinkList    *list;

    if (graph && graph->nodeHead)
    {
        graph->extents[0] = +_MAXFLT;
        graph->extents[1] = +_MAXFLT;
        graph->extents[2] = -_MAXFLT;
        graph->extents[3] = -_MAXFLT;

        org[0] = pos[0];
        org[1] = pos[1];
        list = graph->linkListHead;
        while (list)
        {
            glgdGraphAutoOrganizeLinkList(graph, list, pos, graph->extents);

            /* Next graph is to the right of the last */
            pos[0] = graph->extents[2] + graph->margin;
            pos[1] = org[1];

            list = list->next;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAutoOrganizeXY
(
    glgdGraph   *graph,
    GLdouble    x,
    GLdouble    y
)
{
    glgdVec2    pos;

    pos[0] = x;
    pos[1] = y;

    return glgdGraphAutoOrganize(graph, pos);
}

glgdNode
*glgdGraphNodeByID(glgdGraph *graph, int nodeID)
{
    return glgdNodeByID(graph->nodeHead, nodeID);
}

glgdNode
*glgdGraphNodeSelected(glgdGraph *graph, int selectNdx)
{
    int         ndx;
    glgdNode    *node;

    node = NULL;
    if (graph && selectNdx >= 0 && selectNdx < glgdGraphNodeSelectCount(graph))
    {
        ndx = 0;
        node = graph->nodeHead;
        while (node)
        {
            if (glgdNodeIsSelected(node))
            {
                if (ndx == selectNdx)
                {
                    return node;
                }
                ndx++;
            }

            node = node->next;
        }
    }

    return node;
}

int
glgdGraphNodeSelectCount(glgdGraph *graph)
{
    int         nodeSelectCount;
    glgdNode    *node;

    nodeSelectCount = 0;
    if (graph != NULL)
    {
        node = graph->nodeHead;
        while (node)
        {
            if (glgdNodeIsSelected(node))
            {
                nodeSelectCount++;
            }
            node = node->next;
        }
    }

    return nodeSelectCount;
}

int
glgdGraphNodeCount(glgdGraph *graph)
{
    int         nodeCount;
    glgdNode    *node;

    nodeCount = 0;
    if (graph != NULL)
    {
        node = graph->nodeHead;
        while (node)
        {
            nodeCount++;
            node = node->next;
        }
    }

    return nodeCount;
}

GLboolean
glgdGraphNodeAdd(glgdGraph *graph, glgdNode *node)
{
    glgdNode        *n;
    GLboolean       done;

    /* Add <node> to the <nodeHead> list sorted by <id> */
    if (graph && node)
    {
        if (graph->nodeHead == NULL)
        {
            graph->nodeHead = node;
        }
        else
        {
            n = graph->nodeHead;
            done = GL_FALSE;
            while (!done)
            {
                if (node->id <= n->id)
                {
                    /* Pre-insert <node> */
                    node->next = n;
                    node->prev = n->prev;
                    if (n->prev != NULL)
                    {
                        n->prev->next = node;
                    }
                    else
                    {
                        graph->nodeHead = node;
                    }
                    n->prev = node;

                    done = GL_TRUE;
                }
                else if (n->next == NULL)
                {
                    /* Add <node> to end of list */
                    n->next = node;
                    node->prev = n;

                    done = GL_TRUE;
                }

                n = n->next;
            }
        }

        graph->nodeCount++;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphLinkListAdd(glgdGraph *graph, glgdLinkList *list)
{
    glgdLinkList    *l;

    if (graph && list)
    {
        if (graph->linkListHead == NULL)
        {
            graph->linkListHead = list;
        }
        else
        {
            l = graph->linkListHead;
            while (l->next != NULL)
            {
                l = l->next;
            }

            l->next = list;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphLinkListDump(glgdGraph *graph)
{
    if (graph != NULL)
    {
        glgdLinkListDump(graph->linkListHead);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphNodeListFlag(glgdGraph *graph, GLuint flagMask, glgdFlagOp flagOp)
{
    glgdNode    *node;

    if (graph != NULL)
    {
        node = graph->nodeHead;
        while (node)
        {
            glgdNodeFlagsSet(node, flagMask, flagOp);

            node = node->next;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphLinkAdd(glgdGraph *graph, glgdLinkList *list, glgdLink *link)
{
    glgdLink        *l;
    GLboolean       done;
    GLboolean       inserted;

    /* Add <link> to the <linkHead> list sorted by src->dst relationship */
    if (graph && list && link)
    {
        if (link->src == link->dst)
        {
            if (list->linkHead == NULL)
            {
                list->linkHead = link;
                glgdLinkFlagsSet(link, GLGDLINK_FLAG_LONER,
                    GLGD_FLAGOP_SET);
                glgdTrace(1, "list->linkHead = [%s->%s] ***LONER***\n",
                    link->src->label, link->dst->label);

                graph->linkCount++;

                return GL_TRUE;
            }
            else
            {
                printf("Error! Attempt to add LONER to non-empty list\n");
                return GL_FALSE;
            }
        }

        if (list->linkHead && (list->linkHead->flags & GLGDLINK_FLAG_LONER))
        {
            printf("Error! Attempt to add link to a LONER list\n");
            return GL_FALSE;
        }

        if (list->linkHead == NULL)
        {
            list->linkHead = link;
            glgdTrace(1, "list->linkHead = [%s->%s]\n",
                link->src->label, link->dst->label);
        }
        else
        {
            /* First Pass: Post-insert <link> */
            l = list->linkHead;
            done = GL_FALSE;
            inserted = GL_FALSE;
            while (!done)
            {
                if (l->dst == link->src)
                {
                    /* Post-insert <link> */
                    link->next = l->next;
                    if (l->next)
                    {
                        l->next->prev = link;
                    }
                    link->prev = l;
                    l->next = link;

                    glgdTrace(1, "[%s->%s] AFTER [%s->%s]\n",
                        link->src->label, link->dst->label,
                        l->src->label, l->dst->label);
                    inserted = GL_TRUE;
                    done = GL_TRUE;
                }
                else if (l->next == NULL)
                {
                    done = GL_TRUE;
                }

                l = l->next;
            }

            /* Second Pass: Pre-insert and append <link> */
            if (inserted == GL_FALSE)
            {
                l = list->linkHead;
                done = GL_FALSE;
                while (!done)
                {
                    if (l->src == link->src || l->src == link->dst)
                    {
                        /* Pre-insert <link> */
                        link->next = l;
                        link->prev = l->prev;
                        if (l->prev != NULL)
                        {
                            l->prev->next = link;
                        }
                        else
                        {
                            list->linkHead = link;
                        }
                        l->prev = link;

                        glgdTrace(1, "[%s->%s] BEFORE [%s->%s]\n",
                            link->src->label, link->dst->label,
                            l->src->label, l->dst->label);
                        done = GL_TRUE;
                    }
                    else if (l->next == NULL)
                    {
                        /* Add <link> to end of list */
                        l->next = link;
                        link->prev = l;

                        glgdTrace(1, "[%s->%s] AFTER [%s->%s] AT END\n",
                            link->src->label, link->dst->label,
                            l->src->label, l->dst->label);
                        done = GL_TRUE;
                    }

                    l = l->next;
                }
            }
        }

        graph->linkCount++;

        return GL_TRUE;
    }

    return GL_FALSE;
}

int
glgdGraphLinkNdx(glgdGraph *graph, glgdLink *link)
{
    int             linkNdx;
    glgdLinkList    *list;
    glgdLink        *l;

    if (graph && link)
    {
        linkNdx = 0;
        list = graph->linkListHead;
        while (list)
        {
            l = list->linkHead;
            while (l)
            {
                if (l == link)
                {
                    return linkNdx;
                }

                l = l->next;
                linkNdx++;
            }

            list = list->next;
        }
    }

    return -1;
}

glgdLink
*glgdGraphLinkByNdx(glgdGraph *graph, int linkNdx)
{
    int             curNdx;
    glgdLinkList    *list;
    glgdLink        *l;

    if (graph && linkNdx >= 0)
    {
        curNdx = 0;
        list = graph->linkListHead;
        while (list)
        {
            l = list->linkHead;
            while (l)
            {
                if (curNdx == linkNdx)
                {
                    return l;
                }

                l = l->next;
                curNdx++;
            }

            list = list->next;
        }
    }

    return NULL;
}

GLboolean
glgdGraphCallbackSet(glgdGraph *graph, glgdGraphFnEnum type, ScmObj fn)
{
    if (graph && type >= 0 && type < GLGDGRAPH_FN_COUNT)
    {
        graph->fn[type] = fn;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphFlagsSet(glgdGraph *graph, GLuint flagMask, glgdFlagOp op)
{
    if (graph && op < GLGD_FLAGOP_COUNT)
    {
        if (op == GLGD_FLAGOP_CLEAR)
        {
            graph->flags &= ~flagMask;
        }
        else if (op == GLGD_FLAGOP_SET)
        {
            graph->flags |= flagMask;
        }
        else if (op == GLGD_FLAGOP_TOGGLE)
        {
            graph->flags ^= flagMask;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphDimSet(glgdGraph *graph, glgdVec2 dim)
{
    if (graph != NULL)
    {
        graph->dim[0] = dim[0];
        graph->dim[1] = dim[1];
    }

    return GL_FALSE;
}

GLboolean
glgdGraphDimSetByList(glgdGraph *graph, GLdouble w, GLdouble h)
{
    if (graph != NULL)
    {
        graph->dim[0] = w;
        graph->dim[1] = h;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphDimGet(glgdGraph *graph, glgdVec2 dim)
{
    if (graph && dim)
    {
        dim[0] = graph->dim[0];
        dim[1] = graph->dim[1];
    }

    return GL_FALSE;
}

GLboolean
glgdGraphMarginSet(glgdGraph *graph, GLdouble margin)
{
    if (graph != NULL)
    {
        graph->margin = margin;
    }

    return GL_FALSE;
}

GLdouble
glgdGraphMarginGet(glgdGraph *graph)
{
    GLdouble    nodeMargin;

    if (graph)
    {
        return graph->margin;
    }

    return GLGDGRAPH_NODEMARGIN_DEFAULT;
}

GLboolean
glgdGraphLineColorSet(glgdGraph *graph, glgdColor col)
{
    if (graph && col)
    {
        graph->lineColor[0] = col[0];
        graph->lineColor[1] = col[1];
        graph->lineColor[2] = col[2];
        graph->lineColor[3] = col[3];

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphLineColorSetByList
(
    glgdGraph   *graph,
    GLdouble    r,
    GLdouble    g,
    GLdouble    b,
    GLdouble    a
)
{
    if (graph)
    {
        graph->lineColor[0] = r;
        graph->lineColor[1] = g;
        graph->lineColor[2] = b;
        graph->lineColor[3] = a;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphLineColorGet(glgdGraph *graph, glgdColor col)
{
    if (graph && col)
    {
        col[0] = graph->lineColor[0];
        col[1] = graph->lineColor[1];
        col[2] = graph->lineColor[2];
        col[3] = graph->lineColor[3];

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAttributeClear(glgdGraph *graph)
{
    if (graph != NULL)
    {
        return glgdBitfieldClear(&graph->attributes);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAttributeSet(glgdGraph *graph, int attrNdx)
{
    if (graph != NULL)
    {
        return glgdBitfieldSet(&graph->attributes, attrNdx);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAttributeToggle(glgdGraph *graph, int attrNdx)
{
    if (graph != NULL)
    {
        return glgdBitfieldToggle(&graph->attributes, attrNdx);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAttributeReset(glgdGraph *graph, int attrNdx)
{
    if (graph != NULL)
    {
        return glgdBitfieldReset(&graph->attributes, attrNdx);
    }

    return GL_FALSE;
}

GLboolean
glgdGraphAttributeIsSet(glgdGraph *graph, int attrNdx)
{
    if (graph != NULL)
    {
        return glgdBitfieldIsSet(&graph->attributes, attrNdx);
    }

    return GL_FALSE;
}

int
glgdVerbosity(int verbosity)
{
    if (verbosity >= 0)
    {
        s_verbosity = verbosity;
    }

    return s_verbosity;
}

int
glgdTrace(int verbosity, const char *fmt, ...)
{
    va_list     ap;

    if (s_verbosity >= verbosity)
    {
        va_start(ap, fmt);
        vprintf(fmt, ap);
        va_end(ap);
    }
}
