/*
 * gldgDraw.c
 *
 * OpenGL Graph Display primitive drawing module implementation
 *
 * Written by: Shawn Taras
 */
#include <stdlib.h>
#include <math.h>
#include <GL/gl.h>
#include "glgd.h"

/*
 * Defines
 */
#define GLGDDRAW_BW1        (4.0f)
#define GLGDDRAW_BW2        (2.0f)

/*
 * Global variables
 */
glgdColor            g_colorBlack = {0.0, 0.0, 0.0, 1.0};
glgdColor            g_colorWhite = {1.0, 1.0, 1.0, 1.0};
glgdColor            g_colorRed = {1.0, 0.0, 0.0, 1.0};
glgdColor            g_colorGreen = {0.0, 1.0, 0.0, 1.0};
glgdColor            g_colorBlue = {0.0, 0.0, 1.0, 1.0};
glgdColor            g_colorYellow = {1.0, 1.0, 0.0, 1.0};
glgdColor            g_colorCyan = {0.0, 1.0, 1.0, 1.0};
glgdColor            g_colorMagenta = {1.0, 0.0, 1.0, 1.0};

/*
 * Static local (to this module) variables
 */
static glgdColor        s_colorBlack = {0.0, 0.0, 0.0, 1.0};
static GLdouble         s_zValue = 0.0f;
static GLdouble         s_lineWidth = 1.0f;
static GLint            s_blendFunc[2] = {GL_ONE, GL_ZERO};

/*
 * Static local (to this module) functions
 */
static void
glgdPushAttributes(void)
{
    glPushAttrib(GL_ENABLE_BIT | GL_HINT_BIT | GL_LINE_BIT);
    glGetIntegerv(GL_BLEND_SRC, &s_blendFunc[0]);
    glGetIntegerv(GL_BLEND_DST, &s_blendFunc[1]);

    /* Common attributes for primitive drawing */
    glDisable(GL_TEXTURE_2D);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);

    glLineWidth((GLfloat)s_lineWidth);
}

static void
glgdPopAttributes(void)
{
    glPopAttrib();
    glBlendFunc(s_blendFunc[0], s_blendFunc[1]);
}

static void
glgdDrawColorSet(glgdColor col, GLboolean isLine)
{
    if (isLine)
    {
        glEnable(GL_LINE_SMOOTH);
        glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glColor4d(col[0], col[1], col[2], col[3]);
    }
    else if (col[3] < 1.0)
    {
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glColor4d(col[0], col[1], col[2], col[3]);
    }
    else
    {
        glDisable(GL_BLEND);
        glColor3d(col[0], col[1], col[2]);
    }
}

static GLdouble
glgdDrawLERP
(
    GLdouble    val,
    GLdouble    smin,
    GLdouble    smax,
    GLdouble    gmin,
    GLdouble    gmax
)
{
    if (smin == smax)
    {
        return gmax;
    }
    else
    {
        return gmin + (gmax - gmin) * (val - smin) / (smax - smin);
    }
}

/*
 * External public functions
 */
void
glgdDrawZValueSet(GLdouble zValue)
{
    s_zValue = zValue;
}

void
glgdDrawZValueGet(GLdouble *zValue)
{
    if (zValue)
    {
        *zValue = s_zValue;
    }
}

void
glgdDrawLineWidthSet(GLdouble lineWidth)
{
    s_lineWidth = lineWidth;
}

void
glgdDrawLineWidthGet(GLdouble *lineWidth)
{
    if (lineWidth)
    {
        *lineWidth = s_lineWidth;
    }
}

void
glgdDrawLine(glgdVec2 xy, glgdVec2 wh, glgdColor col)
{
    GLdouble    x1, y1;
    GLdouble    x2, y2;

    x1 = xy[0];
    y1 = xy[1];
    x2 = x1 + wh[0] - 1.0f;
    y2 = y1 + wh[1] - 1.0f;

    glgdDrawColorSet(col, GL_TRUE);

    glBegin(GL_LINE_STRIP);
        glVertex3d(x1, y1, s_zValue);
        glVertex3d(x2, y2, s_zValue);
    glEnd();
}

void
glgdDrawRect(glgdVec2 xy, glgdVec2 wh, glgdColor col)
{
    GLdouble    x1, y1;
    GLdouble    x2, y2;

    x1 = xy[0];
    y1 = xy[1];
    x2 = x1 + wh[0];
    y2 = y1 + wh[1];

    glgdDrawColorSet(col, GL_FALSE);

    glBegin(GL_TRIANGLE_STRIP);
        glVertex3d(x1, y1, s_zValue);
        glVertex3d(x1, y2, s_zValue);
        glVertex3d(x2, y1, s_zValue);
        glVertex3d(x2, y2, s_zValue);
    glEnd();
}

void
glgdDrawBoundary(glgdVec2 xy, glgdVec2 wh, glgdColor col)
{
    GLdouble    x1, y1;
    GLdouble    x2, y2;

    x1 = xy[0];
    y1 = xy[1];
    x2 = x1 + wh[0] - 1.0f;
    y2 = y1 + wh[1] - 1.0f;

    glgdDrawColorSet(col, GL_TRUE);

    glBegin(GL_LINE_LOOP);
        glVertex3d(x1, y1, s_zValue);
        glVertex3d(x2, y1, s_zValue);
        glVertex3d(x2, y2, s_zValue);
        glVertex3d(x1, y2, s_zValue);
    glEnd();
}

void
glgdDrawRectBoundary(glgdVec2 xy, glgdVec2 wh, glgdColor col)
{
    GLdouble    x1, y1;
    GLdouble    x2, y2;

    x1 = xy[0];
    y1 = xy[1];
    x2 = x1 + wh[0] - 1.0f;
    y2 = y1 + wh[1];

    /* Draw the rectangle */
    glgdDrawColorSet(col, GL_FALSE);

    glBegin(GL_TRIANGLE_STRIP);
        glVertex3d(x1 + 1.0f, y1, s_zValue);
        glVertex3d(x1 + 1.0f, y2, s_zValue);
        glVertex3d(x2,        y1, s_zValue);
        glVertex3d(x2,        y2, s_zValue);
    glEnd();

    /* Draw the boundary */
    glgdDrawColorSet(s_colorBlack, GL_TRUE);
    glBegin(GL_LINE_LOOP);
        glVertex3d(x1, y1, s_zValue);
        glVertex3d(x2, y1, s_zValue);
        glVertex3d(x2, y2, s_zValue);
        glVertex3d(x1, y2, s_zValue);
    glEnd();
}

void
glgdDrawQuad
(
    glgdVec2    a,
    glgdVec2    b,
    glgdVec2    c,
    glgdVec2    d,
    glgdColor   col
)
{
    glgdDrawColorSet(col, GL_FALSE);

    glBegin(GL_TRIANGLE_STRIP);
        glVertex3d(a[0], a[1], s_zValue);
        glVertex3d(b[0], b[1], s_zValue);
        glVertex3d(d[0], d[1], s_zValue);
        glVertex3d(c[0], c[1], s_zValue);
    glEnd();
}

void
glgdDrawQuadBoundary
(
    glgdVec2    a,
    glgdVec2    b,
    glgdVec2    c,
    glgdVec2    d,
    glgdColor   col
)
{
    glgdDrawQuad(a, b, c, d, col);

    /* Draw the boundary */
    glgdDrawColorSet(s_colorBlack, GL_TRUE);

    glBegin(GL_LINE_LOOP);
        glVertex3d(a[0], a[1], s_zValue);
        glVertex3d(b[0], b[1], s_zValue);
        glVertex3d(c[0], c[1], s_zValue);
        glVertex3d(d[0], d[1], s_zValue);
    glEnd();
}

void
glgdDrawBox
(
    glgdDrawBoxType boxType,
    glgdVec2        xy,
    glgdVec2        wh,
    glgdColor       col,
    GLdouble        borderWidth
)
{
    GLdouble    borderHeight;
    glgdVec2    tmpXY;
    glgdVec2    tmpWH;
    glgdVec2    qxy[4];
    glgdColor   tmpCol;

    if (boxType == GLGDDRAW_BOXTYPE_NONE)
    {
        return;
    }

    borderHeight = borderWidth;

    glgdPushAttributes();
    switch (boxType)
    {
        case GLGDDRAW_BOXTYPE_UP:
            tmpXY[0] = xy[0] + borderWidth;
            tmpXY[1] = xy[1] + borderHeight;
            tmpWH[0] = wh[0] - 2.0f * borderWidth - 1.0f;
            tmpWH[1] = wh[1] - 2.0f * borderHeight - 1.0f;
            glgdDrawRect(tmpXY, tmpWH, col);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0];
            tmpXY[1] = xy[1];
            tmpWH[0] = wh[0] - 1.0f;
            tmpWH[1] = borderHeight;
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_LIGHTEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_LIGHTEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_LIGHTEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0];
            tmpXY[1] = xy[1] + wh[1] - borderHeight - 1.0f;
            tmpWH[0] = wh[0] - 1.0f;
            tmpWH[1] = borderHeight;
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];
            qxy[0][0] = xy[0];
            qxy[0][1] = xy[1];
            qxy[1][0] = xy[0];
            qxy[1][1] = xy[1] + wh[1] - 1.0f;
            qxy[2][0] = xy[0] + borderWidth;
            qxy[2][1] = xy[1] + wh[1] - borderHeight - 1.0f;
            qxy[3][0] = xy[0] + borderWidth;
            qxy[3][1] = xy[1] + borderHeight;
            glgdDrawQuad(qxy[0], qxy[1], qxy[2], qxy[3], tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_LIGHTEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_LIGHTEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_LIGHTEN);
            qxy[0][0] = xy[0] + wh[0] - borderWidth - 1.0f;
            qxy[0][1] = xy[1] + borderHeight;
            qxy[1][0] = xy[0] + wh[0] - borderWidth - 1.0f;
            qxy[1][1] = xy[1] + wh[1] - borderHeight - 1.0f;
            qxy[2][0] = xy[0] + wh[0] - 1.0f;
            qxy[2][1] = xy[1] + wh[1] - 1.0f;
            qxy[3][0] = xy[0] + wh[0] - 1.0f;
            qxy[3][1] = xy[1];
            glgdDrawQuad(qxy[0], qxy[1], qxy[2], qxy[3], tmpCol);

            glgdDrawBoundary(xy, wh, col);
        break;

        case GLGDDRAW_BOXTYPE_DOWN:
            tmpXY[0] = xy[0] + borderWidth + 1.0f;
            tmpXY[1] = xy[1] + borderHeight + 1.0f;
            tmpWH[0] = wh[0] - 2.0f * borderWidth - 2.0f;
            tmpWH[1] = wh[1] - 2.0f * borderHeight - 1.0f;
            glgdDrawRectBoundary(tmpXY, tmpWH, col);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0];
            tmpXY[1] = xy[1];
            tmpWH[0] = wh[0];
            tmpWH[1] = borderHeight;
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_LIGHTEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_LIGHTEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_LIGHTEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0];
            tmpXY[1] = xy[1] + wh[1] - borderHeight;
            tmpWH[0] = wh[0];
            tmpWH[1] = borderHeight;
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];
            qxy[0][0] = xy[0];
            qxy[0][1] = xy[1];
            qxy[1][0] = xy[0];
            qxy[1][1] = xy[1] + wh[1];
            qxy[2][0] = xy[0] + borderWidth;
            qxy[2][1] = xy[1] + wh[1] - borderHeight;
            qxy[3][0] = xy[0] + borderWidth;
            qxy[3][1] = xy[1] + borderHeight;
            glgdDrawQuad(qxy[0], qxy[1], qxy[2], qxy[3], tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_LIGHTEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_LIGHTEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_LIGHTEN);
            qxy[0][0] = xy[0] + wh[0] - borderWidth;
            qxy[0][1] = xy[1] + borderHeight;
            qxy[1][0] = xy[0] + wh[0] - borderWidth;
            qxy[1][1] = xy[1] + wh[1] - borderHeight;
            qxy[2][0] = xy[0] + wh[0];
            qxy[2][1] = xy[1] + wh[1];
            qxy[3][0] = xy[0] + wh[0];
            qxy[3][1] = xy[1];
            glgdDrawQuad(qxy[0], qxy[1], qxy[2], qxy[3], tmpCol);
        break;

        case GLGDDRAW_BOXTYPE_FLAT:
            glgdDrawRect(xy, wh, col);
        break;

        case GLGDDRAW_BOXTYPE_BORDER:
            glgdDrawRectBoundary(xy, wh, col);
        break;

        case GLGDDRAW_BOXTYPE_SHADOW:
            /* The Shadow */
            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];

            tmpXY[0] = xy[0] + borderWidth;
            tmpXY[1] = xy[1] + wh[1];
            tmpWH[0] = wh[0];
            tmpWH[1] = borderHeight;
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            tmpXY[0] = xy[0] + wh[0];
            tmpXY[1] = xy[1] + borderHeight;
            tmpWH[0] = borderWidth;
            tmpWH[1] = wh[1];
            glgdDrawRect(tmpXY, tmpWH, tmpCol);

            /* The box */
            glgdDrawRectBoundary(xy, wh, col);
        break;

        case GLGDDRAW_BOXTYPE_FRAME:
            glgdDrawRect(xy, wh, col);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_DARKEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_DARKEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_DARKEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0];
            tmpXY[1] = xy[1];
            tmpWH[0] = wh[0] - 2.0f;
            tmpWH[1] = wh[1] - 2.0f;
            glgdDrawBoundary(tmpXY, tmpWH, tmpCol);

            tmpCol[0] = GLGDDRAW_COLORCLAMP(col[0] * GLGDDRAW_LIGHTEN);
            tmpCol[1] = GLGDDRAW_COLORCLAMP(col[1] * GLGDDRAW_LIGHTEN);
            tmpCol[2] = GLGDDRAW_COLORCLAMP(col[2] * GLGDDRAW_LIGHTEN);
            tmpCol[3] = col[3];
            tmpXY[0] = xy[0] + 2.0f;
            tmpXY[1] = xy[1] + 2.0f;
            tmpWH[0] = wh[0] - 2.0f;
            tmpWH[1] = wh[1] - 2.0f;
            glgdDrawBoundary(tmpXY, tmpWH, tmpCol);
        break;

        default:
        break;
    }
    glgdPopAttributes();
}

void
glgdDrawSlider
(
    glgdDrawBoxType     boxType,
    glgdDrawSliderType  sliderType,
    glgdVec2            xy,
    glgdVec2            wh,
    glgdColor           col1,
    glgdColor           col2,
    GLdouble            size,
    GLdouble            val
)
{
    GLdouble            xsl, ysl, wsl, hsl;
    glgdDrawBoxType     slBoxType;
    glgdVec2            pos, dim;

    glgdPushAttributes();
    switch (sliderType)
    {
        case GLGDDRAW_SLIDERTYPE_VERT:
        case GLGDDRAW_SLIDERTYPE_VERT_NICE:
            hsl = size * (wh[1] - 2.0f * GLGDDRAW_BW1);
            ysl = glgdDrawLERP(1.0f - val, 0.0f, 1.0f, xy[1] + GLGDDRAW_BW1, xy[1] + wh[1] - GLGDDRAW_BW1 - hsl);
            wsl = wh[0] - 2.0f * GLGDDRAW_BW1;
            xsl = xy[0] + GLGDDRAW_BW1;
        break;

        case GLGDDRAW_SLIDERTYPE_HORIZ:
        case GLGDDRAW_SLIDERTYPE_HORIZ_NICE:
            wsl = size * (wh[0] - 2.0f * GLGDDRAW_BW1);
            xsl = glgdDrawLERP(val, 0.0f, 1.0f, xy[0] + GLGDDRAW_BW1, xy[0] + wh[0] - GLGDDRAW_BW1 - wsl);
            hsl = wh[1] - 2.0f * GLGDDRAW_BW1;
            ysl = xy[1] + GLGDDRAW_BW1;
        break;

        case GLGDDRAW_SLIDERTYPE_VERT_FILL:
            hsl = val * (wh[1] - 2.0f * GLGDDRAW_BW1);
            ysl = xy[1] + GLGDDRAW_BW1 + (1.0f - val) * (wh[1] - 2.0f * GLGDDRAW_BW1);
            wsl = wh[0] - 2.0f * GLGDDRAW_BW1;
            xsl = xy[0] + GLGDDRAW_BW1;
        break;

        case GLGDDRAW_SLIDERTYPE_HORIZ_FILL:
            wsl = val * (wh[0] - 2.0f * GLGDDRAW_BW1);
            xsl = xy[0] + GLGDDRAW_BW1;
            hsl = wh[1] - 2.0f * GLGDDRAW_BW1;
            ysl = xy[1] + GLGDDRAW_BW1;
        break;

        default:
            return;
    }

    /* Draw the slider */
    glgdDrawBox(boxType, xy, wh, col1, GLGDDRAW_BW1);
    if (sliderType == GLGDDRAW_SLIDERTYPE_VERT_NICE)
    {
        pos[0] = xy[0] + wh[0] / 2.0f - 1.0f;
        pos[1] = xy[1] + GLGDDRAW_BW1;
        dim[0] = 2.0f;
        dim[1] = wh[1] - 2.0f * GLGDDRAW_BW1;
        glgdDrawBox(GLGDDRAW_BOXTYPE_FLAT, pos, dim, s_colorBlack, 0.0f);

        pos[0] = xsl;
        pos[1] = ysl;
        dim[0] = wsl;
        dim[1] = hsl;
        glgdDrawBox(GLGDDRAW_BOXTYPE_UP, pos, dim, col1, GLGDDRAW_BW1);

        pos[0] = xsl + 2.0f;
        pos[1] = ysl + hsl / 2.0f - 2.0f;
        dim[0] = wsl - 2.0f;
        dim[1] = 4.0f;
        glgdDrawBox(GLGDDRAW_BOXTYPE_DOWN, pos, dim, col2, 1.0f);
    }
    else if (sliderType == GLGDDRAW_SLIDERTYPE_HORIZ_NICE)
    {
        pos[0] = xy[0] + GLGDDRAW_BW1;
        pos[1] = xy[1] + wh[1] / 2.0f - 1.0f;
        dim[0] = wh[0] - 2.0f * GLGDDRAW_BW1;
        dim[1] = 2.0f;
        glgdDrawBox(GLGDDRAW_BOXTYPE_FLAT, pos, dim, s_colorBlack, 0.0f);

        pos[0] = xsl;
        pos[1] = ysl;
        dim[0] = wsl;
        dim[1] = hsl;
        glgdDrawBox(GLGDDRAW_BOXTYPE_UP, pos, dim, col1, GLGDDRAW_BW1);

        pos[0] = xsl + wsl / 2.0f - 2.0f;
        pos[1] = ysl + 1.0f;
        dim[0] = 4.0f;
        dim[1] = hsl - 2.0f;
        glgdDrawBox(GLGDDRAW_BOXTYPE_DOWN, pos, dim, col2, 1.0f);
    }
    else
    {
        switch (boxType)
        {
            case GLGDDRAW_BOXTYPE_UP:
            case GLGDDRAW_BOXTYPE_DOWN:
                slBoxType = GLGDDRAW_BOXTYPE_UP;
            break;

            case GLGDDRAW_BOXTYPE_FRAME:
                slBoxType = GLGDDRAW_BOXTYPE_FRAME;
            break;

            default:
                slBoxType = GLGDDRAW_BOXTYPE_BORDER;
            break;
        }
        pos[0] = xsl;
        pos[1] = ysl;
        dim[0] = wsl;
        dim[1] = hsl;
        glgdDrawBox(slBoxType, pos, dim, col2, GLGDDRAW_BW2);
    }
    glgdPopAttributes();
}
