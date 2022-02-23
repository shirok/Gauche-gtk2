/*
 * glgdDraw.h
 *
 * OpenGL Graph Display primitive drawing module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDDRAW_H__
#define __GLGDDRAW_H__

SCM_DECL_BEGIN

/*
 * Enumerations
 */
typedef enum
{
    GLGDDRAW_BOXTYPE_NONE = 0,
    GLGDDRAW_BOXTYPE_UP,
    GLGDDRAW_BOXTYPE_DOWN,
    GLGDDRAW_BOXTYPE_FLAT,
    GLGDDRAW_BOXTYPE_BORDER,
    GLGDDRAW_BOXTYPE_SHADOW,
    GLGDDRAW_BOXTYPE_FRAME,
    GLGDDRAW_BOXTYPE_COUNT
} glgdDrawBoxType;

typedef enum
{
    GLGDDRAW_SLIDERTYPE_NONE = 0,
    GLGDDRAW_SLIDERTYPE_VERT,
    GLGDDRAW_SLIDERTYPE_VERT_FILL,
    GLGDDRAW_SLIDERTYPE_VERT_NICE,
    GLGDDRAW_SLIDERTYPE_HORIZ,
    GLGDDRAW_SLIDERTYPE_HORIZ_FILL,
    GLGDDRAW_SLIDERTYPE_HORIZ_NICE,
    GLGDDRAW_SLIDERTYPE_COUNT
} glgdDrawSliderType;

/*
 * Defines
 */
#define GLGDDRAW_COLORCLAMP(c)      ((c) > 1.0f ? 1.0f : (c))
#define GLGDDRAW_LIGHTEN            (1.5)
#define GLGDDRAW_DARKEN             (0.7)

/*
 * Global Variables
 */
extern glgdColor            g_colorBlack;
extern glgdColor            g_colorWhite;
extern glgdColor            g_colorRed;
extern glgdColor            g_colorGreen;
extern glgdColor            g_colorBlue;
extern glgdColor            g_colorYellow;
extern glgdColor            g_colorCyan;
extern glgdColor            g_colorMagenta;

/*
 * Module API
 */
void    glgdDrawZValueSet(GLdouble zValue);
void    glgdDrawZValueGet(GLdouble *zValue);
void    glgdDrawLineWidthSet(GLdouble lineWidth);
void    glgdDrawLineWidthGet(GLdouble *lineWidth);

void    glgdDrawLine(glgdVec2 xy, glgdVec2 wh, glgdColor col);
void    glgdDrawRect(glgdVec2 xy, glgdVec2 wh, glgdColor col);
void    glgdDrawBoundary(glgdVec2 xy, glgdVec2 wh, glgdColor col);
void    glgdDrawRectBoundary(glgdVec2 xy, glgdVec2 wh, glgdColor col);
void    glgdDrawQuad(glgdVec2 a, glgdVec2 b, glgdVec2 c, glgdVec2 d,
                     glgdColor col);
void    glgdDrawQuadBoundary(glgdVec2 a, glgdVec2 b, glgdVec2 c, glgdVec2 d,
                             glgdColor col);
void    glgdDrawBox(glgdDrawBoxType boxType, glgdVec2 xy, glgdVec2 wh,
                    glgdColor col, GLdouble borderWidth);
void    glgdDrawSlider(glgdDrawBoxType boxType, glgdDrawSliderType sliderType,
                       glgdVec2 xy, glgdVec2 wh, glgdColor col1, glgdColor col2,
                       GLdouble size, GLdouble val);

SCM_DECL_END

#endif  /* __GLGDDRAW_H__ */
