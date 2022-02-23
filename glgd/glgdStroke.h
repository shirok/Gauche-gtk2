/*
 * gldgStroke.h
 *
 * OpenGL Graph Display stroke font module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDSTROKE_H__
#define __GLGDSTROKE_H__

SCM_DECL_BEGIN

/*
 * Defines
 */
#define GLGDSTROKE_FLAG_INITIALIZED         (0x0001)
#define GLGDSTROKE_FLAG_CREATED             (0x0002)
#define GLGDSTROKE_FLAG_MODIF               (0x0004)
#define GLGDSTROKE_FLAG_INVERT              (0x0008)

#define GLGDSTROKE_POINTSIZEX_DEFAULT       (8.0)
#define GLGDSTROKE_POINTSIZEY_DEFAULT       (16.0)
#define GLGDSTROKE_TABSIZE_DEFAULT          (2)

/*
 * Type Definitions
 */
typedef struct _glgdStroke
{
    GLbitfield      flags;
    int             tabSize;
    glgdVec2        windowDim;
    glgdVec2        pointSize;
    glgdVec2        pos;
    glgdVec4        clip;
    glgdColor       col;
} glgdStroke;

/*
 * Module API
 */
glgdStroke  *glgdStrokeCreate(void);
glgdStroke  *glgdStrokeDestroy(glgdStroke *stroke);

void        glgdStrokeInit(glgdStroke *stroke);
void        glgdStrokeFini(glgdStroke *stroke);

glgdStroke  *glgdStrokeGetCurrent(void);
void        glgdStrokeSetCurrent(glgdStroke *stroke);

int         glgdStrokePrint(glgdStroke *stroke, const char *fmt, ...);
int         glgdStrokePrintVar(const char *fmt, va_list ap);

void        glgdStrokeTabSizeSet(glgdStroke *stroke, int tabSize);
void        glgdStrokePointSizeSet(glgdStroke *stroke, glgdVec2 pointSize);
void        glgdStrokePointSizeSetByList(glgdStroke *stroke,
                                         GLdouble w, GLdouble h);
void        glgdStrokePosSet(glgdStroke *stroke, glgdVec2 pos);
void        glgdStrokePosSetByList(glgdStroke *stroke, GLdouble x, GLdouble y);

void        glgdStrokeWindowDimSet(glgdStroke *stroke, glgdVec2 windowDim);
void        glgdStrokeWindowDimSetByList(glgdStroke *stroke,
                                         GLdouble w, GLdouble h);
void        glgdStrokeClipFullWindow(glgdStroke *stroke);
void        glgdStrokeClipSet(glgdStroke *stroke, glgdVec4 clipRect);
void        glgdStrokeClipSetByList(glgdStroke *stroke,
                                    GLdouble x1, GLdouble y1,
                                    GLdouble x2, GLdouble y2);

void        glgdStrokeColorSet(glgdStroke *stroke, glgdVec4 col);
void        glgdStrokeColorSetByList(glgdStroke *stroke,
                                     GLdouble r, GLdouble g,
                                     GLdouble b, GLdouble a);
int         glgdStrokeBuild(glgdStroke *stroke, int charNdx, int ndx);

SCM_DECL_END

#endif  /* __GLGDSTROKE_H__ */
