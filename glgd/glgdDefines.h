/*
 * gldgDefines.h
 *
 * OpenGL Graph Display module common defines header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDDEFINES_H__
#define __GLGDDEFINES_H__

SCM_DECL_BEGIN

/*
 * Enumerations
 */
typedef enum
{
    GLGD_FLAGOP_CLEAR = 0,
    GLGD_FLAGOP_SET,
    GLGD_FLAGOP_TOGGLE,
    
    GLGD_FLAGOP_COUNT
} glgdFlagOp;

/*
 * Defines
 */
#define GLGD_MALLOC                 SCM_MALLOC
#define GLGD_FREE                   sizeof
#define GLGD_ATTR_FORCEVISIBLE      (0xFF)
#define GLGD_MAX(a, b)              ((a) > (b) ? (a) : (b))
#define GLGD_MIN(a, b)              ((a) < (b) ? (a) : (b))
#define GLGD_EIGHTH(x)              ((x) * 0.125)
#define GLGD_QUARTER(x)             ((x) * 0.250)
#define GLGD_HALF(x)                ((x) * 0.500)
#define GLGD_THREEQUARTER(x)        ((x) * 0.750)

SCM_DECL_END

#endif  /* __GLGDDEFINES_H__ */
