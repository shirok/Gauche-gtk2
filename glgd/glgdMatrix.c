/*
 * gldgMatrix.c
 *
 * OpenGL Graph Display matrix utility module implementation
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
#define _SQR(x)             ((x) * (x))

/*
 * Static local (to this module) variables
 */
static char     s_mtxFmt[32] = "|%7.4f %7.4f %7.4f %12.4f|\n";

/*
 * External public functions
 */
GLboolean
glgdMatrixIdentity(glgdMatrix mtx)
{
    if (mtx != NULL)
    {
        mtx[ 0] = 1.0;
        mtx[ 1] = 0.0;
        mtx[ 2] = 0.0;
        mtx[ 3] = 0.0;

        mtx[ 4] = 0.0;
        mtx[ 5] = 1.0;
        mtx[ 6] = 0.0;
        mtx[ 7] = 0.0;

        mtx[ 8] = 0.0;
        mtx[ 9] = 0.0;
        mtx[10] = 1.0;
        mtx[11] = 0.0;

        mtx[12] = 0.0;
        mtx[13] = 0.0;
        mtx[14] = 0.0;
        mtx[15] = 1.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdMatrixDump(glgdMatrix mtx, int indent)
{
    int         i;
    int         ndx;

    ndx = 0;    
    for (i=0; i<4; i++)
    {
        printf(s_mtxFmt, mtx[ndx], mtx[ndx+1], mtx[ndx+2], mtx[ndx+3]);

        ndx += 4;
    }
}

GLboolean
glgdMatrixSetByQuat(glgdMatrix mtx, glgdQuat q)
{
    GLdouble        s;
    GLdouble        xs;
    GLdouble        ys;
    GLdouble        zs;
    GLdouble        wx;
    GLdouble        wy;
    GLdouble        wz;
    GLdouble        xx;
    GLdouble        xy;
    GLdouble        xz;
    GLdouble        yy;
    GLdouble        yz;
    GLdouble        zz;

    if (mtx && q)
    {
        s = 2.0 / (_SQR(q[0]) + _SQR(q[1]) + _SQR(q[2]) + _SQR(q[3]));

        xs = q[0] * s;
        ys = q[1] * s;
        zs = q[2] * s;

        wx = q[3] * xs;
        wy = q[3] * ys;
        wz = q[3] * zs;

        xx = q[0] * xs;
        xy = q[0] * ys;
        xz = q[0] * zs;

        yy = q[1] * ys;
        yz = q[1] * zs;
        zz = q[2] * zs;

        mtx[ 0] = 1.0 - (yy + zz);
        mtx[ 4] = xy - wz;
        mtx[ 8] = xz + wy;
        mtx[12] = 0.0;
        
        mtx[ 1] = xy + wz;
        mtx[ 5] = 1.0 - (xx + zz);
        mtx[ 9] = yz - wx;
        mtx[13] = 0.0;
        
        mtx[ 2] = xz - wy;
        mtx[ 6] = yz + wx;
        mtx[10] = 1.0 - (xx + yy);
        mtx[14] = 0.0;
        
        mtx[ 3] = 0.0;
        mtx[ 7] = 0.0;
        mtx[11] = 0.0;
        mtx[15] = 1.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdMatrixPerspective
(
    glgdMatrix  mtx,
    GLdouble    fovy,
    GLdouble    aspect,
    GLdouble    zNear,
    GLdouble    zFar
)
{
    GLdouble    xmin, xmax;
    GLdouble    ymin, ymax;
    
    if (mtx != NULL)
    {
        ymax = zNear * tan(fovy * M_PI / 360.0f);
        ymin = -ymax;
        xmin = ymin * aspect;
        xmax = ymax * aspect;
        
        glgdMatrixFrustum(mtx, xmin, xmax, ymin, ymax, zNear, zFar);
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdMatrixFrustum
(
    glgdMatrix  mtx,
    GLdouble    left,
    GLdouble    right,
    GLdouble    bottom,
    GLdouble    top,
    GLdouble    zNear,
    GLdouble    zFar
)
{
    GLdouble    rlInv;
    GLdouble    tbInv;
    GLdouble    fnInv;
    GLdouble    a, b, c, d;
    GLdouble    x, y;
    
    if (mtx != NULL)
    {
        rlInv = 1.0 / (right - left);
        tbInv = 1.0 / (top - bottom);
        fnInv = 1.0 / (zFar - zNear);
        
        x = (2.0 * zNear) * rlInv;
        y = (2.0 * zNear) * tbInv;
        a = (right + left) * rlInv;
        b = (top + bottom) * tbInv;
        c = -(zFar + zNear) * fnInv;
        d = -(2.0 * zFar * zNear) * fnInv;
        
        mtx[ 0] = x;    mtx[ 1] = 0.0;  mtx[ 2] = 0.0;  mtx[ 3] = 0.0;
        mtx[ 4] = 0.0;  mtx[ 5] = y;    mtx[ 6] = 0.0;  mtx[ 7] = 0.0;
        mtx[ 8] = a;    mtx[ 9] = b;    mtx[10] = c;    mtx[11] = -1.0;
        mtx[12] = 0.0;  mtx[13] = 0.0;  mtx[14] = d;    mtx[15] = 0.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdMatrixOrtho
(
    glgdMatrix  mtx,
    GLdouble    left,
    GLdouble    right,
    GLdouble    bottom,
    GLdouble    top,
    GLdouble    zNear,
    GLdouble    zFar
)
{
    GLdouble    x, y, z;
    GLdouble    tx, ty, tz;
    glgdMatrix  m;
    
    if (mtx != NULL)
    {
        x = +2.0 / (right - left);
        y = +2.0 / (top - bottom);
        z = -2.0 / (zFar - zNear);
        tx = -(right + left) / (right - left);
        ty = -(top + bottom) / (top - bottom);
        tz = -(zFar + zNear) / (zFar - zNear);      

        mtx[ 0] = x;    mtx[ 1] = 0.0;  mtx[ 2] = 0.0;  mtx[ 3] = 0.0;
        mtx[ 4] = 0.0;  mtx[ 5] = y;    mtx[ 6] = 0.0;  mtx[ 7] = 0.0;
        mtx[ 8] = 0.0;  mtx[ 9] = 0.0;  mtx[10] = z;    mtx[11] = 0.0;
        mtx[12] = tx;   mtx[13] = ty;   mtx[14] = tz;   mtx[15] = 1.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}
