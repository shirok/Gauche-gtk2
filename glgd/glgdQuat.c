/*
 * gldgQuat.c
 *
 * OpenGL Graph Display quaternion utility module implementation
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
#define _EPSILON            (0.0005)
#define _PI                 (M_PI)

/*
 * External public functions
 */
GLboolean
glgdQuatIdentity(glgdQuat q)
{
    if (q != NULL)
    {
        q[0] = 0.0;
        q[1] = 0.0;
        q[2] = 0.0;
        q[3] = 1.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSet(glgdQuat dst, glgdQuat src)
{
    if (src && dst)
    {
        dst[0] = src[0];
        dst[1] = src[1];
        dst[2] = src[2];
        dst[3] = src[3];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByList(glgdQuat dst, GLdouble x, GLdouble y, GLdouble z, GLdouble w)
{
    if (dst != NULL)
    {
        dst[0] = x;
        dst[1] = y;
        dst[2] = z;
        dst[3] = w;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByEuler(glgdQuat q, GLdouble xRad, GLdouble yRad, GLdouble zRad)
{
    GLdouble        sinX, sinY, sinZ;
    GLdouble        cosX, cosY, cosZ;
    GLdouble        halfX, halfY, halfZ;
    
    if (q != NULL)
    {
        halfX = xRad * 0.5;
        halfY = yRad * 0.5;
        halfZ = zRad * 0.5;
        
        cosX = cos(halfX);
        cosY = cos(halfY);
        cosZ = cos(halfZ);
        
        sinX = sin(halfX);
        sinY = sin(halfY);
        sinZ = sin(halfZ);
        
        q[0] = sinX * cosY * cosZ - cosX * sinY * sinZ;
        q[1] = cosX * sinY * cosZ + sinX * cosY * sinZ;
        q[2] = cosX * cosY * sinZ - sinX * sinY * cosZ;
        q[3] = cosX * cosY * cosZ + sinX * sinY * sinZ;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByXRotation(glgdQuat q, GLdouble xRad)
{
    GLdouble    sinX;
    GLdouble    cosX;
    GLdouble    halfX;
    
    if (q != NULL)
    {
        halfX = xRad * 0.5;
        cosX = cos(halfX);
        sinX = sin(halfX);
        
        q[0] = sinX;
        q[1] = 0.0;
        q[2] = 0.0;
        q[3] = cosX;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByYRotation(glgdQuat q, GLdouble yRad)
{
    GLdouble    sinY;
    GLdouble    cosY;
    GLdouble    halfY;
    
    if (q != NULL)
    {
        halfY = yRad * 0.5;
        cosY = cos(halfY);
        sinY = sin(halfY);
        
        q[0] = 0.0;
        q[1] = sinY;
        q[2] = 0.0;
        q[3] = cosY;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByZRotation(glgdQuat q, GLdouble zRad)
{
    GLdouble    sinZ;
    GLdouble    cosZ;
    GLdouble    halfZ;
    
    if (q != NULL)
    {
        halfZ = zRad * 0.5;
        cosZ = cos(halfZ);
        sinZ = sin(halfZ);
        
        q[0] = 0.0;
        q[1] = 0.0;
        q[2] = sinZ;
        q[3] = cosZ;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSetByNormalizedAxis(glgdQuat q, GLdouble *axis, GLdouble thetaRad)
{
    GLdouble    sinTheta;
    GLdouble    cosTheta;
    GLdouble    halfTheta;
    
    if (q && axis)
    {
        halfTheta = thetaRad * 0.5;
        cosTheta = cos(halfTheta);
        sinTheta = sin(halfTheta);
        
        q[0] = sinTheta * axis[0];
        q[1] = sinTheta * axis[1];
        q[2] = sinTheta * axis[2];
        q[3] = cosTheta;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatAdd(glgdQuat dst, glgdQuat qa, glgdQuat qb)
{
    if (dst && qa && qb)
    {
        dst[0] = qa[0] + qb[0];
        dst[1] = qa[1] + qb[1];
        dst[2] = qa[2] + qb[2];
        dst[3] = qa[3] + qb[3];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSub(glgdQuat dst, glgdQuat qa, glgdQuat qb)
{
    if (dst && qa && qb)
    {
        dst[0] = qa[0] - qb[0];
        dst[1] = qa[1] - qb[1];
        dst[2] = qa[2] - qb[2];
        dst[3] = qa[3] - qb[3];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatMult(glgdQuat dst, glgdQuat qa, glgdQuat qb)
{
    if (dst && qa && qb)
    {
        dst[0] = qa[3]*qb[0] + qa[0]*qb[3] + qa[1]*qb[2] - qa[2]*qb[1];
        dst[1] = qa[3]*qb[1] + qa[1]*qb[3] + qa[2]*qb[0] - qa[0]*qb[2];
        dst[2] = qa[3]*qb[2] + qa[2]*qb[3] + qa[0]*qb[1] - qa[1]*qb[0];
        dst[3] = qa[3]*qb[3] - qa[0]*qb[0] - qa[1]*qb[1] - qa[2]*qb[2];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatLog(glgdQuat dst, glgdQuat src)
{
    GLdouble    scale, theta;

    if (dst && src)
    {
        scale = sqrt(_SQR(src[0]) + _SQR(src[1]) + _SQR(src[2]));
        if (scale > 0.0)
        {
            theta = atan2(scale, src[3]);
            scale = theta / scale;
        }
        
        dst[0] = src[0] * scale;
        dst[1] = src[1] * scale;
        dst[2] = src[2] * scale;
        dst[3] = 0.0f;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatExp(glgdQuat dst, glgdQuat src)
{
    GLdouble    scale, theta;

    if (dst && src)
    {
        theta = sqrt(_SQR(src[0]) + _SQR(src[1]) + _SQR(src[2]));
        scale = (theta > _EPSILON) ? (sin(theta) / theta) : 1.0;
        
        dst[0] = src[0] * scale;
        dst[1] = src[1] * scale;
        dst[2] = src[2] * scale;
        dst[3] = cos(theta);
        
        return GL_TRUE;
    }

    return GL_FALSE;    
}

GLboolean
glgdQuatConjugate(glgdQuat dst, glgdQuat src)
{
    if (dst && src)
    {
        dst[0] = -src[0];
        dst[1] = -src[1];
        dst[2] = -src[2];
        dst[3] =  src[3];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatInverse(glgdQuat dst, glgdQuat src)
{
    GLdouble    len2;
    GLdouble    factor;
    
    if (dst && src)
    {
        len2 = _SQR(src[0]) + _SQR(src[1]) + _SQR(src[2]) + _SQR(src[3]);
        if (len2 > 0.0)
        {
            factor = 1.0f / sqrt(len2);
            
            dst[0] = -src[0] * factor;
            dst[1] = -src[1] * factor;
            dst[2] = -src[2] * factor;
            dst[3] = -src[3] * factor;
            
            return GL_TRUE;
        }
        else
        {
            glgdQuatIdentity(dst);
        }
    }
    
    return GL_FALSE;
}

GLboolean
glgdQuatSlerp(glgdQuat dst, glgdQuat qa, glgdQuat qb, GLdouble t)
{
    GLdouble    omega;
    GLdouble    sinOmega, cosOmega;
    GLdouble    startScale, endScale;

    if (dst && qa && qb)
    {
        cosOmega = glgdQuatDot(qa, qb);

        if ((1.0 + cosOmega) > _EPSILON)
        {
            if ((1.0 - cosOmega) > _EPSILON)
            {
                omega      = acos(cosOmega);
                sinOmega   = sin(omega);
                startScale = sin((1.0 - t) * omega) / sinOmega;
                endScale   = sin(t * omega) / sinOmega;
            }
            else
            {
                startScale = 1.0 - t;
                endScale   = t;
            }

            dst[0] = qa[0] * startScale + qb[0] * endScale;
            dst[1] = qa[1] * startScale + qb[1] * endScale;
            dst[2] = qa[2] * startScale + qb[2] * endScale;
            dst[3] = qa[3] * startScale + qb[3] * endScale;
        }
        else
        {
            dst[0] = -qa[1];
            dst[1] =  qa[0];
            dst[2] = -qa[3];
            dst[3] =  qa[2];

            startScale = sin((0.5f - t) * _PI);
            endScale   = sin(t * _PI);

            dst[0] = qa[0] * startScale + qb[0] * endScale;
            dst[1] = qa[1] * startScale + qb[1] * endScale;
            dst[2] = qa[2] * startScale + qb[2] * endScale;
            dst[3] = qa[3] * startScale + qb[3] * endScale;
        }
        
        return GL_TRUE;
    }

    return GL_FALSE;
}

GLdouble
glgdQuatDot(glgdQuat qa, glgdQuat qb)
{
    if (qa && qb)
    {
        return ((qa[0]*qb[0]) + (qa[1]*qb[1]) + (qa[2]*qb[2]) + (qa[3]*qb[3]));
    }
    
    return 0.0;
}
