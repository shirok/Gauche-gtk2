/*
 * gldgCam.c
 *
 * OpenGL Graph Display camera control module implementation
 *
 * Written by: Shawn Taras
 */
#include <stdlib.h>
#include <math.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include "glgd.h"

/*
 * Defines
 */
#define _PI                     (M_PI)
#define _DEG2RAD(x)             ((x) * _PI / 180.0)

/*
 * External public functions
 */
GLboolean
glgdCamInit(glgdCam *cam)
{
    if (cam != NULL)
    {
        cam->flags = GLGDCAM_FLAG_INITIALIZED;
        glgdMatrixIdentity(cam->projMtx);
        glgdQuatIdentity(cam->camRot);
        cam->camPos[0] = 0.0;
        cam->camPos[1] = 0.0;
        cam->camPos[2] = 0.0;
        cam->mouseLast[0] = -1.0;
        cam->mouseLast[1] = -1.0;
        cam->tanFOV[0] = 0.414;     /* 45 degree FOV */
        cam->tanFOV[1] = 0.414;
        cam->winDim[0] = 0.0;
        cam->winDim[1] = 0.0;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamBegin(glgdCam *cam)
{
    glgdMatrix  camRotMtx;
    
    if (cam)
    {
        /* Load the projection matrix */
        glMatrixMode(GL_PROJECTION);
        glLoadMatrixd(cam->projMtx);
    
        /* Compute and load the modelview matrix */
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glLoadIdentity();
        glTranslated(cam->camPos[0], cam->camPos[1], cam->camPos[2]);
        glgdMatrixSetByQuat(camRotMtx, cam->camRot);
        glMultMatrixd(camRotMtx);
        
        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdCamBeginPick(glgdCam *cam, GLdouble mx, GLdouble my)
{
    GLint       viewport[4];
    glgdMatrix  camRotMtx;
        
    if (cam)
    {
        glGetIntegerv(GL_VIEWPORT, viewport);
        
        /* Load the projection matrix */
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        gluPickMatrix(mx, viewport[3] - my, 8.0, 8.0, viewport);
        glMultMatrixd(cam->projMtx);
    
        /* Compute and load the modelview matrix */
        glMatrixMode(GL_MODELVIEW);
        glPushMatrix();
        glLoadIdentity();
        glTranslated(cam->camPos[0], cam->camPos[1], cam->camPos[2]);
        glgdMatrixSetByQuat(camRotMtx, cam->camRot);
        glMultMatrixd(camRotMtx);
        
        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdCamUpdate(glgdCam *cam, glgdCamMode mode,
              GLdouble mx, GLdouble my, GLdouble frameTime)
{
    glgdQuat    qa, qb, qc;
    GLdouble    scale;
    GLdouble    ratio[2];
    GLdouble    dx, dy;
    GLboolean   something;

    if (frameTime <= 0.016667)
    {
        frameTime = 0.016667;
    }
    if (cam != NULL)
    {
        something = GL_FALSE;
        if (mode == GLGDCAM_MODE_NONE)
        {
            cam->mouseLast[0] = -1.0;
            cam->mouseLast[1] = -1.0;
        }
        else
        {
            if (cam->mouseLast[0] >= 0.0)
            {
                dx = mx - cam->mouseLast[0];
                dy = my - cam->mouseLast[1];
                if (mode == GLGDCAM_MODE_ORBIT)
                {
                    scale = 10.0 * frameTime;
                    glgdQuatSetByXRotation(qa, _DEG2RAD(dy * scale));
                    glgdQuatSetByYRotation(qb, _DEG2RAD(dx * scale));
                    glgdQuatMult(qc, qb, cam->camRot);
                    glgdQuatMult(cam->camRot, qc, qa);
                }
                else if (mode == GLGDCAM_MODE_ZOOM)
                {
                    scale = 10.0 * frameTime;
                    cam->camPos[2] += dx * scale;
                }
                else if (mode == GLGDCAM_MODE_PAN)
                {
                    ratio[0] = cam->camPos[2]*cam->tanFOV[0]/cam->winDim[0];
                    ratio[1] = cam->camPos[2]*cam->tanFOV[1]/cam->winDim[1];
                    cam->camPos[0] -= dx * ratio[0];
                    cam->camPos[1] += dy * ratio[1];
                }
            }
            cam->mouseLast[0] = mx;
            cam->mouseLast[1] = my;

            something = GL_TRUE;
        }

        return something;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamEnd(glgdCam *cam)
{
    if (cam)
    {
        glMatrixMode(GL_MODELVIEW);
        glPopMatrix();
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamPosSet(glgdCam *cam, GLdouble x, GLdouble y, GLdouble z)
{
    if (cam)
    {
        cam->camPos[0] = x;
        cam->camPos[1] = y;
        cam->camPos[2] = z;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamWinDimSet(glgdCam *cam, GLdouble w, GLdouble h)
{
    if (cam)
    {
        cam->winDim[0] = w;
        cam->winDim[1] = h;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamRotSet(glgdCam *cam, glgdQuat qrot)
{
    if (cam)
    {
        return glgdQuatSet(cam->camRot, qrot);
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamMouseSet(glgdCam *cam, GLdouble mx, GLdouble my)
{
    if (cam)
    {
        cam->mouseLast[0] = mx;
        cam->mouseLast[1] = my;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamFrameWidth(glgdCam *cam,
                  GLdouble left, GLdouble right,
                  GLdouble bottom, GLdouble top)
{
    GLdouble    z;
    GLdouble    w, h;
    
    w = right - left;
    h = top - bottom;
    
    if (cam && w > 0.0 && h > 0.0)
    {
        cam->camPos[0] = -GLGD_HALF(left + right);
        cam->camPos[1] = -GLGD_HALF(bottom + top);
        cam->camPos[2] = -(w * 0.5) / tan(cam->tanFOV[0]);
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamFrameHeight (glgdCam *cam,
                    GLdouble left, GLdouble right,
                    GLdouble bottom, GLdouble  top)
{
    GLdouble    z;
    GLdouble    w, h;
    
    w = right - left;
    h = top - bottom;
    
    if (cam && w > 0.0 && h > 0.0)
    {
        cam->camPos[0] = -GLGD_HALF(left + right);
        cam->camPos[1] = -GLGD_HALF(bottom + top);
        cam->camPos[2] = -(h * 0.5) / cam->tanFOV[1];
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamFrame(glgdCam *cam,
             GLdouble left, GLdouble right,
             GLdouble bottom, GLdouble top)
{
    GLdouble    w, h;
    
    w = right - left;
    h = top - bottom;
    
    if (cam && w > 0.0 && h > 0.0)
    {
        if (w > h)
        {
            glgdCamFrameWidth(cam, left, right, bottom, top);
        }
        else
        {
            glgdCamFrameHeight(cam, left, right, bottom, top);
        }
        
        glgdTrace(2, "(%g,%g,%g,%g) -> (%g,%g,%g)\n",
                  left, right, bottom, top,
                  cam->camPos[0], cam->camPos[1], cam->camPos[2]);

        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdCamPerspective(glgdCam *cam,
                   GLdouble    fovy,
                   GLdouble    aspect,
                   GLdouble    zNear,
                   GLdouble    zFar)
{
    GLdouble        fovyBy2;
    
    glgdMatrixPerspective(cam->projMtx, fovy, aspect, zNear, zFar);
    
    fovyBy2 = fovy * 0.5;
    cam->tanFOV[0] = tan(fovyBy2 / aspect);
    cam->tanFOV[1] = tan(fovyBy2);
    
    return GL_FALSE;
}

GLboolean
glgdCamFrustum(glgdCam     *cam,
               GLdouble    left,
               GLdouble    right,
               GLdouble    bottom,
               GLdouble    top,
               GLdouble    zNear,
               GLdouble    zFar)
{
    GLdouble        aspect;
    GLdouble        fovyBy2;
    
    glgdMatrixFrustum(cam->projMtx, left, right, bottom, top, zNear, zFar);

    aspect = (top - bottom) / (right - left);
    fovyBy2 = atan2((top - bottom) * 0.5, zNear);
    cam->tanFOV[0] = tan(fovyBy2 / aspect);
    cam->tanFOV[1] = tan(fovyBy2);
    
    return GL_FALSE;
}
                
GLboolean
glgdCamOrtho(glgdCam     *cam,
             GLdouble    left,
             GLdouble    right,
             GLdouble    bottom,
             GLdouble    top,
             GLdouble    zNear,
             GLdouble    zFar)
{
    glgdMatrixOrtho(cam->projMtx, left, right, bottom, top, zNear, zFar);
    
    return GL_FALSE;
}
                
