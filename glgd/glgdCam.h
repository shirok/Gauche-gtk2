/*
 * glgdCam.h
 *
 * OpenGL Graph Display camera control module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDCAM_H__
#define __GLGDCAM_H__

SCM_DECL_BEGIN

/*
 * Enumerations
 */
typedef enum
{
    GLGDCAM_MODE_NONE = 0,
    GLGDCAM_MODE_ORBIT,
    GLGDCAM_MODE_ZOOM,
    GLGDCAM_MODE_PAN,

    GLGDCAM_MODE_COUNT
} glgdCamMode;

/*
 * Defines
 */
#define GLGDCAM_FLAG_INITIALIZED            (0x0001)

/*
 * Type Definitions
 */
typedef struct _glgdCam
{
    GLbitfield      flags;
    glgdMatrix      projMtx;
    glgdQuat        camRot;
    GLdouble        camPos[3];
    GLdouble        mouseLast[2];
    GLdouble        tanFOV[2];
    GLdouble        winDim[2];
} glgdCam;

typedef GLboolean   (*glgdCamCtrlFn)(glgdCam *cam);

/*
 * Module API
 */
GLboolean   glgdCamInit(glgdCam *cam);
GLboolean   glgdCamBegin(glgdCam *cam);
GLboolean   glgdCamBeginPick(glgdCam *cam, GLdouble mx, GLdouble my);
GLboolean   glgdCamUpdate(glgdCam *cam, glgdCamMode mode,
                GLdouble mx, GLdouble my, GLdouble frameTime);
GLboolean   glgdCamEnd(glgdCam *cam);

GLboolean   glgdCamWinDimSet(glgdCam *cam, GLdouble w, GLdouble h);
GLboolean   glgdCamPosSet(glgdCam *cam, GLdouble x, GLdouble y, GLdouble z);
GLboolean   glgdCamRotSet(glgdCam *cam, glgdQuat qrot);
GLboolean   glgdCamMouseSet(glgdCam *cam, GLdouble mx, GLdouble my);
GLboolean   glgdCamFrameWidth(glgdCam *cam,
                              GLdouble left, GLdouble right,
                              GLdouble bottom, GLdouble top);
GLboolean   glgdCamFrameHeight(glgdCam *cam,
                               GLdouble left, GLdouble right,
                               GLdouble bottom, GLdouble top);
GLboolean   glgdCamFrame(glgdCam *cam,
                         GLdouble left, GLdouble right,
                         GLdouble bottom, GLdouble top);
GLboolean   glgdCamPerspective(glgdCam *cam,
                               GLdouble fovy, GLdouble aspect,
                               GLdouble zNear, GLdouble zFar);
GLboolean   glgdCamFrustum(glgdCam *cam,
                           GLdouble left, GLdouble right,
                           GLdouble bottom, GLdouble top,
                           GLdouble zNear, GLdouble zFar);
GLboolean   glgdCamOrtho(glgdCam *cam,
                         GLdouble left, GLdouble right,
                         GLdouble bottom, GLdouble top,
                         GLdouble zNear, GLdouble zFar);

/*
 * Camera control functions
 */
GLboolean   glgdCamCtrlMaya(glgdCam *cam);
GLboolean   glgdCamCtrl3DSMax(glgdCam *cam);

SCM_DECL_END

#endif  /* __GLGDCAM_H__ */
