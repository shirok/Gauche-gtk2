/*
 * gldgMatrix.h
 *
 * OpenGL Graph Display matrix utility module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDMATRIX_H__
#define __GLGDMATRIX_H__

SCM_DECL_BEGIN

/*
 * Type Definitions
 */
typedef GLdouble        glgdMatrix[16];

/*
 * Module API
 */
GLboolean   glgdMatrixIdentity(glgdMatrix mtx);
GLboolean   glgdMatrixDump(glgdMatrix mtx, int indent);
GLboolean   glgdMatrixSetByQuat(glgdMatrix mtx, glgdQuat q);
GLboolean   glgdMatrixPerspective(glgdMatrix mtx,
                                  GLdouble fovy, GLdouble aspect,
                                  GLdouble zNear, GLdouble zFar);
GLboolean   glgdMatrixFrustum(glgdMatrix mtx,
                              GLdouble left, GLdouble right,
                              GLdouble bottom, GLdouble top,
                              GLdouble zNear, GLdouble zFar);
GLboolean   glgdMatrixOrtho(glgdMatrix mtx,
                            GLdouble left, GLdouble right,
                            GLdouble bottom, GLdouble top,
                            GLdouble zNear, GLdouble zFar);

SCM_DECL_END

#endif  /* __GLGDMATRIX_H__ */
