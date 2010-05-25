/*
 * glgdQuat.h
 *
 * OpenGL Graph Display quaternion utility module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDQUAT_H__
#define __GLGDQUAT_H__

SCM_DECL_BEGIN

/*
 * Type Definitions
 */
typedef GLdouble        glgdQuat[4];

/*
 * Module API
 */
GLboolean   glgdQuatIdentity(glgdQuat q);
GLboolean   glgdQuatSet(glgdQuat dst, glgdQuat src);
GLboolean   glgdQuatSetByList(glgdQuat dst,
                              GLdouble x, GLdouble y, GLdouble z, GLdouble w);
GLboolean   glgdQuatSetByEuler(glgdQuat q,
                               GLdouble xRad, GLdouble yRad, GLdouble zRad);
GLboolean   glgdQuatSetByXRotation(glgdQuat q, GLdouble xRad);
GLboolean   glgdQuatSetByYRotation(glgdQuat q, GLdouble yRad);
GLboolean   glgdQuatSetByZRotation(glgdQuat q, GLdouble zRad);
GLboolean   glgdQuatAdd(glgdQuat dst, glgdQuat qa, glgdQuat qb);
GLboolean   glgdQuatSub(glgdQuat dst, glgdQuat qa, glgdQuat qb);
GLboolean   glgdQuatMult(glgdQuat dst, glgdQuat qa, glgdQuat qb);
GLboolean   glgdQuatLog(glgdQuat dst, glgdQuat src);
GLboolean   glgdQuatExp(glgdQuat dst, glgdQuat src);
GLboolean   glgdQuatConjugate(glgdQuat dst, glgdQuat src);
GLboolean   glgdQuatInverse(glgdQuat dst, glgdQuat src);
GLboolean   glgdQuatSlerp(glgdQuat dst, glgdQuat qa, glgdQuat qb, GLdouble t);

GLdouble    glgdQuatDot(glgdQuat qa, glgdQuat qb);

SCM_DECL_END

#endif  /* __GLGDQUAT_H__ */
