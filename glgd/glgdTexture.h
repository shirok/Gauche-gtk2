/*
 * gldgTexture.h
 *
 * OpenGL Graph Display texture utility module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDTEXTURE_H__
#define __GLGDTEXTURE_H__

SCM_DECL_BEGIN

/*
 * Type Definitions
 */
typedef struct _glgdTexture
{
    GLuint      name;
    GLsizei     width;
    GLsizei     height;
    GLvoid      *texels;
} glgdTexture;

/*
 * Module API
 */
glgdTexture *glgdTextureCreate(void);
glgdTexture *glgdTextureDestroy(glgdTexture *tex);
GLboolean   glgdTextureInit(glgdTexture *tex);
GLboolean   glgdTextureFini(glgdTexture *tex);

GLboolean   glgdTextureSetup(glgdTexture *tex, int width, int height);

SCM_DECL_END

#endif  /* __GLGDTEXTURE_H__ */
