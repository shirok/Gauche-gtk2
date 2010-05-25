/*
 * gldgTexture.c
 *
 * OpenGL Graph Display texture utility module implementation
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
 * External public functions
 */
glgdTexture
*glgdTextureCreate(void)
{
    glgdTexture *tex;
    
    tex = (glgdTexture *)GLGD_MALLOC(sizeof(glgdTexture));
    if (tex)
    {
        glgdTextureInit(tex);
    }
    
    return tex;
}

glgdTexture
*glgdTextureDestroy(glgdTexture *tex)
{
    if (tex != NULL)
    {
        glgdTextureFini(tex);
        GLGD_FREE(tex);
    }
        
    return (glgdTexture *)NULL;
}

GLboolean
glgdTextureInit(glgdTexture *tex)
{
    if (tex != NULL)
    {
        tex->name = 0;
        tex->width = 0;
        tex->height = 0;
        tex->texels = NULL;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdTextureFini(glgdTexture *tex)
{
    if (tex != NULL)
    {
        glDeleteTextures(1, &tex->name);
        if (tex->texels != NULL)
        {
            GLGD_FREE(tex->texels);
        }
        glgdTextureInit(tex);
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}

GLboolean
glgdTextureSetup(glgdTexture *tex, int width, int height)
{
    GLint   widthSet;
    GLint   heightSet;
    GLvoid  *texels;

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, &widthSet);
    glgdTrace(1, "GL_MAX_TEXTURE_SIZE = %d\n", widthSet);

    /* Create the texture */
    if (tex)
    {
        glTexImage2D(GL_PROXY_TEXTURE_2D, 0, GL_RGBA,
            width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL);

        /* Check for valid (width,height) parameters */
        glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0,
                GL_TEXTURE_WIDTH, &widthSet);
        glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0,
                GL_TEXTURE_HEIGHT, &heightSet);
        if (widthSet == 0 || heightSet == 0)
        {
            return GL_FALSE;
        }

        texels = GLGD_MALLOC(width * height * 4);
        memset(texels, 0, width * height * 4);

        glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

        glGenTextures(1, &tex->name);
        glBindTexture(GL_TEXTURE_2D, tex->name);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,
            width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, texels);

        tex->width = width;
        tex->height = height;
        tex->texels = texels;
        
        return GL_TRUE;
    }
    
    return GL_FALSE;
}
