/*
 * gldgBitfield.c
 *
 * OpenGL Graph Display bitfield utility module implementation
 *
 * Written by: Shawn Taras
 */
#include <stdlib.h>
#include <math.h>
#include <GL/gl.h>
#include "glgd.h"

/*
 * External public functions
 */
GLboolean
glgdBitfieldInit(glgdBitfield *bits)
{
    if (bits != NULL)
    {
        return glgdBitfieldClear(bits);
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldFini(glgdBitfield *bits)
{
    return glgdBitfieldInit(bits);
}

GLboolean
glgdBitfieldClear(glgdBitfield *bits)
{
    int     i;

    if (bits != NULL)
    {
        for (i=0; i<GLGDBITFIELD_BYTE_COUNT; i++)
        {
            bits->bits[i] = 0x00;
        }

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldSet(glgdBitfield *bits, int bitNdx)
{
    int         byteNdx;
    GLubyte     mask;

    if (bits != NULL && bitNdx >= 0 && bitNdx < GLGDBITFIELD_BIT_COUNT)
    {
        byteNdx = bitNdx / GLGDBITFIELD_BITS_PER_BYTE;
        mask = 0x1 << (bitNdx % GLGDBITFIELD_BITS_PER_BYTE);

        if ((bits->bits[byteNdx] & mask) == 0x00)
        {
            bits->bits[byteNdx] |= mask;

            /* Only return GL_TRUE if it was actually set! */
            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldToggle(glgdBitfield *bits, int bitNdx)
{
    int         byteNdx;
    GLubyte     mask;

    if (bits != NULL && bitNdx >= 0 && bitNdx < GLGDBITFIELD_BIT_COUNT)
    {
        byteNdx = bitNdx / GLGDBITFIELD_BITS_PER_BYTE;
        mask = 0x1 << (bitNdx % GLGDBITFIELD_BITS_PER_BYTE);

        bits->bits[byteNdx] ^= mask;

        return GL_TRUE;
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldReset(glgdBitfield *bits, int bitNdx)
{
    int         byteNdx;
    GLubyte     mask;

    if (bits != NULL && bitNdx >= 0 && bitNdx < GLGDBITFIELD_BIT_COUNT)
    {
        byteNdx = bitNdx / GLGDBITFIELD_BITS_PER_BYTE;
        mask = 0x1 << (bitNdx % GLGDBITFIELD_BITS_PER_BYTE);

        if (bits->bits[byteNdx] & mask)
        {
            bits->bits[byteNdx] &= ~mask;

            /* Only return GL_TRUE if it was actually reset! */
            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldIsSet(glgdBitfield *bits, int bitNdx)
{
    int         byteNdx;
    GLubyte     mask;

    if (bits != NULL && bitNdx >= 0 && bitNdx < GLGDBITFIELD_BIT_COUNT)
    {
        byteNdx = bitNdx / GLGDBITFIELD_BITS_PER_BYTE;
        mask = 0x1 << (bitNdx % GLGDBITFIELD_BITS_PER_BYTE);

        if (bits->bits[byteNdx] & mask)
        {
            return GL_TRUE;
        }
    }

    return GL_FALSE;
}

GLboolean
glgdBitfieldCompare(glgdBitfield *a, glgdBitfield *b)
{
    int     i;

    if (a && b)
    {
        for (i=0; i<GLGDBITFIELD_BYTE_COUNT; i++)
        {
            if (a->bits[i] & b->bits[i])
            {
                /* Something matches! */
                return GL_TRUE;
            }
        }
    }

    return GL_FALSE;
}
