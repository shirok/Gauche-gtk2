/*
 * gldgBitfield.h
 *
 * OpenGL Graph Display bitfield utility module header file
 *
 * Written by: Shawn Taras
 */
#ifndef __GLGDBITFIELD_H__
#define __GLGDBITFIELD_H__

SCM_DECL_BEGIN

/*
 * Defines
 */
#define GLGDBITFIELD_BIT_COUNT          (256)
#define GLGDBITFIELD_BITS_PER_BYTE      (8)
#define GLGDBITFIELD_BYTE_COUNT         (GLGDBITFIELD_BIT_COUNT / GLGDBITFIELD_BITS_PER_BYTE)

/*
 * Type Definitions
 */
typedef struct _glgdBitfield
{
    GLubyte     bits[GLGDBITFIELD_BYTE_COUNT];
} glgdBitfield;

/*
 * Module API
 */
GLboolean   glgdBitfieldInit(glgdBitfield *bits);
GLboolean   glgdBitfieldFini(glgdBitfield *bits);
GLboolean   glgdBitfieldClear(glgdBitfield *bits);
GLboolean   glgdBitfieldSet(glgdBitfield *bits, int bitNdx);
GLboolean   glgdBitfieldToggle(glgdBitfield *bits, int bitNdx);
GLboolean   glgdBitfieldReset(glgdBitfield *bits, int bitNdx);
GLboolean   glgdBitfieldIsSet(glgdBitfield *bits, int bitNdx);

GLboolean   glgdBitfieldCompare(glgdBitfield *a, glgdBitfield *b);

SCM_DECL_END

#endif  /* __GLGDBITFIELD_H__ */
