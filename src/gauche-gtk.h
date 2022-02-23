/*
 * gauche-gtk.h - Gauche+Gtk extension
 *
 *  Copyright(C) 2002-2004 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 */

#ifndef GAUCHE_GTK_H
#define GAUCHE_GTK_H

#include <gauche.h>
#include <gauche/extend.h>
#include <gauche/class.h>
#include <gauche/uvector.h>

#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk-pixbuf/gdk-pixdata.h>

/* This should go to Gauche core, but for now ...*/
#ifndef SCM_RETURN2
#define SCM_RETURN2(a, b)  SCM_RETURN(Scm_Values2(a, b))
#endif
#ifndef SCM_RETURN3
#define SCM_RETURN3(a, b, c)  SCM_RETURN(Scm_Values3(a, b, c))
#endif
#ifndef SCM_RETURN4
#define SCM_RETURN4(a, b, c, d)  SCM_RETURN(Scm_Values4(a, b, c, d))
#endif
#ifndef SCM_RETURN5
#define SCM_RETURN5(a, b, c, d, e)  SCM_RETURN(Scm_Values5(a, b, c, d, e))
#endif

SCM_DECL_BEGIN

/*
 * Generally useful macros
 */

#define CONST_GCHAR_PTR(string) \
    ((const gchar*)Scm_GetStringConst(SCM_STRING(string)))
#define CONST_CHAR_PTR(string) \
    ((const char*)Scm_GetStringConst(SCM_STRING(string)))
#define SCM_STRING_OR_NULL_P(obj) \
    (SCM_FALSEP(obj)||SCM_STRINGP(obj))
#define CONST_GCHAR_PTR_NULLABLE(string) \
    (SCM_FALSEP(string)?NULL:CONST_GCHAR_PTR(string))
#define SCM_MAKE_STR_COPYING_SAFELY(char_ptr) \
    ((char_ptr)?SCM_MAKE_STR_COPYING(char_ptr):SCM_FALSE)

/* box returned allocated gchar* string */
#define GCHAR_PTR_BOX(string) Scm_GtkGcharPtrBox(string)
extern ScmObj Scm_GtkGcharPtrBox(gchar *s);

/* This should go to Gauche core, but for now ...*/
#ifndef GAUCHE_MAJOR_VERSION
typedef void (*ScmFinalizerProc)(ScmObj z, void *data);
extern void Scm_RegisterFinalizer(ScmObj z, ScmFinalizerProc finalizer,
                                  void *data);
#endif

/*
 * GObject <-> ScmObj mapping
 */

/* Scheme view of GObject.
   See gauche-gtk.c for discussion on memory management. */
typedef struct ScmGObjectRec {
    SCM_INSTANCE_HEADER;
    GObject *gobject;           /* can be NULL if explicitly unreferenced */
    ScmObj data;                /* emulate g_object_{get|set}_data */
     /* mmc! */
    int ref_count;
    gulong destroy_handler;     /* see `make_gobject' */
} ScmGObject;

SCM_CLASS_DECL(Scm_GObjectClass);
#define SCM_CLASS_GOBJECT    (&Scm_GObjectClass)
#define SCM_GOBJECT(obj)     ((ScmGObject*)(obj))
#define SCM_GOBJECT_P(obj)   Scm_TypeP(obj, SCM_CLASS_GOBJECT)

#define SCM_GOBJECT_OBJECT(obj)  G_OBJECT(Scm_GObjectCheck(SCM_GOBJECT(obj)))

#define SCM_GOBJECT_UNBOX(caster, obj) \
    (SCM_FALSEP(obj)?NULL:caster(Scm_GObjectCheck(SCM_GOBJECT(obj))))
#define SCM_GOBJECT_BOX(obj)  Scm_MakeGObject(obj)

/* Register the association of GType and ScmClass; must be called in init */
extern void      Scm_GtkRegisterClass(GType type, ScmClass *class);

/* mmc: fixme: why do i export it? Isn't it solved by the _after patch?*/
/* extern*/
/* static int gobject_compare(ScmObj x, ScmObj y, int equalp); */


/* GType -> ScmClass; may return NULL */
extern ScmClass *Scm_GtkTypeToScmClass(GType type);

/* ScmClass -> GType; may return G_TYPE_INVALID */
extern GType Scm_ClassToGtkType(ScmClass *k);
extern int   Scm_ClassListToGtkTypeList(ScmObj klasses, GType *g);

/* mmc: */
ScmObj Scm_GList_to_list(GList *list);

extern int SCM_STRING_LIST_P(ScmObj x);
extern gchar** SCM_STRING_LIST(ScmObj x);
extern ScmObj SCM_MAKE_STRING_LIST(gchar** cstring);

/* GValue <-> ScmObj conversion */
extern ScmObj  Scm_UnboxGValue(const GValue *gv);
extern void    Scm_BoxGValue(GValue *gv, ScmObj sv);
extern GValue *Scm_ObjToGValue(ScmObj obj, GValue *gv);
extern GObject *Scm_GObjectCheck(ScmGObject *obj);

extern ScmClass *Scm_GObjectCPL[];
extern ScmObj    Scm_MakeGObject(void *obj);
extern void      Scm_GtkProtect(ScmObj closure);
extern void      Scm_GtkUnprotect(gpointer data);
extern void      Scm_GObjectUnref(ScmGObject *gobj);
extern ScmObj    Scm_GObjectGetData(ScmGObject *gobj, ScmObj key, ScmObj fallback);
extern ScmObj    Scm_GObjectSetData(ScmGObject *gobj, ScmObj key, ScmObj data);
extern ScmObj    Scm_GtkObjectAllocate(ScmClass *klass, ScmObj initargs);


GClosure *Scm_MakeGClosure(ScmProcedure *procedure);
GClosure *Scm_MakeGClosure_mmc(ScmProcedure *procedure, ScmObj name);

extern gboolean  Scm_GtkCallThunk(gpointer data);
extern ScmObj    Scm_GtkApply(ScmObj proc, ScmObj args);

extern void universal_cell_function(GtkTreeViewColumn *col,
                                    GtkCellRenderer   *renderer,
                                    GtkTreeModel      *model,
                                    GtkTreeIter       *iter,
                                    gpointer           user_data);



/*
 * Unix signal handling hook (see the comments in gauche-gtk.c)
 */

extern void Scm_GtkInitUnixSignalHook(void);

/*
 * GTimer
 */

typedef struct ScmGTimerRec {
    SCM_HEADER;
    GTimer *data;
} ScmGTimer;

SCM_CLASS_DECL(Scm_GTimerClass);
#define SCM_CLASS_GTIMER     (&Scm_GTimerClass)
#define SCM_GTIMER_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GTIMER)

#define SCM_GTIMER(obj)      (((ScmGTimer*)(obj))->data)
#define SCM_MAKE_GTIMER(obj) Scm_MakeGTimer(obj)

extern ScmObj Scm_MakeGTimer(GTimer *data);

/*
 * Pango auxiliary structures
 */

typedef struct ScmPangoLayoutIterRec {
    SCM_HEADER;
    PangoLayoutIter *iter;
} ScmPangoLayoutIter;

SCM_CLASS_DECL(Scm_PangoLayoutIterClass);
#define SCM_CLASS_PANGO_LAYOUT_ITER  (&Scm_PangoLayoutIterClass)
#define SCM_PANGO_LAYOUT_ITER_P(obj) SCM_XTYPEP(obj, SCM_CLASS_PANGO_LAYOUT_ITER)

#define SCM_PANGO_LAYOUT_ITER(obj)   (((ScmPangoLayoutIter*)(obj))->iter)
#define SCM_MAKE_PANGO_LAYOUT_ITER(obj) Scm_MakePangoLayoutIter(obj)

extern ScmObj Scm_MakePangoLayoutIter(PangoLayoutIter *iter);

/*
 * GdkAtom <-> ScmGdkAtom
 */

typedef struct ScmGdkAtomRec {
    SCM_HEADER;
    GdkAtom atom;
} ScmGdkAtom;

SCM_CLASS_DECL(Scm_GdkAtomClass);
#define SCM_CLASS_GDK_ATOM    (&Scm_GdkAtomClass)
#define SCM_GDK_ATOM_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_GDK_ATOM)

#define SCM_GDK_ATOM(obj)     (((ScmGdkAtom*)(obj))->atom)
#define SCM_MAKE_GDK_ATOM(obj) Scm_MakeGdkAtom(obj)

extern ScmObj Scm_MakeGdkAtom(GdkAtom atom);

/*
 * GdkEvent is declared as union.
 */

typedef struct ScmGdkEventRec {
    SCM_HEADER;
    GdkEvent *data;             /* memory belongs to Scheme */
} ScmGdkEvent;

SCM_CLASS_DECL(Scm_GdkEventClass);
#define SCM_CLASS_GDK_EVENT     (&Scm_GdkEventClass)
#define SCM_GDK_EVENT_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GDK_EVENT)
#define SCM_GDK_EVENT(obj)      (((ScmGdkEvent*)(obj))->data)
#define SCM_MAKE_GDK_EVENT(obj) Scm_MakeGdkEvent(obj)

extern ScmObj Scm_MakeGdkEvent(GdkEvent *r);

/*
 * GList, GSList <-> Scheme list
 */

extern ScmObj Scm_GoListToList(GList *list);
extern ScmObj Scm_GoSListToList(GSList *list);

extern GList  *Scm_ListToGList(ScmObj list);
extern GSList *Scm_ListToGSList(ScmObj list);

/*
 * String list -> C String array
 */
extern const char **Scm_StringListToStringArray(ScmObj list);

/*
 * property utility
 */
guchar* Scm_GdkPropertyDataFromUVector(ScmObj uvec,
                                       int *format,     /* out */
                                       int *nelements); /* out */

/*
 * Arrays of primitive types
 */

typedef struct ScmGdkPointVectorRec {
    SCM_HEADER;
    int size;
    GdkPoint *elements;
} ScmGdkPointVector;

SCM_CLASS_DECL(Scm_GdkPointVectorClass);
#define SCM_CLASS_GDK_POINT_VECTOR     (&Scm_GdkPointVectorClass)
#define SCM_GDK_POINT_VECTOR_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GDK_POINT_VECTOR)
#define SCM_GDK_POINT_VECTOR(obj)      ((ScmGdkPointVector*)(obj))

extern ScmObj Scm_MakeGdkPointVector(GdkPoint *pts, int npts);

typedef struct ScmGdkSegmentVectorRec {
    SCM_HEADER;
    int size;
    GdkSegment *elements;
} ScmGdkSegmentVector;

SCM_CLASS_DECL(Scm_GdkSegmentVectorClass);
#define SCM_CLASS_GDK_SEGMENT_VECTOR     (&Scm_GdkSegmentVectorClass)
#define SCM_GDK_SEGMENT_VECTOR_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GDK_SEGMENT_VECTOR)
#define SCM_GDK_SEGMENT_VECTOR(obj)      ((ScmGdkSegmentVector*)(obj))

extern ScmObj Scm_MakeGdkSegmentVector(GdkSegment *segs, int nsegs);

typedef struct ScmGdkRectangleVectorRec {
    SCM_HEADER;
    int size;
    GdkRectangle *elements;
} ScmGdkRectangleVector;

SCM_CLASS_DECL(Scm_GdkRectangleVectorClass);
#define SCM_CLASS_GDK_RECTANGLE_VECTOR     (&Scm_GdkRectangleVectorClass)
#define SCM_GDK_RECTANGLE_VECTOR_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GDK_RECTANGLE_VECTOR)
#define SCM_GDK_RECTANGLE_VECTOR(obj)      ((ScmGdkRectangleVector*)(obj))

extern ScmObj Scm_MakeGdkRectangleVector(GdkRectangle *rects, int nrects);

typedef struct ScmGdkColorVectorRec {
    SCM_HEADER;
    int size;
    GdkColor *elements;
} ScmGdkColorVector;

SCM_CLASS_DECL(Scm_GdkColorVectorClass);
#define SCM_CLASS_GDK_COLOR_VECTOR     (&Scm_GdkColorVectorClass)
#define SCM_GDK_COLOR_VECTOR_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GDK_COLOR_VECTOR)
#define SCM_GDK_COLOR_VECTOR(obj)      ((ScmGdkColorVector*)(obj))

extern ScmObj Scm_MakeGdkColorVector(GdkColor *colors, int ncolors);

/*
 * string-list <-> gchar** conversion
 */
extern gint   Scm_GtkStringsToGcharArrays(ScmObj list, gchar ***chars);
extern ScmObj Scm_GtkGcharArraysToStrings(gint count, gchar **chars);

#include "gtk-lib.h"

/*
 * ScmGtkRadioGroup - Scheme representation of the group of GtkRadio*
 */

/*
 * GtkRadioButton and GtkRadioMenuItem uses GSList to group them.
 * The list must be shared among all the group members.
 * We can get the group by gtk_radio_button_get_group etc., but it is
 * inherently a transient structure; when a member is added in front
 * of the list, the pointer to the old list doesn't reflect the entire
 * group (GTk internally updates the pointers of all group members
 * when the members of group change).  Furthermore, the returned
 * GSList may be deallocated anytime when the button pointed by the first
 * element is removed from the group.  Thus it is not appropriate
 * to expose the list directly to Scheme.
 *
 * Scheme-level radio group keeps a pointer to one of the
 * member of the group.  Any radiobutton belongs to at most one
 * group, so logically they are equivalent.  When the button is
 * removed from the group, the Scheme radiobutton group object is
 * updated to point one of the rest members when there are any.
 */
typedef struct ScmGtkRadioGroupRec {
    SCM_HEADER;
    ScmObj radio;               /* GtkRadioButton or GtkRadioMenuItem or #f */
} ScmGtkRadioGroup;

SCM_CLASS_DECL(Scm_GtkRadioGroupClass);
#define SCM_CLASS_GTK_RADIO_GROUP  (&Scm_GtkRadioGroupClass)
#define SCM_GTK_RADIO_GROUP_P(obj) SCM_XTYPEP(obj, SCM_CLASS_GTK_RADIO_GROUP)
#define SCM_GTK_RADIO_GROUP(obj)   ((ScmGtkRadioGroup*)(obj))

extern ScmObj Scm_MakeGtkRadioGroup(GObject *radio);
extern ScmObj Scm_GtkRadioGroupToList(ScmGtkRadioGroup *group);
extern GSList *Scm_GtkRadioGroupGetGroup(ScmObj group);


/* mmc: gunichar: */
#define Scm_gunichar2char(x)  SCM_MAKE_CHAR(Scm_UcsToChar(x))
#define Scm_char2gunichar(x) Scm_CharToUcs(SCM_CHAR_VALUE(x))



/* Generated: */
typedef struct ScmGSignalQueryRec {
    SCM_HEADER;
    GSignalQuery *data;
    ScmString *name;
} ScmGSignalQuery;

SCM_CLASS_DECL(Scm_GSignalQuery_Class);
#define SCM_CLASS_G_SIGNAL_QUERY     (&Scm_GSignalQuery_Class)
#define SCM_G_SIGNAL_QUERY_P(obj)    (Scm_TypeP(obj, SCM_CLASS_G_SIGNAL_QUERY))
#define SCM_G_SIGNAL_QUERY(obj)      ((GSignalQuery*) (((ScmGSignalQuery*) obj)->data))
extern ScmObj Scm_Make_GSignalQuery(GSignalQuery *data);


extern int gtk_trace_references; /* fixme: gauche_gtk_ */
extern void dump_referenced_gobjects();


ScmObj Scm_g_signal_emit(ScmObj destination, int signal_id, int detail, ScmObj params);


void gauche_gdk_pixbuf_format_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);



SCM_DECL_END
#endif /*GAUCHE_GTK_H*/
