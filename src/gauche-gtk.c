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
 *
 *  $Id: gauche-gtk.c,v 1.52 2007/01/13 01:36:31 maruska Exp $
 */

#include "gauche-gtk.h"
#include <gauche/vm.h>          /* SCM_VM_MAX_VALUES */
/*===============================================================
 * GObject <-> ScmObj mapping
 */

/*
   NB: it is not trivial to keep reference-counting Gtk memory allocation
   and mark-sweep GC happily together.   The naive method (increment Gtk
   refcount when Scheme obtains the pointer to it, and decrement it when
   Scheme objects are garbage-collected) has a problem:

   We need to protect Scheme pointers passed to Gtk object.  Gtk
   has a callback when it releases the passed pointer, so we can
   register the Scheme pointer to a global table to protect from
   being GC'ed, and remove it in the Gtk's callback.  However,
   if the reference consists a cycle, i.e. the Scheme object passed
   to Gtk has a reference to other Scheme objects which eventually
   points back to the Gtk object, then the Gtk object's refcount
   never go down to zero, and whole structure will never be reclaimed.

   This issue has been discussed in gtk mailing list.
   http://mail.gnome.org/archives/gtk-list/1998-April/msg00525.html
   Vollmer Marius told how Guile-gtk handled it, which seems the
   best approximation strategy so far I've seen:
   http://mail.gnome.org/archives/gtk-list/1998-April/msg00596.html

   It may be possible that I hook Boehm GC (by providing user-defined
   mark_proc) to implement Vollmer's strategy, but I haven't understood
   innards of Boehm GC enough yet.

 */

static struct {
    ScmHashTable *protected;    /* Table of Scheme objects that is passed to
                                   GTk, to protect them from GC'ed.
                                   The key is the object itself, and the value
                                   is a # of times the object is protected. */
    ScmInternalMutex protected_mutex;
    ScmHashTable *typemap;      /* Map ScmClass to GType.  It is rarely
                                   used, but needed in some API that handles
                                   meta information (e.g. liststore) */
    ScmInternalMutex typemap_mutex;

    GQuark scmclass_key;        /* A Quark used in the property list of
                                   GType to keep its associated ScmClass. */
    GQuark scmobj_key;          /* A Quark used in the property list of
                                   GObject to point back Scheme object. */
} gtkdata = {
    /* Initialize the first item so that the structure is placed
       in the data area. */
    NULL
};

/*
 * Type mapping
 */

/* In order to 'box' given GObject in a Scheme object, we need to know
 * the Scheme class corresponding to the given GObject type.  GObject
 * type system has a sort of property list (called qdata), so we use
 * it to keep the type's corresponding ScmClass.
 * Scm_GtkRegisterClass establishes the bidirectional link between
 * GType and ScmClass.  It is called from initialization routine.
 */
typedef struct ScmGTypeRec {
    SCM_HEADER;
    GType gtype;
} ScmGType;

/* mmc: this is for debugging. I keep a weak(?) hash of all Gobjects. */
ScmHashTable* referenced_gobjects;

static SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GTypeClass, NULL);

/* mmc: it is fundamental that gtkdata.`typemap_mutex' can be locked more times by the same thread! fixme! */
void Scm_GtkRegisterClass(GType type, ScmClass *klass)
{
    ScmGType *gtype = SCM_NEW(ScmGType);
    SCM_SET_CLASS(gtype, &Scm_GTypeClass);
    gtype->gtype = type;
#if DEBUG
   Scm_Warn("%s: %s\n", __FUNCTION__, g_type_name(type));
#endif
    g_type_set_qdata(type, gtkdata.scmclass_key, (gpointer)klass);
    /* mmc: why do we have to lock it? Are hashes thread-safe? */
    (void)SCM_INTERNAL_MUTEX_LOCK(gtkdata.typemap_mutex);
    Scm_HashTablePut(gtkdata.typemap, SCM_OBJ(klass), SCM_OBJ(gtype));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gtkdata.typemap_mutex);
}

/* mmc: so this doesn't work for  base types ???  */
ScmClass *Scm_GtkTypeToScmClass(GType type)
{
    ScmClass *c = NULL;
    GType t = type;
    /* Gtk API may return an object of private subtype of the published
       type, so if we don't find the corresponding ScmClass, need to
       look for its ancestors. */
    for (;;) {
        c = (ScmClass*)g_type_get_qdata(t, gtkdata.scmclass_key);
        if (c) return c;
        t = g_type_parent(t);
        if (t == 0) {
            const char *name = g_type_name(type);
            Scm_Warn("Unknown GType %x(%s); GObject assumed", type,
                     name? name : "noname");
            return SCM_CLASS_GOBJECT;
        }
    }
    /*NOTREACHED*/
}

GType Scm_ClassToGtkType(ScmClass *klass)
{
    ScmHashEntry *e;
    (void)SCM_INTERNAL_MUTEX_LOCK(gtkdata.typemap_mutex);
    e = Scm_HashTableGet(gtkdata.typemap, SCM_OBJ(klass));
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gtkdata.typemap_mutex);
    if (!e) return G_TYPE_INVALID;
    else return ((ScmGType*)(e->value))->gtype;
}

/* mmc:  types must be allocated */
int Scm_ClassListToGtkTypeList(ScmObj klasses, GType *types)
{
    int len, i = 0;
    ScmObj k = SCM_NIL;
    GType gt;

    if ((len = Scm_Length(klasses)) > 0) {
        ScmObj sp;
        SCM_FOR_EACH(sp, klasses) {
            k = SCM_CAR(sp);
            if (!Scm_TypeP(k, SCM_CLASS_CLASS)) goto noklass;
            gt = Scm_ClassToGtkType(SCM_CLASS(k));
            if (gt == G_TYPE_INVALID) goto notype;
            types[i++] = gt;
        }
        return i;
    } else if (SCM_VECTORP(klasses)) {
        ScmObj *sp = SCM_VECTOR_ELEMENTS(klasses);
        len = SCM_VECTOR_SIZE(klasses);
        for (i=0; i<len; i++) {
            k = *sp++;
            if (!Scm_TypeP(k, SCM_CLASS_CLASS)) goto noklass;
            gt = Scm_ClassToGtkType(SCM_CLASS(k));
            if (gt == G_TYPE_INVALID) goto notype;
            types[i] = gt;
        }
        return i;
    } else {
        Scm_Error("list or vector of <class> expected, but got %S", klasses);
    }
  noklass:
    Scm_Error("<class> required, but got %S", k);
  notype:
    Scm_Error("Class %S doesn't have corresponding Gtk type", k);
    return -1;                  /* dummy */
}

/* pre-registered primitive types */
static struct predef_type {
    ScmClass *scmklass;
    GType gtype;
} predef_types[] = {
    { SCM_CLASS_BOOL,       G_TYPE_BOOLEAN },
    { SCM_CLASS_CHAR,       G_TYPE_CHAR },
    { SCM_CLASS_INTEGER,    G_TYPE_INT },
    { SCM_CLASS_REAL,       G_TYPE_DOUBLE },
    { SCM_CLASS_STRING,     G_TYPE_STRING },
    { NULL }
};


static void typemap_initialize(ScmHashTable *table)
{
   /* mmc: no reverse mapping  from q_data ? */
    struct predef_type *ptype = predef_types;
#if DEBUG
    Scm_Warn("%s: %d\n", __FUNCTION__, sizeof(predef_types)/sizeof(predef_types[0])); /* array_size */
#endif
    for (; ptype->scmklass; ptype++) {
#if 1
       Scm_GtkRegisterClass(ptype->gtype, ptype->scmklass);
#else
        ScmGType *g = SCM_NEW(ScmGType);
        SCM_SET_CLASS(g, &Scm_GTypeClass);
        g->gtype = ptype->gtype;
        Scm_HashTablePut(table, SCM_OBJ(ptype->scmklass), SCM_OBJ(g));
#endif
        /* fixme: This should call  void Scm_GtkRegisterClass(GType type, ScmClass *klass)
         * And Also the events! */
    }
}

/*
 * GObject
 */

/*static void gobject_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)*/

static int
gobject_compare(ScmObj x, ScmObj y, int equalp)
{
#if DEBUG
   Scm_Warn("%s:\n", __FUNCTION__);
   Scm_Warn("%s: %u %u\n", __FUNCTION__, SCM_GOBJECT_OBJECT(x), SCM_GOBJECT_OBJECT(y));
#endif
    if (equalp) {
        return (SCM_GOBJECT_OBJECT(x) == SCM_GOBJECT_OBJECT(y))? 0 : -1;
    } else {
        Scm_Error("can't order GObject %S and %S", x, y);
        return 0;
    }
}

/* signal handler for "destroy"  */
static void gobject_destroy(GtkObject *gobj, void *data)
{
#if DEBUG
   Scm_Warn("%s:\n", __FUNCTION__);
#endif
    ScmGObject *g = (ScmGObject*)data;
    Scm_GObjectUnref(g);
}

/* mmc: manually creating the class  (stubs do it usually) */
ScmClass *Scm_GObjectCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_GObjectClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BASE_CLASS(Scm_GObjectClass, ScmGObject,
                      NULL, gobject_compare, NULL, Scm_GtkObjectAllocate,
                      Scm_GObjectCPL+1); /* mmc: why + 1 ???  Only top !*/

#if 0 /* for now, we rely on explicit deallocation */
static void gobject_finalize(ScmObj obj, void *data)
{
    ScmGObject *g = SCM_GOBJECT(obj);
    g_object_set_qdata(SCM_GOBJECT_OBJECT(g), gtkdata.scmobj_key, NULL);
    g_object_unref(SCM_GOBJECT_OBJECT(g));
}
#endif


void
dump_referenced_gobjects()
{
   ScmHashIter iter;
   Scm_HashIterInit(&iter, SCM_HASH_TABLE_CORE(referenced_gobjects));

   ScmDictEntry* e;
   GObject* go;
   Scm_Warn("%s", __FUNCTION__);
   while (e = Scm_HashIterNext(&iter))
      {
         go = G_OBJECT(SCM_DICT_KEY(e));
         Scm_Warn("\t%s: %d",
                  /* (! klass || !klass->name)?"": Scm_GetString(SCM_STRING(klass->name)), */
                  g_type_name(G_OBJECT_TYPE(go)),
                  go->ref_count);
      }
   Scm_Warn("END %s", __FUNCTION__);
}

int gtk_trace_references = 0;

/* Internal routine to create a Scheme wrapper for gobject. */
static ScmGObject *make_gobject(ScmClass *klass, GObject *gobj)
{
    ScmGObject *g;
    SCM_ASSERT(Scm_SubtypeP(klass, SCM_CLASS_GOBJECT)); /* in gauche-gtk.h */
    g = SCM_ALLOCATE(ScmGObject, klass);
    SCM_SET_CLASS(g, klass);
    g->gobject = gobj;
#if DEBUG
    Scm_Warn("%s: %u\n", __FUNCTION__, gobj);
#endif
    g->data = SCM_NIL;
    Scm_GtkProtect(SCM_OBJ(g));
    g_object_set_qdata_full(gobj, gtkdata.scmobj_key, (gpointer)g,
                            (GDestroyNotify)Scm_GtkUnprotect);
#if 0 /* for now, we rely on explicit deallocation */
    Scm_RegisterFinalizer(SCM_OBJ(g), gobject_finalize, NULL);
#endif
    if (g_type_is_a(G_OBJECT_TYPE(gobj), GTK_TYPE_OBJECT)) {
#if 0
        Scm_Printf(SCM_VM_CURRENT_ERROR_PORT(Scm_VM()),"g_object_ref\n");
#endif
        g_object_ref(gobj);
        /* Take floating reference */
        gtk_object_sink(GTK_OBJECT(gobj));

        /* mmc: g->ref_count = 1; */
        /* mmc:  */
        /* Scm_HashTablePutRaw */
        Scm_HashTablePut(referenced_gobjects, SCM_OBJ(gobj), SCM_OBJ(gobj));
        if (gtk_trace_references)
           Scm_Warn("%s: %s refcount is %d after ref & sink.\n", __FUNCTION__,
                    /* (! klass || !klass->name)?"": Scm_GetString(SCM_STRING(klass->name)), */
                    g_type_name(G_OBJECT_TYPE(gobj)),
                    gobj->ref_count);
        /* g_type_name() */

        /* Drop the reference upon destruction. */
        g->destroy_handler = g_signal_connect_after(GTK_OBJECT(gobj), "destroy", /* _after */
                         (GCallback)gobject_destroy, (void*)g);
    } else g->destroy_handler = 0;

    return g;
}

/* 'Box' a pointer to GObject. */
ScmObj Scm_MakeGObject(void *obj)
{
    ScmClass *klass;
    ScmGObject *g;
    GObject *gobj;

    /* Allow obj == NULL */
    if (obj == NULL) return SCM_FALSE;
    gobj = G_OBJECT(obj);

    /* First, see if this GObject already has corresponding ScmObj */
    g = (ScmGObject*)g_object_get_qdata(gobj, gtkdata.scmobj_key);
    if (g == NULL) {
#if DEBUG
       Scm_Warn("%s: we have to create a new ScmGObject: %u\n", __FUNCTION__, gobj);
#endif
        /* Creates ScmGObject */
        klass = Scm_GtkTypeToScmClass(G_OBJECT_TYPE(gobj));
        g = make_gobject(klass, gobj);
    }
    return SCM_OBJ(g);
}

/* Common allocator of GtkObject.  Should be used only from gtk*.stub. */
ScmObj Scm_GtkObjectAllocate(ScmClass *klass, ScmObj initargs)
{
    ScmClass **k = klass->cpa;
    GType gbase = G_TYPE_INVALID, t;

    /* Find out which GtkObject should be instantiated, and also
       there's no conflicting GtkType in CPL. */
    t = Scm_ClassToGtkType(klass);
    if (t != G_TYPE_INVALID) gbase = t;
    for (; *k; k++) {
        t = Scm_ClassToGtkType(*k);
        if (t != G_TYPE_INVALID) {
            if (gbase == G_TYPE_INVALID) {
                gbase = t;
            } else {
                if (!g_type_is_a(gbase, t)) {
                    const gchar *gn = g_type_name(gbase);
                    const gchar *tn = g_type_name(t);
                    Scm_Error("class precedence list of %S contains conflicting GtkObject types: %s and %s",
                              klass, (gn? gn : "?"), (tn? tn : "?"));
                }
            }
        }
    }
    if (gbase == G_TYPE_INVALID) {
        Scm_Error("can't instantiate object of class %S", klass);
    }
    return SCM_OBJ(make_gobject(klass, g_object_new(gbase, NULL)));
}

/* Protect and unprotect the Scheme object passed to Gtk */
void Scm_GtkProtect(ScmObj data)
{
    ScmHashEntry *e;
    int count;
    (void)SCM_INTERNAL_MUTEX_LOCK(gtkdata.protected_mutex);
    e = Scm_HashTableAdd(gtkdata.protected, data, SCM_MAKE_INT(0));
    count = SCM_INT_VALUE(e->value) + 1;
    e->value = SCM_MAKE_INT(count);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gtkdata.protected_mutex);
}

void Scm_GtkUnprotect(gpointer data)
{
    ScmHashEntry *e;
    int count;

    if (!data) return;
    (void)SCM_INTERNAL_MUTEX_LOCK(gtkdata.protected_mutex);
    e = Scm_HashTableGet(gtkdata.protected, SCM_OBJ(data));
    if (e) {
        count = SCM_INT_VALUE(e->value) - 1;
        if (count == 0) {
            Scm_HashTableDelete(gtkdata.protected, SCM_OBJ(data));
        } else {
            e->value = SCM_MAKE_INT(count);
        }
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(gtkdata.protected_mutex);
}

/* Explicitly unreference GObject.
   This is necessary to break the cyclic reference until the customized
   mark procedure is implemented (see the discussion above).
   Once unreferenced, gobject field becomes NULL, even if the pointed
   GObject has more than one reference.  The idea is that the Scheme
   object here will be garbage-collected soon. */

void Scm_GObjectUnref(ScmGObject *gobj)
{
    if (gobj->destroy_handler && gobj->gobject){
        /* If the signal is being emitted?  */
        /* I might hint it here: */
        g_signal_handler_disconnect(gobj->gobject, gobj->destroy_handler);
        gobj->destroy_handler = 0;
    }

    if (gobj->gobject) {
        GObject *g = gobj->gobject;

        Scm_HashTableDelete(referenced_gobjects, SCM_OBJ(g));
        if (gtk_trace_references)
           Scm_Warn("%s: refcount is %d before our unref.\n", __FUNCTION__, g->ref_count);
        g_object_set_qdata(g, gtkdata.scmobj_key, NULL);
        gobj->gobject = NULL;
        g_object_unref(g);
    }
}

/* Checks if GObject is not unreferenced */
GObject *Scm_GObjectCheck(ScmGObject *gobj)
{
    if (!gobj->gobject) {
        Scm_Error("GObject has been unreferenced from %S", gobj);
    }
    return gobj->gobject;
}

/* Scheme-world GObject data & properties */
ScmObj Scm_GObjectGetData(ScmGObject *gobj, ScmObj key, ScmObj fallback)
{
    ScmObj p = Scm_Assq(key, gobj->data); /* was     ScmObj p = Scm_Assoc(key, gobj->data, SCM_CMP_EQUAL); */
    if (SCM_PAIRP(p)) return SCM_CDR(p);
    if (SCM_UNBOUNDP(fallback)) Scm_Error("GObject %S doesn't have a property for the key %S", gobj, key);
    return fallback;
}

ScmObj Scm_GObjectSetData(ScmGObject *gobj, ScmObj key, ScmObj data)
{
    if (SCM_UNBOUNDP(data)) {
        gobj->data = Scm_AssocDeleteX(key, gobj->data, SCM_CMP_EQ);
    } else {
        ScmObj p = Scm_Assq(key, gobj->data);
        if (SCM_PAIRP(p)) SCM_SET_CDR(p, data);
        else {
            gobj->data = Scm_Acons(key, data, gobj->data);
        }
    }
    return SCM_UNDEFINED;
}

#if 0
static const char *get_key(ScmObj key)
{
    if (SCM_STRINGP(key)) return Scm_GetStringConst(SCM_STRING(key));
    if (SCM_SYMBOLP(key)) return Scm_GetStringConst(SCM_SYMBOL_NAME(key));
    if (SCM_IDENTIFIERP(key)) return Scm_GetStringConst(SCM_SYMBOL_NAME(SCM_IDENTIFIER(key)->name));
    Scm_Error("property key must be a string or a symbol, but got %S", key);
    return "";                  /* dummy */
}
#endif

/*===============================================================
 * Callbacks
 */

/* Issues:
 *
 *  * Error handling and non-local exit in callbacked Scheme program:
 *    The Scheme closure is effectively invoked inside with-error-handler
 *    so that the errors are captured and handled (right now by
 *    Scm_ReportError) before returning to C.
 *    Outbound continuation invocation, that is, the invocation of a
 *    continuation captured below the Gtk loop, is more problematic.
 *    It can happen, for example, if the user builds his/her own error
 *    handling mechanism using call/cc.
 *    If the program 'restarts', that is, re-enters gtk-main again,
 *    it would be a problem.  It is OK if the program just exits.
 *    We won't know which is the case here, so we assume the user
 *    knows what he/she is doing.
 *
 *  * Argument marshalling.   Gtk/Glib callback mechanism provide
 *    complete type information of arguments & return value, so what
 *    I need to do is just map those arguments to Scheme values.
 *    The old Gtk used GtkArg structure for this purpose, but the new
 *    Gtk uses Glib's GValue mechanism.  There are still some callbacks
 *    in Gtk that uses GtkArg, so we need to support both.
 */

/* We wrap callback with error handler.  see gtk.scm for the actual
   definition of %gtk-call-callback.  */
static ScmObj call_callback(ScmObj proc, ScmObj args)
{
    static ScmObj call_callback_proc = SCM_UNDEFINED;
    SCM_BIND_PROC(call_callback_proc, "%gtk-call-callback",
                  Scm_FindModule(SCM_SYMBOL(SCM_INTERN("gtk")), 0));
    return Scm_ApplyRec2(call_callback_proc, SCM_OBJ(proc), args);
}


/* Argument & return value marshalling - GValue version */

ScmObj Scm_UnboxGValue(const GValue *gv)
{
    GType gt = G_VALUE_TYPE(gv);
    switch (G_TYPE_FUNDAMENTAL(gt)) {
    case G_TYPE_CHAR:  return SCM_MAKE_INT((int)g_value_get_char(gv));
    case G_TYPE_UCHAR: return SCM_MAKE_INT((int)g_value_get_uchar(gv));
    case G_TYPE_BOOLEAN: return SCM_MAKE_BOOL(g_value_get_boolean(gv));
    case G_TYPE_INT:   return Scm_MakeInteger(g_value_get_int(gv));
    case G_TYPE_UINT:  return Scm_MakeIntegerU(g_value_get_uint(gv));
    case G_TYPE_LONG:  return Scm_MakeInteger(g_value_get_long(gv));
    case G_TYPE_ULONG: return Scm_MakeInteger(g_value_get_ulong(gv));
    case G_TYPE_FLOAT: return Scm_MakeFlonum((double)g_value_get_float(gv));
    case G_TYPE_DOUBLE:return Scm_MakeFlonum(g_value_get_double(gv));
    case G_TYPE_STRING:
        return SCM_MAKE_STR_COPYING(g_value_get_string(gv));
    case G_TYPE_OBJECT:
        return Scm_MakeGObject(G_OBJECT(g_value_get_object(gv))); /* how many references ? */
    case G_TYPE_POINTER: {
        Scm_Warn("got G_TYPE_POINTER (really a %s)", g_type_name(gt));
        return SCM_UNDEFINED;
    }

       /* enum G_TYPE_ENUM G_VALUE_HOLDS_ENUM
        *
        *gint        g_value_get_enum                (const [823]GValue *value);
        * !! */

    default:
       if (gt == GTK_TYPE_REQUISITION)
          {
             return SCM_MAKE_GTK_REQUISITION((GtkRequisition*)g_value_get_boxed(gv));
          }
       if (gt == GDK_TYPE_RECTANGLE)
          {
             return SCM_MAKE_GDK_RECTANGLE((GdkRectangle*)g_value_get_boxed(gv));
          }
       if (G_VALUE_HOLDS_ENUM(gv))
          return Scm_MakeInteger(g_value_get_enum(gv));

        /* I'm not sure this is a right thing, but for now...*/
        if (gt == GDK_TYPE_EVENT) {
            return Scm_MakeGdkEvent((GdkEvent*)g_value_get_boxed(gv));
        }
        if (g_type_is_a (gt, G_TYPE_FLAGS)) {
            return Scm_MakeInteger(g_value_get_flags(gv));
        }
        if (gt == gtk_tree_path_get_type()) {
            return SCM_MAKE_GTK_TREE_PATH((GtkTreePath*)g_value_get_boxed(gv));
        }

        /* GtkSelectionData */

        Scm_Warn("cannot convert a GValue of type %s to a Scheme object (%d)",
                 g_type_name(gt), G_TYPE_FUNDAMENTAL(gt));
        return SCM_UNDEFINED;
    }
}

/* mmc:  gvalue  determines the type. scheme value  -> gv   ?? */
void Scm_BoxGValue(GValue *gv, ScmObj sv)
{
    GType gt = G_VALUE_TYPE(gv);
    switch (G_TYPE_FUNDAMENTAL(gt)) {
    case G_TYPE_INVALID:
        /* this happens in some callbacks.   I assume the receiver doesn't
           need the return value. */
        return;
    case G_TYPE_CHAR: {
        int v;
        if (SCM_INTP(sv)) v = SCM_INT_VALUE(sv);
        else if (SCM_CHARP(sv)) v = SCM_CHAR_VALUE(sv);
        else goto err;
        if (v < -128 || v > 127) goto err;
        g_value_set_char(gv, (gchar)v);
        return;
    }
    case G_TYPE_UCHAR: {
        int v;
        if (SCM_INTP(sv)) v = SCM_INT_VALUE(sv);
        else if (SCM_CHARP(sv)) v = SCM_CHAR_VALUE(sv);
        else goto err;
        if (v < 0 || v > 255) goto err;
        g_value_set_uchar(gv, (guchar)v);
        return;
    }
    case G_TYPE_BOOLEAN: {
        g_value_set_boolean(gv, SCM_BOOL_VALUE(sv));
        return;
    }
    case G_TYPE_INT: {
        if (!SCM_EXACTP(sv)) goto err;
        g_value_set_int(gv, Scm_GetInteger(sv));
        return;
    }
    case G_TYPE_UINT: {
        if (!SCM_EXACTP(sv)) goto err;
        g_value_set_uint(gv, Scm_GetIntegerU(sv));
        return;
    }
    case G_TYPE_LONG: {
        if (!SCM_EXACTP(sv)) goto err;
        g_value_set_long(gv, Scm_GetInteger(sv));
        return;
    }
    case G_TYPE_ULONG: {
        if (!SCM_EXACTP(sv)) goto err;
        g_value_set_ulong(gv, Scm_GetIntegerU(sv));
        return;
    }
    case G_TYPE_FLOAT: {
        if (!SCM_REALP(sv)) goto err;
        g_value_set_float(gv, (gfloat)Scm_GetDouble(sv));
        return;
    }
    case G_TYPE_DOUBLE: {
        if (!SCM_REALP(sv)) goto err;
        g_value_set_double(gv, Scm_GetDouble(sv));
        return;
    }
    case G_TYPE_STRING: {
        if (!SCM_STRINGP(sv)) goto err;
        g_value_set_string(gv, Scm_GetStringConst(SCM_STRING(sv)));
        return;
    }
    case G_TYPE_OBJECT: {
        if (!Scm_TypeP(sv, SCM_CLASS_GOBJECT)) goto err;
        g_value_set_object(gv, SCM_GOBJECT_OBJECT(sv));
        return;
    }
    default:
            if (g_type_is_a(gt, G_TYPE_ENUM)) {
                  g_value_set_enum(gv, Scm_GetInteger(sv));
                  return;
            }

      err:
        Scm_Error("cannot convert a Scheme object %S to a GValue of type %s",
                  sv, g_type_name(gt));
    }
}

/* Like BoxGValue, except the type is determined by Scheme value.
   GValue structure is initialized by this. */
GValue *Scm_ObjToGValue(ScmObj obj, GValue *gv)
{
    gv->g_type = 0;
    if (SCM_INTP(obj)) {
        g_value_init(gv, G_TYPE_INT);
        g_value_set_int(gv, Scm_GetInteger(obj));
        return gv;
    }
    if (SCM_BIGNUMP(obj)) {
        /*NB: check the value range */
        g_value_init(gv, G_TYPE_INT);
        g_value_set_int(gv, Scm_GetInteger(obj));
        return gv;
    }
    if (SCM_STRINGP(obj)) {
        g_value_init(gv, G_TYPE_STRING);
        g_value_set_string(gv, Scm_GetStringConst(SCM_STRING(obj)));
        return gv;
    }
    if (SCM_SYMBOLP(obj)) {
        g_value_init(gv, G_TYPE_STRING);
        g_value_set_string(gv, Scm_GetStringConst(SCM_SYMBOL_NAME(obj)));
        return gv;
    }
    if (SCM_BOOLP(obj)) {
        g_value_init(gv, G_TYPE_BOOLEAN);
        g_value_set_boolean(gv, SCM_BOOL_VALUE(obj));
        return gv;
    }
    if (SCM_FLONUMP(obj)) {
        g_value_init(gv, G_TYPE_DOUBLE);
        g_value_set_double(gv, Scm_GetDouble(obj));
        return gv;
    }
    if (SCM_GOBJECT_P(obj)) {
        GType gt = Scm_ClassToGtkType(SCM_CLASS_OF(obj));
        if (gt != G_TYPE_INVALID) {
            g_value_init(gv, gt);
            g_value_set_object(gv, SCM_GOBJECT_OBJECT(obj));
            return gv;
        }
    }
    Scm_Error("can't convert Scheme value %S to GValue", obj);
    return NULL;
}

/* mmc: i need a function, not side-effecting procedure.  that's for define-ctype   (genstub's instruction)*/

/* inline ? */
GValue *Scm_obj_to_gvalue (ScmObj obj)
{
   GValue* g = malloc (sizeof(GValue));
   return Scm_ObjToGValue (obj, g);
}

/*
 * GClosure interface
 */
typedef struct {
    GClosure closure;
    ScmProcedure *proc;
   int  gpointers;                /* number of gpointer GValues hinted */
   char* gpointer_mapping;      /* the hints: every char encodes a type. */
} SClosure;


void Scm_mmc_GClosureMarshal(GClosure *closure, GValue *retval,
                             guint nparams, const GValue *params,
                             gpointer ihint, gpointer data)
{
    ScmObj argh = SCM_NIL, argt = SCM_NIL, ret;
    ScmProcedure *proc = ((SClosure*)closure)->proc;
    int i;

    Scm_Warn("%s:", __FUNCTION__);
    /* max given by gauche VM ! */
    /* Maximum # of values allowed for multiple value return */
    int indexes[SCM_VM_MAX_VALUES]= {0}; /* bzero ?*/
    int index = 0;



    int n = ((SClosure*)closure)->gpointers;
    char* types = ((SClosure*)closure)->gpointer_mapping;


    SCM_ASSERT(proc && SCM_PROCEDUREP(proc));

    /* Scm_Warn("%s: looking for GPOINTERS %s",  __FUNCTION__, types); */

    for (i=0; i<nparams; i++) {

       GType gt = G_VALUE_TYPE(params+i);
       if (G_TYPE_FUNDAMENTAL(gt) == G_TYPE_POINTER) {
          if (n-- > 0)
             {
                indexes[index++] = i;
                switch (*types){
                case 'i':
                   SCM_APPEND1(argh, argt,
                               Scm_MakeInteger(* ((gint*)g_value_get_pointer(params+i))));
                   break;
                default:
                   Scm_Warn("unknown type %c", *types);
                };
                types++;
             } else
                {
                   Scm_Warn("dunno about any more gpointers!!");
                };
       } else {
          /* params */
          Scm_Warn("%s:\n", __FUNCTION__);
          SCM_APPEND1(argh, argt, Scm_UnboxGValue(params+i));
    }

    }

    ret = call_callback(SCM_OBJ(proc), argh);

#if 1
    ScmVM* vm = Scm_VM();
    ScmObj values = Scm_VMGetResult(vm);
    /* list of values. now we have to walk the argument list once again (we could have a list of indexes), and push the values to the Gpointers... */
#undef debug
#define debug     1
#if debug
    Scm_Warn("result has %d values. and we have %d", Scm_Length(values), index);
#endif
    if (Scm_Length(values) > 1)
       {
          /* we have to fill-in back the arguments....  the values pointed to by the arguments/ gpointers */
          char* types = ((SClosure*)closure)->gpointer_mapping;
          ScmObj p;
          for (i = 0, p = SCM_CDR(values); (i < index) && SCM_PAIRP(p); i++, (p) = SCM_CDR(p)){
             GType gt = G_VALUE_TYPE(params + indexes[i]);
             if (G_TYPE_FUNDAMENTAL(gt) == G_TYPE_POINTER) {
                switch (types[i]){
                case 'i': {

#if debug
                   /* SCM_INT_VALUE(SCM_CAR(p)) */
                   int value = Scm_GetInteger(SCM_CAR(p));
                   /* value = 0; */
                   Scm_Warn("exporting integer value %d to: %d",value, indexes[i]);
#endif
                   (* ((gint*)g_value_get_pointer(params+indexes[i]))) = value;
                   break;
                }
                default:
                   Scm_Warn("unknown type %c skipping", types[i]);
                };
             } else
                Scm_Warn("%s: index %d %d  is no more a gtype, bug!", __FUNCTION__, i, indexes[i]);
          };
       }
#if debug
    Scm_Warn("returning");
#endif
#else
    gint* pointer = (g_value_get_pointer(params+3));
    Scm_Warn("not changing %d", *pointer);
    if (*pointer > 0)
       *pointer -= 1;
    Scm_Warn("not changing %d", *pointer);
#endif

    if (retval) Scm_BoxGValue(retval, ret);
    Scm_Warn("%s: Ending", __FUNCTION__);
}


void Scm_GClosureMarshal(GClosure *closure, GValue *retval,
                         guint nparams, const GValue *params,
                         gpointer ihint, gpointer data)
{
    ScmObj argh = SCM_NIL, argt = SCM_NIL, ret;
    ScmProcedure *proc = ((SClosure*)closure)->proc;
    int i;

    SCM_ASSERT(proc && SCM_PROCEDUREP(proc));
    for (i=0; i<nparams; i++) {
       GType gt = G_VALUE_TYPE(params +i);
#if DEBUG
       if (G_TYPE_FUNDAMENTAL(gt) == G_TYPE_OBJECT) {
          Scm_Warn("%s: g_type_object %u %s\n", __FUNCTION__, g_value_get_object(params+i),
                   (GTK_IS_WINDOW(g_value_get_object(params+i))?"window":"")
                   );
       }
#endif
        SCM_APPEND1(argh, argt, Scm_UnboxGValue(params+i));
    }

    ret = call_callback(SCM_OBJ(proc), argh);

    if (retval) Scm_BoxGValue(retval, ret);
}

void Scm_GClosureDestroy(gpointer data, GClosure *closure)
{
    Scm_GtkUnprotect(data);
}

/* the closure is used just 1 time, so on `destroy' -> unprotect it(remove from the hash) */
GClosure *Scm_MakeGClosure(ScmProcedure *procedure)
{
    GClosure *c = g_closure_new_simple(sizeof(SClosure), NULL);
    ((SClosure*)c)->proc = procedure;
    Scm_GtkProtect(SCM_OBJ(procedure)); /*  hash ->  */
    g_closure_add_finalize_notifier(c, (gpointer)procedure,
                                    Scm_GClosureDestroy);
    g_closure_set_marshal(c, Scm_GClosureMarshal); /* this, overrides the ?? */
    return c;
}


void
universal_cell_function (GtkTreeViewColumn *col,
                         GtkCellRenderer   *renderer,
                         GtkTreeModel      *model,
                         GtkTreeIter       *iter,
                         gpointer           user_data)
{
#if 0
    /* type check! */
    assert();
#endif
    /* i could keep a signature (C pointers to the column &...), and don't bother the scheme part w/ these args! */
#if 0
    Scm_Warn("%s:", __FUNCTION__);
#endif
    SClosure *closure = (SClosure*) user_data;

    ScmObj scm_col = Scm_MakeGObject(col); /* fixme: i could create it in the closure! */

    ScmObj scm_renderer = Scm_MakeGObject(renderer);
    ScmObj scm_model = Scm_MakeGObject(model);
    ScmObj scm_iter = Scm_MakeGtkTreeIter(iter); /*Scm_MakeGObject(iter); */

    GValue gval = {0};
    gtk_tree_model_get_value(model, iter, 0, &gval);

    int number = 0;

    GType gt = G_VALUE_TYPE(&gval);
    if (G_TYPE_FUNDAMENTAL(gt) == G_TYPE_INT)
        number =  g_value_get_int(&gval);
    g_value_unset(&gval);
    /* Scm_Warn ("%s: %d\n", __FUNCTION__, number); */


    /* ScmProcedure *proc = */
    /* (run_closure (); */

    ScmProcedure *proc = ((SClosure*)closure)->proc;

    Scm_ApplyRec4(SCM_OBJ(proc), scm_col, scm_renderer, scm_iter, scm_model);
#if 0
    age_cell_data_function
        {
            gfloat  age;
            gchar   buf[20];
            gtk_tree_model_get(model, iter, COLUMN_AGE_FLOAT, &age, -1);
            g_snprintf(buf, sizeof(buf), "%.1f", age);
            g_object_set(renderer, "text", buf, NULL);
        }
#endif
#if 0
    Scm_Warn("%s: END", __FUNCTION__);
#endif
}


#define mmc_debug  0
GClosure *Scm_MakeGClosure_mmc(ScmProcedure *procedure,ScmObj name) /* ScmString* ScmString*/
{
    GClosure *c = g_closure_new_simple(sizeof(SClosure), NULL);
    ((SClosure*)c)->proc = procedure;
    Scm_GtkProtect(SCM_OBJ(procedure)); /*  hash ->  */
    g_closure_add_finalize_notifier(c, (gpointer)procedure,
                                    Scm_GClosureDestroy);


    ScmModule *module = SCM_MODULE(SCM_FIND_MODULE("gtk", TRUE)); /* SCM_OBJ(SCM_CURRENT_MODULE()) */
    ScmSymbol *symbol = SCM_SYMBOL(SCM_INTERN("gpointer-mapping")); /* Scm_Intern((ScmString*) SCM_MAKE_STR("pg-handle-hook")) */

#if mmc_debug
    if (symbol)
       Scm_Warn("%s: found the symbol",  __FUNCTION__);
#endif

    ScmObj mapping = Scm_SymbolValue(module, symbol);

#if mmc_debug
    if (mapping && (SCM_HASHTABLEP(mapping)))
       Scm_Warn("%s: found the value, too, Now searching for %s",  __FUNCTION__, Scm_GetStringConst(SCM_STRING(name)));
#endif



    ScmHashEntry* e = Scm_HashTableGet(SCM_HASHTABLE(mapping), name);

    if (mapping && (SCM_HASHTABLEP(mapping)) &&
        /* find in the hash: */
        (e))
       {

          /* Scm_Warn("%s: found a hashtable",  __FUNCTION__); */

          if (SCM_PAIRP(e->value)
              && SCM_INTEGERP(SCM_CAR(e->value))
              && SCM_STRINGP(SCM_CDR(e->value)))
             {
                /* Scm_Warn("%s: found an entry in the hashtable",  __FUNCTION__); */
                /* must be a cons/pair   name ->  (number . vector-or-string) */
                ((SClosure*)c)->gpointers = Scm_GetInteger(SCM_CAR(e->value));
                /* fixme:  check the string lenght !!! */
                ((SClosure*)c)->gpointer_mapping = Scm_GetString(SCM_STRING(SCM_CDR(e->value)));

                g_closure_set_marshal(c, Scm_mmc_GClosureMarshal); /* this, overrides the ?? */
                goto end;
             }
       };
/*  old: */
    ((SClosure*)c)->gpointers = 0;
    g_closure_set_marshal(c, Scm_GClosureMarshal); /* this, overrides the ?? */

  end:
    return c;
}

/* This can be passed to gtk_idle_add etc. */
gboolean Scm_GtkCallThunk(gpointer closure)
{
    SCM_ASSERT(closure != NULL && SCM_PROCEDUREP(closure));
    ScmObj ret = call_callback(SCM_OBJ(closure), SCM_NIL);
    return SCM_BOOL_VALUE(ret);
}

/* More general version.  Returns a list of values. */
ScmObj Scm_GtkApply(ScmObj proc, ScmObj args)
{
    call_callback(proc, args);
    return Scm_VMGetResult(Scm_VM());
}

/*===============================================================
 * Unix signal handling
 */

/* After gtk-main-loop, Gtk takes over the control of the
 * application.  When an unix signal arrives, it is queued
 * in the Gauche VM signal queue and also it terminates the
 * poll() function inside Gtk main loop.  However, Gtk knows
 * nothing about Gauche VM, so it re-invokes poll() if no other
 * event occurs---thus Gauche's signal handler will never be
 * called.
 *
 * Gtk doesn't provide a direct way to address this (idle
 * handler can't be used, for it would be a busy wait for the
 * unix signals).  However, the underlying g_main_loop mechanism
 * that Gtk main loop uses has very flexible way to hook our
 * function inside the main loop.
 */

static gboolean scm_signal_prepare(GSource *source, gint *timeout)
{
    *timeout = -1;
    return FALSE;
}

static gboolean scm_signal_check(GSource *source)
{
    ScmVM *vm = Scm_VM();
    return vm->signalPending;
}

static gboolean scm_signal_dispatch(GSource *source,
                                    GSourceFunc callback,
                                    gpointer user_data)
{
    Scm_SigCheck(Scm_VM());
    return TRUE;
}

GSourceFuncs scm_signal_watch_funcs = {
    scm_signal_prepare,
    scm_signal_check,
    scm_signal_dispatch,
    NULL
};

static gboolean scm_signal_watcher_add(gpointer data)
{
    GSource *source = g_source_new(&scm_signal_watch_funcs,
                                   sizeof(GSource));
    /* attach to the default context, which Gtk seems to use. */
    g_source_attach(source, NULL);
    return TRUE;
}

void Scm_GtkInitUnixSignalHook(void)
{
    gtk_init_add(scm_signal_watcher_add, NULL);
}

/*===============================================================
 * GTimer
 */

static void g_timer_finalize(ScmObj obj, void *data)
{
    ScmGTimer *g = (ScmGTimer*)obj;
    g_timer_destroy(g->data);
    g->data = NULL;
}

ScmObj Scm_MakeGTimer(GTimer *r)
{
    ScmGTimer *g = SCM_NEW(ScmGTimer);
    SCM_SET_CLASS(g, SCM_CLASS_GTIMER);
    g->data = r;
    Scm_RegisterFinalizer(SCM_OBJ(g), g_timer_finalize, NULL);
    return SCM_OBJ(g);
}

/*===============================================================
 * Pango auxiliary structures
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_PangoLayoutIterClass, NULL);

static void pango_layout_iter_finalize(ScmObj obj, void *data)
{
    ScmPangoLayoutIter *g = (ScmPangoLayoutIter*)obj;
    pango_layout_iter_free(g->iter);
    g->iter = NULL;
}

ScmObj Scm_MakePangoLayoutIter(PangoLayoutIter *r)
{
    ScmPangoLayoutIter *g = SCM_NEW(ScmPangoLayoutIter);
    SCM_SET_CLASS(g, SCM_CLASS_PANGO_LAYOUT_ITER);
    g->iter = r;
    Scm_RegisterFinalizer(SCM_OBJ(g), pango_layout_iter_finalize, NULL);
    return SCM_OBJ(g);
}

/*===============================================================
 * GdkAtom <-> ScmObj mapping
 */

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkAtomClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

ScmObj Scm_MakeGdkAtom(GdkAtom atom)
{
    ScmGdkAtom *z = SCM_NEW(ScmGdkAtom);
    SCM_SET_CLASS(z, SCM_CLASS_GDK_ATOM);
    z->atom = atom;             /* no refcounting needed */
    return SCM_OBJ(z);
}

/*===============================================================
 * gchar** <-> string list mapping
 */

gint Scm_GtkStringsToGcharArrays(ScmObj list, gchar ***chars)
{
    int len = Scm_Length(list), i = 0;
    ScmObj cp;
    gchar **s = SCM_NEW2(gchar**, sizeof(gchar*)*len);
    SCM_FOR_EACH(cp, list) {
        if (!SCM_STRINGP(SCM_CAR(cp))) {
            Scm_Error("string requried, but got %S", SCM_CAR(cp));
        }
        s[i++] = (gchar*)Scm_GetString(SCM_STRING(SCM_CAR(cp)));
    }
    *chars = s;
    return len;
}

ScmObj Scm_GtkGcharArraysToStrings(gint count, gchar **chars)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    int i;
    for (i=0; i<count; i++) {
        SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(chars[i]));
    }
    return h;
}

/* Box returned allocated gchar*.  We need to copy it to own the memory. */
ScmObj Scm_GtkGcharPtrBox(gchar *string)
{
    ScmObj s = SCM_MAKE_STR_COPYING(string);
    g_free(string);
    return s;
}

/*===============================================================
 * uvector <-> property data (used by gtk-property stuff)
 */

/* gtk-property-change accepts u8, u16, and u32vector as the
   property data.  u8 and u16 vectors are directly mapped to
   guchar* and gushort*.  We need a special care to map u32vector
   to gulong*. */
guchar* Scm_GdkPropertyDataFromUVector(ScmObj uvec,
                                       int *format,    /* out */
                                       int *nelements) /* out */
{
    if (SCM_U8VECTORP(uvec)) {
        *nelements = SCM_U8VECTOR_SIZE(uvec);
        *format = 8;
        return (guchar*)SCM_U8VECTOR_ELEMENTS(uvec);
    } else if (SCM_U16VECTORP(uvec)) {
        *nelements = SCM_U16VECTOR_SIZE(uvec);
        *format = 16;
        return (guchar*)SCM_U16VECTOR_ELEMENTS(uvec);
    } else if (SCM_U32VECTORP(uvec)) {
        *nelements = SCM_U32VECTOR_SIZE(uvec);
        *format = 32;
#if SIZEOF_LONG == 4
        return (guchar*)SCM_U32VECTOR_ELEMENTS(uvec);
#else
        {
            gulong *buf = SCM_NEW_ATOMIC2(gulong*,
                                          (*nelements)*sizeof(gulong));
            int i;
            for (i=0; i<*nelements; i++) {
                buf[i] = (gulong)(SCM_U32VECTOR_ELEMENTS(uvec)[i]);
            }
            return (guchar*)buf;
        }
#endif
    } else {
        Scm_Error("property data must be either u8, u16, or u32vector, but got %S", uvec);
        return NULL;            /* dummy */
    }
}


/*===============================================================
 * GdkEvent
 */

extern ScmClass Scm_GdkEventAnyClass;
extern ScmClass Scm_GdkEventExposeClass;
extern ScmClass Scm_GdkEventMotionClass;
extern ScmClass Scm_GdkEventButtonClass;
extern ScmClass Scm_GdkEventKeyClass;
extern ScmClass Scm_GdkEventCrossingClass;
extern ScmClass Scm_GdkEventFocusClass;
extern ScmClass Scm_GdkEventConfigureClass;
extern ScmClass Scm_GdkEventPropertyClass;
extern ScmClass Scm_GdkEventSelectionClass;
extern ScmClass Scm_GdkEventProximityClass;
extern ScmClass Scm_GdkEventDNDClass;
extern ScmClass Scm_GdkEventClientClass;
extern ScmClass Scm_GdkEventVisibilityClass;
extern ScmClass Scm_GdkEventNoExposeClass;
extern ScmClass Scm_GdkEventScrollClass;
extern ScmClass Scm_GdkEventWindowStateClass;
extern ScmClass Scm_GdkEventSettingClass;

/* maps event->type to the class of the event */
static struct EvClassTableRec {
    GdkEventType type;
    ScmClass *klass;
} evClassTable[] = {
    { GDK_DELETE,           &Scm_GdkEventAnyClass },
    { GDK_DESTROY,          &Scm_GdkEventAnyClass },
    { GDK_EXPOSE,           &Scm_GdkEventExposeClass },
    { GDK_MOTION_NOTIFY,    &Scm_GdkEventMotionClass },
    { GDK_BUTTON_PRESS,     &Scm_GdkEventButtonClass },
    { GDK_2BUTTON_PRESS,    &Scm_GdkEventButtonClass },
    { GDK_3BUTTON_PRESS,    &Scm_GdkEventButtonClass },
    { GDK_BUTTON_RELEASE,   &Scm_GdkEventButtonClass },
    { GDK_KEY_PRESS,        &Scm_GdkEventKeyClass },
    { GDK_KEY_RELEASE,      &Scm_GdkEventKeyClass },
    { GDK_ENTER_NOTIFY,     &Scm_GdkEventCrossingClass },
    { GDK_LEAVE_NOTIFY,     &Scm_GdkEventCrossingClass },
    { GDK_FOCUS_CHANGE,     &Scm_GdkEventFocusClass },
    { GDK_CONFIGURE,        &Scm_GdkEventConfigureClass },
    { GDK_MAP,              &Scm_GdkEventAnyClass },
    { GDK_UNMAP,            &Scm_GdkEventAnyClass },
    { GDK_PROPERTY_NOTIFY,  &Scm_GdkEventPropertyClass },
    { GDK_SELECTION_CLEAR,  &Scm_GdkEventSelectionClass },
    { GDK_SELECTION_REQUEST,&Scm_GdkEventSelectionClass },
    { GDK_SELECTION_NOTIFY, &Scm_GdkEventSelectionClass },
    { GDK_PROXIMITY_IN,     &Scm_GdkEventProximityClass },
    { GDK_PROXIMITY_OUT,    &Scm_GdkEventProximityClass },
    { GDK_DRAG_ENTER,       &Scm_GdkEventDNDClass },
    { GDK_DRAG_LEAVE,       &Scm_GdkEventDNDClass },
    { GDK_DRAG_MOTION,      &Scm_GdkEventDNDClass },
    { GDK_DRAG_STATUS,      &Scm_GdkEventDNDClass },
    { GDK_DROP_START,       &Scm_GdkEventDNDClass },
    { GDK_DROP_FINISHED,    &Scm_GdkEventDNDClass },
    { GDK_CLIENT_EVENT,     &Scm_GdkEventClientClass },
    { GDK_VISIBILITY_NOTIFY,&Scm_GdkEventVisibilityClass },
    { GDK_NO_EXPOSE,        &Scm_GdkEventNoExposeClass },
    { GDK_SCROLL,           &Scm_GdkEventScrollClass },
    { GDK_WINDOW_STATE,     &Scm_GdkEventWindowStateClass },
    { GDK_SETTING,          &Scm_GdkEventSettingClass },
    { -1,                   &Scm_GdkEventAnyClass }
};
/* fixme: These are valid GTypes, aren't they? So I should put them into the hash! */

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkEventClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

static void gdk_event_finalize(ScmObj obj, void *data)
{
    ScmGdkEvent *e = (ScmGdkEvent*)obj;
    gdk_event_free(e->data);
    e->data = NULL;
}

ScmObj Scm_MakeGdkEvent(GdkEvent *r)
{
    ScmClass *klass = &Scm_GdkEventAnyClass;
    ScmGdkEvent *g;
    struct EvClassTableRec *ctab;

    for (ctab = evClassTable; ctab->type >= 0; ctab++) {
        if (((GdkEventAny*)r)->type == ctab->type) {
            klass = ctab->klass;
            break;
        }
    }
    g = SCM_NEW(ScmGdkEvent);
    SCM_SET_CLASS(g, klass);
    g->data = gdk_event_copy(r);
    Scm_RegisterFinalizer(SCM_OBJ(g), gdk_event_finalize, NULL);
    return SCM_OBJ(g);
}

/*===============================================================
 * GList & GSList
 */

/* these 2 would be templates, in C++, imho. -- mmc! */
ScmObj Scm_GoListToList(GList *list)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    while (list) {
        if (!list->data) Scm_Error("GList->List: list contains NULL");
        if (!G_IS_OBJECT(list->data)) {
            Scm_Error("GList->List: list contains non-GObject");
        }
        SCM_APPEND1(h, t, SCM_GOBJECT_BOX(list->data));
        list = g_list_next(list);
    }
    return h;
}

/* this is for `Single'-link'ed lists. */
ScmObj Scm_GoSListToList(GSList *list)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    while (list) {
        if (!list->data) Scm_Error("GSList->List: list contains NULL");
        if (!G_IS_OBJECT(list->data)) {
            Scm_Error("GSList->List: list contains non-GObject");
        }
        SCM_APPEND1(h, t, SCM_GOBJECT_BOX(list->data));
        list = g_slist_next(list);
    }
    return h;
}

GList *Scm_ListToGList(ScmObj list)
{
    GList *glist = NULL;
    ScmObj lp;
    SCM_FOR_EACH(lp, list) {
        ScmObj elt = SCM_CAR(lp);
        if (!SCM_GOBJECT_P(elt)) {
            if (glist) g_list_free(glist);
            Scm_Error("List of <g-object> required, but the list contains %S", elt);
        }
        glist = g_list_append(glist, SCM_GOBJECT_OBJECT(elt));
    }
    return glist;
}

GSList *Scm_ListToGSList(ScmObj list)
{
    GSList *glist = NULL;
    ScmObj lp;
    SCM_FOR_EACH(lp, list) {
        ScmObj elt = SCM_CAR(lp);
        if (!SCM_GOBJECT_P(elt)) {
            if (glist) g_slist_free(glist);
            Scm_Error("List of <g-object> required, but the list contains %S", elt);
        }
        glist = g_slist_append(glist, SCM_GOBJECT_OBJECT(elt));
    }
    return glist;
}

/* mmc: unboxing  as a function  ....  Frees the argument*/
ScmObj
Scm_GList_to_list(GList *list)
{
   ScmObj r = Scm_GoListToList(list);
   g_list_free(list);
   SCM_RETURN(r);
};


ScmObj
Scm_const_GList_to_list(GList *list)
{
   ScmObj r = Scm_GoListToList(list);
   /* g_list_free(list); */
   SCM_RETURN(r);
};


/*===============================================================
 * String list and array
 */

const char **Scm_StringListToStringArray(ScmObj list)
{
    int len = Scm_Length(list), i = 0;
    ScmObj lp;
    const char **a;
    if (len < 0) Scm_Error("proper list required, but got %S", list);
    a = SCM_NEW2(const char **, (len+1)*sizeof(char *));
    SCM_FOR_EACH(lp, list) {
        if (!SCM_STRINGP(SCM_CAR(lp)))
            Scm_Error("string required, but got %S", SCM_CAR(lp));
        a[i] = Scm_GetStringConst(SCM_STRING(SCM_CAR(lp)));
        i++;
    }
    a[i] = NULL;
    return a;
}

/*===============================================================
 * Arrays of primitive types
 */

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkPointVectorClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

ScmObj Scm_MakeGdkPointVector(GdkPoint *pts, int npts)
{
    ScmGdkPointVector *r = SCM_NEW(ScmGdkPointVector); /* see .h */
    SCM_SET_CLASS(r, SCM_CLASS_GDK_POINT_VECTOR);
    r->size = npts;
    r->elements = SCM_NEW_ATOMIC2(GdkPoint*, sizeof(GdkPoint[1])*npts);
    if (pts) memcpy(r->elements, pts, sizeof(GdkPoint[1])*npts);
    return SCM_OBJ(r);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkSegmentVectorClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

ScmObj Scm_MakeGdkSegmentVector(GdkSegment *segs, int nsegs)
{
    ScmGdkSegmentVector *r = SCM_NEW(ScmGdkSegmentVector);
    SCM_SET_CLASS(r, SCM_CLASS_GDK_SEGMENT_VECTOR);
    r->size = nsegs;
    r->elements = SCM_NEW_ATOMIC2(GdkSegment*, sizeof(GdkSegment[1])*nsegs);
    if (segs) memcpy(r->elements, segs, sizeof(GdkSegment[1])*nsegs);
    return SCM_OBJ(r);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkRectangleVectorClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

ScmObj Scm_MakeGdkRectangleVector(GdkRectangle *rects, int nrects)
{
    ScmGdkRectangleVector *r = SCM_NEW(ScmGdkRectangleVector);
    SCM_SET_CLASS(r, SCM_CLASS_GDK_RECTANGLE_VECTOR);
    r->size = nrects;
    r->elements = SCM_NEW_ATOMIC2(GdkRectangle*, sizeof(GdkRectangle[1])*nrects);
    if (rects) memcpy(r->elements, rects, sizeof(GdkRectangle[1])*nrects);
    return SCM_OBJ(r);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GdkColorVectorClass,
                         NULL, NULL, NULL, NULL,
                         Scm_GObjectCPL+1);

ScmObj Scm_MakeGdkColorVector(GdkColor *colors, int ncolors)
{
    ScmGdkColorVector *r = SCM_NEW(ScmGdkColorVector);
    SCM_SET_CLASS(r, SCM_CLASS_GDK_COLOR_VECTOR);
    r->size = ncolors;
    r->elements = SCM_NEW_ATOMIC2(GdkColor*, sizeof(GdkColor[1])*ncolors);
    if (colors) memcpy(r->elements, colors, sizeof(GdkColor[1])*ncolors);
    return SCM_OBJ(r);
}

/*===============================================================
 * RadioGroup
 */

/* See the comment of header file about ScmGtkRadioGroup */

static int radio_group_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmObj rx, ry;
    GObject *gx, *gy;

    if (!equalp) Scm_Error("can't order %S and %S", x, y);
    rx = SCM_GTK_RADIO_GROUP(x)->radio;
    ry = SCM_GTK_RADIO_GROUP(x)->radio;
    if (SCM_FALSEP(rx)) {
        return SCM_FALSEP(ry)? 0 : -1;
    }
    SCM_ASSERT(SCM_GOBJECT_P(rx) && SCM_GOBJECT_P(ry));
    gx = SCM_GOBJECT_OBJECT(rx);
    gy = SCM_GOBJECT_OBJECT(ry);
    if (GTK_IS_RADIO_BUTTON(gx)) {
        if (GTK_IS_RADIO_BUTTON(gy)) {
            GtkRadioButton *bx = GTK_RADIO_BUTTON(gx);
            GtkRadioButton *by = GTK_RADIO_BUTTON(gy);
            return (gtk_radio_button_get_group(bx) == gtk_radio_button_get_group(by))? 0 : -1;
        }
        return -1;
    }
    if (GTK_IS_RADIO_MENU_ITEM(gx)) {
        if (GTK_IS_RADIO_MENU_ITEM(gy)) {
            GtkRadioMenuItem *bx = GTK_RADIO_MENU_ITEM(gx);
            GtkRadioMenuItem *by = GTK_RADIO_MENU_ITEM(gy);
            return (gtk_radio_menu_item_get_group(bx) == gtk_radio_menu_item_get_group(by))? 0 : -1;
        }
        return -1;
    }
    return -1;
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GtkRadioGroupClass,
                         NULL, radio_group_compare, NULL, NULL,
                         NULL);

/* 'radio' must be either a GtkRadioButton or a GtkRadioMenuItem */
ScmObj Scm_MakeGtkRadioGroup(GObject *radio)
{
    GSList *glist = NULL;
    ScmGtkRadioGroup *group;
    if (GTK_IS_RADIO_BUTTON(radio)) {
        glist = gtk_radio_button_get_group(GTK_RADIO_BUTTON(radio));
    } else if (GTK_IS_RADIO_MENU_ITEM(radio)) {
        glist = gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(radio));
    } else {
        Scm_Error("<gtk-radio-group> can be created only for <gtk-radio-button> or <gtk-radio-menu-item> object, but got an instance of %s",
                  g_type_name(G_OBJECT_TYPE(radio)));
    }
    group = SCM_NEW(ScmGtkRadioGroup);
    SCM_SET_CLASS(group, SCM_CLASS_GTK_RADIO_GROUP);
    if (glist != NULL) {
        group->radio = Scm_MakeGObject(radio);
    } else {
        group->radio = SCM_FALSE;
    }
    return SCM_OBJ(group);
}

GSList *Scm_GtkRadioGroupGetGroup(ScmObj g)
{
    GObject *gradio;
    ScmGtkRadioGroup *group;
    if (SCM_FALSEP(g)) return NULL;
    if (!SCM_GTK_RADIO_GROUP_P(g)) {
        Scm_Error("<gtk-radio-group> or #f required, but got %S", g);
    }
    group = SCM_GTK_RADIO_GROUP(g);
    if (SCM_FALSEP(group->radio)) return NULL;
    SCM_ASSERT(SCM_GOBJECT_P(group->radio));
    gradio = SCM_GOBJECT_OBJECT(group->radio);
    if (GTK_IS_RADIO_BUTTON(gradio)) {
        return gtk_radio_button_get_group(GTK_RADIO_BUTTON(gradio));
    }
    if (GTK_IS_RADIO_MENU_ITEM(gradio)) {
        return gtk_radio_menu_item_get_group(GTK_RADIO_MENU_ITEM(gradio));
    }
    Scm_Error("internal inconsistency in %S", group);
    return NULL;                /* dummy */
}

ScmObj Scm_GtkRadioGroupToList(ScmGtkRadioGroup *group)
{
    GSList *glist = Scm_GtkRadioGroupGetGroup(SCM_OBJ(group));
    if (glist == NULL) return SCM_NIL;
    else return Scm_GoSListToList(glist);
}

/*===============================================================
 * Initialization
 */

#include "gtk-lib.inits"
extern void Scm_Init_gauche_glib(ScmModule*);
extern void Scm_Init_gauche_gdklib(ScmModule*); /* mmc! */

void Scm_Init_gauche_gtk(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_gtk);
    mod = SCM_MODULE(SCM_FIND_MODULE("gtk", TRUE));

    g_type_init();
    gtkdata.scmclass_key = g_quark_from_static_string("ScmClass");
    gtkdata.scmobj_key = g_quark_from_static_string("ScmObj");


    referenced_gobjects = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));


    (void)SCM_INTERNAL_MUTEX_INIT(gtkdata.protected_mutex);
    gtkdata.protected = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));
    (void)SCM_INTERNAL_MUTEX_INIT(gtkdata.typemap_mutex);
    gtkdata.typemap = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 0));

    /* mmc: This is the first part. good! */
    typemap_initialize(gtkdata.typemap);

    Scm_InitBuiltinClass(&Scm_GObjectClass, "<g-object>",
                         NULL, sizeof(ScmGObject),
                         mod);
    Scm_InitBuiltinClass(&Scm_GTypeClass, "<g-type>",
                         NULL, sizeof(ScmGType),
                         mod);
    Scm_InitBuiltinClass(&Scm_PangoLayoutIterClass, "<pango-layout-iter>",
                         NULL, sizeof(ScmPangoLayoutIter),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkAtomClass, "<gdk-atom>",
                         NULL, sizeof(ScmGdkAtom),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkEventClass, "<gdk-event>",
                         NULL, sizeof(ScmGdkEvent),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkRegionClass, "<gdk-region>",
                         NULL, sizeof(ScmGdkRegion),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkPointVectorClass, "<gdk-point-vector>",
                         NULL, sizeof(ScmGdkPointVector),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkSegmentVectorClass, "<gdk-segment-vector>",
                         NULL, sizeof(ScmGdkSegmentVector),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkRectangleVectorClass, "<gdk-rectangle-vector>",
                         NULL, sizeof(ScmGdkRectangleVector),
                         mod);
    Scm_InitBuiltinClass(&Scm_GdkColorVectorClass, "<gdk-color-vector>",
                         NULL, sizeof(ScmGdkColorVector),
                         mod);
    Scm_InitBuiltinClass(&Scm_GtkRadioGroupClass, "<gtk-radio-group>",
                         NULL, sizeof(ScmGtkRadioGroup),
                         mod);
    Scm_Init_gauche_glib(mod);
    Scm_Init_gauche_gdklib(mod);
    Scm_Init_gtk_lib(mod);      /* this is in  gtk-lib.inits ! */

#if 1
    /* Now I should register the Event GTypes! */
    Scm_GtkRegisterClass(GDK_TYPE_EVENT, &Scm_GdkEventAnyClass);
#else
    {
       ScmClass *klass = &Scm_GdkEventAnyClass;
       ScmGdkEvent *g;
       struct EvClassTableRec *ctab;

       {
          int count = 0;
          for (ctab = evClassTable; ctab->type >= 0; ctab++) count++;
          Scm_Warn("registering GDK %d events\n", __FUNCTION__, count);
          /* sizeof(evClassTable)/sizeof(evClassTable[0]),   array_size */
       }

       for (ctab = evClassTable; ctab->type >= 0; ctab++) {
          Scm_GtkRegisterClass(ctab->type,
                               ctab->klass);
       }
    }
#endif
    Scm_GtkInitUnixSignalHook();

    /* mmc: */
#if 0
    hint_table  =  (ScmHashTable*)
       Scm_MakeHashTable((ScmHashProc) SCM_HASH_STRING, /* (ScmHashProc) Scm_HashString_mmc, */
                         (ScmHashCmpProc) NULL, /* Scm_EqvHash_mmc, /* typedef int (*ScmHashCmpProc)(ScmObj, ScmHashEntry *);
                                                 *
                                                 * string = ?? */
                         1000);

#endif
}




/* mmc:*/

/* implementing <list-of-gchar*> */
int SCM_STRING_LIST_P(ScmObj x)
{
    ScmObj p;
    if (SCM_LISTP(x)){
        SCM_FOR_EACH(p, x){
            if (! SCM_STRINGP(SCM_CAR(p))) return 0;
        }
        return 1;
    } else return 0;
}

/* unboxer: */
gchar**
SCM_STRING_LIST(ScmObj x)
{
    int len = Scm_Length(x);
    /* allocate the array of pointers */
    gchar** cstrings = (malloc (1 + len)); /* should i malloc all the space at once? */
    ScmObj p;
    int index = 0;
    /* allocate the single strings */
    for((p) = (x); SCM_PAIRP(p); (p) = SCM_CDR(p)){
        cstrings[index++] = (gchar*) Scm_GetStringConst((ScmString *) SCM_CAR(p));
    }
    cstrings[len] = 0;
    return cstrings;
}


ScmObj SCM_MAKE_STRING_LIST(gchar** cstring)
{
   int len = 0;
   gchar** head = cstring;
   while (*cstring++) len++;

#if GAUCHE_MAJOR_VERSION == 0 && GAUCHE_MINOR_VERSION == 8 && GAUCHE_MICRO_VERSION <= 6
   return Scm_CStringArrayToList(head, len);
#else
   /* fixme: gchar vs char ? */
   return Scm_CStringArrayToList((const char**) head, len, SCM_MAKSTR_COPYING);
#endif
}

/* Get the list of Signals: */

/* implementing  GSignalQuery type:*/
static void
ScmGSignalQuery_finalize(ScmObj obj, void* data)
{
    /*  no need to test. the finalizer has just been deduced from the structure itself ? */
    GSignalQuery *g = SCM_G_SIGNAL_QUERY(obj);
#if 0
    printf("pg_finalize\\n");
#endif
    if (g) { g_free(g); g = NULL; }	/* fixme:  bug! Should be compatible with g-signal-query in gauche-glib.stub */
}


ScmObj
Scm_Make_GSignalQuery (GSignalQuery *data)
{
    ScmGSignalQuery* g = SCM_NEW(ScmGSignalQuery);
    SCM_SET_CLASS(g, SCM_CLASS_G_SIGNAL_QUERY);
    g->data = data;
    g->name = (ScmString*) SCM_MAKE_STR_COPYING(data->signal_name); /* fixme: constant! */
    Scm_RegisterFinalizer(SCM_OBJ(g), ScmGSignalQuery_finalize, NULL);
    return SCM_OBJ(g);
};

#define DEBUG_EMIT 0
ScmObj
Scm_g_signal_emit(ScmObj destination, int signal_id, int detail, ScmObj params)
{

    /* Can signals be emitted on gobjects only?    Here we assume so!*/

    if (!SCM_GOBJECT_P(destination))
        Scm_Error("<g-object> required, but got %S", destination);

    /* GObject *gobject = Scm_GObjectCheck (SCM_GOBJECT(destination)); */

    /* Get the info */
    GSignalQuery g = {0};
    g_signal_query(signal_id,&g);

    if (g.signal_id == 0) {
        Scm_Error("gobject says: This signal %d is unknown!", signal_id);
    }

    /* Check if signal can be applied to gobject */

#if 1
    GType desc_gtype = Scm_ClassToGtkType(SCM_CLASS_OF(destination));
    if (!g_type_is_a(desc_gtype, g.itype))
        {
            Scm_Error("gobject says: This signal %s cannot be emited on %S. Needs %s",
                      g.signal_name, destination, g_type_name(g.itype)) ; /* cname */
        }
#else
    ScmClass* klass = Scm_GtkTypeToScmClass(g.itype);
    /* this might be tested in g- tools ? */
    if (! SCM_XTYPEP(destination, klass))
        {
            Scm_Warn("The signal cannot be applied!");
#if 0
            /* type_name */
            const char* cname;
            if (! klass->name)  /* SCM_UNDEFINED?*/
                cname = "???";
            else  {
                Scm_Warn("\tGetting the name!");
                cname = Scm_GetString(SCM_STRING(klass->name));
            }
            Scm_Warn("\tGot the name!");
#endif
            Scm_Error("This signal %s cannot be emited on %S", g.signal_name, klass) ; /* cname */
        }
#endif


#if DEBUG_EMIT
    Scm_Warn("The signal can be applied to this gobject!");
#endif

    /* Check the number & types of args */
    if (Scm_Length(params) != g.n_params)
        Scm_Error("wrong number of signal parameters: %d should be %d", Scm_Length(params), g.n_params);


    {
        int i = 0;
        ScmObj p;
        SCM_FOR_EACH(p, params) {

#if 0
            ScmClass* klass = Scm_GtkTypeToScmClass(g.param_types[i]);
            if (! SCM_XTYPEP(SCM_CAR(p), klass)) {
                Scm_Error("wrong argument %d: for signal %s, wants %s you provided %S", i, g.signal_name,
                          g_type_name(g.param_types[i]), SCM_CAR(p));
            }
            /* GtkMovementStep -> int  */
#else

#if DEBUG_EMIT
            Scm_Warn("testing type of param %d", i);
#endif
            /* Get the SCM klass -> GType and ask if the value GType is ok ...*/
            ScmObj o = SCM_CAR(p);

            ScmClass *klass;

            if (SCM_INTEGERP(o))
                klass = SCM_CLASS_INTEGER;
            else
                klass = SCM_CLASS_OF(o);

            GType gtype = Scm_ClassToGtkType(klass);

            if (gtype == G_TYPE_INVALID)
                Scm_Error("wrong argument %d: %S", i, SCM_CAR(p));
#if DEBUG_EMIT
            Scm_Warn("comparing 2 gtypes: %s %s", g_type_name(gtype), g_type_name(g.param_types[i]));
#endif
            if (! ( g_type_is_a(g.param_types[i], gtype) ||
                    (g_type_is_a(g.param_types[i], G_TYPE_ENUM)) && SCM_INTEGERP(o)))
                {
                    Scm_Error("wrong argument %d: for signal %s, wants %s you provided %S", i, g.signal_name,
                              g_type_name(g.param_types[i]), SCM_CAR(p));
                }
#endif
            i++;
        };
#if DEBUG_EMIT
        Scm_Warn("The signal parameters are of correct types!");
#endif
    }


    /* todo: How to test can we accept the return type? */

    {
        GValue return_val = {0};
        g_value_init(&return_val, g.return_type);

        GValue* instance_and_params = g_new0(GValue, g.n_params + 1);

        g_value_init(instance_and_params, g.itype);
        Scm_BoxGValue(instance_and_params, destination);  /* Should I provide a fixed Gtype? */

        int i = 1;
        ScmObj p;
        SCM_FOR_EACH(p, params) {
#if DEBUG_EMIT
            Scm_Warn("converting %d %S", i-1, SCM_CAR(p));
#endif
            g_value_init(instance_and_params + i, g.param_types[i-1]);
            Scm_BoxGValue(instance_and_params +i, SCM_CAR(p));
            i++;
        }

        g_signal_emitv(instance_and_params, signal_id, detail, &return_val);

        /* todo: return the returned Gvalue. */
        SCM_RETURN(SCM_UNDEFINED);
    }
}


/* print method of
 * the <gdk-pixbuf> class. Could be done in scheme (given gdk-pixbuf-format-get-name) ?
 * mmc: Maybe not. I want info on NULL connection too. But working w/ such complicates all? */
void
gauche_gdk_pixbuf_format_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    GdkPixbufFormat* f=SCM_GDK_PIXBUF_FORMAT(obj);
    if (f)                       /* fixme!  gobject_live ? */
        {
            Scm_Printf(out, "#<gdk-pixbuf-format %s>", gdk_pixbuf_format_get_name(f)); /* fixme: no name */
        }
    else Scm_Printf(out, "#<gdk-pixbuf-format>");
}
