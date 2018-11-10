

;; what it wants from types and struct/enum/array

;; struct:
;; definition: i think i simply include the whole .types file!
;; scm-class-name-of c-type-of c-name-of
;; c-predicate-of c-unboxer-of c-boxer-of

;; only for `defined'
;; c-predicate-nullable-of
;; allocation-type-of 

;; emit.stub-class-hierarchy
;; cpl-of


;; emit.h  .... i already have it. 


(define-module h2s.emit
  (export emit-all)

  (use h2s.objects)
  (use h2s.gtk-types-for-emit)
  (use h2s.utils)
  (use h2s.track)


  (use text.tr)
  (use file.util)
  (use srfi-13)
  (use srfi-1)
  (use srfi-2)
  (use util.toposort)
  )
(select-module h2s.emit)



(define debug #f)

;;================================================================
;; PASS 3 - EMITTER
;;

;; emit.types - generate *.types file
;; emit.h     - generate *.h file
;; emit.stub  - generate *.stub file

(define-method emit.types ((self <gtk-struct>) commenter)
  (print #`"(define-type ,(scm-class-name-of self) \",(c-name-of (c-type-of self))\" #f")
  (print #`"  \",(c-predicate-of self)\" \",(c-unboxer-of self)\" \",(c-boxer-of self)\")")
  ;; when do we use this one?
  ;; mmc: does this mean that every unboxer must handle the NULL case?
  (print #`"(define-type ,(scm-class-name-of self)-or-null  \",(c-name-of (c-type-of self))\" #f")
  (print #`"  \",(c-predicate-nullable-of self)\" \",(c-unboxer-of self)\" \",(c-boxer-of self)\")")
  (print))

;; example of this?  ....types in .hint files? How is that distributed/assigned to files?  heh,  (input-file "gtkcelllayout.h") is the parameter!!!!
(define-method emit.types ((self <extra-stub>) commenter)
  (when (type? self)
    (print-body self)))


;; where is this produced ??
;; (define-type <pango-attr-list> "PangoAttrList*" #f #f #f "SCM_MAKE_PANGO_ATTR_LIST")
;; (define-type <pango-context> "PangoContext*" #f #f #f "SCM_MAKE_PANGO_CONTEXT")

;; this is my manual stuff !!
;; (define-type <list-of-gchar*> "gchar **" "list of C strings, free-d by the external library" "SCM_STRING_LIST_P" "SCM_STRING_LIST" "SCM_MAKE_STRING_LIST")



(define-method emit.types ((self <top>) commenter) #f) ; what is <top> ??
;; this means, that functions don't produce text!



;; Q: where is the inheritance defined ?  mm: i would expect in the (define-cclass     `emit.stub-class-hierarchy'
(define-method emit.h ((self <gtk-struct>) commenter)
  (let1 atype (allocation-type-of self)
    ;; Structure definition.  Note necessary for GObjects.
    (case atype
      ((refcounted indirect simple)
       (print #`"typedef struct Scm,(c-name-of self)Rec {")
       (print #`"    SCM_HEADER;")
       (print #`"    ,(c-name-of self) ,(if (eq? atype 'simple) \"\" \"*\")data;")
       (print #`"} Scm,(c-name-of self);")
       (print)))
    ;; Class declaration
    (print #`"SCM_CLASS_DECL(Scm_,(c-name-of self)Class);")
    (print #`"#define ,(c-class-macro-of self)     (&Scm_,(c-name-of self)Class)")
    ;; Type predicate
    (case atype
      ((gobject)
       (print #`"#define ,(c-predicate-of self)(obj)    (Scm_TypeP(obj, ,(c-class-macro-of self)))")) ; walk the gauche inheritance tree
      (else
       ;; mmc: SCM_GTK_SOURCE_BUFFER is not a gobject ??? gtk says it is.   GtkTextBuffer -> 
       (print #`"#define ,(c-predicate-of self)(obj)     SCM_XTYPEP(obj, ,(c-class-macro-of self))")))
    ;; Boxer and unboxer
    (case atype
      ((gobject)
       ;; mmc:  dynamic typing?
       (print #`"#define ,(c-unboxer-of self)(obj)      SCM_GOBJECT_UNBOX(,(c-caster-of self), obj)")
       (print #`"#define ,(c-boxer-of self)(obj) SCM_GOBJECT_BOX(obj)")); see: /usr/lib/gauche/0.8.2/include/gauche-gtk.h
      ((refcounted indirect)
       ;; mmc: examples of these^???
       (print #`"#define ,(c-unboxer-of self)(obj)      (SCM_FALSEP(obj)?NULL:((Scm,(c-name-of self)*)(obj))->data)")
       (print #`"#define ,(c-boxer-of self)(obj) (Scm_Make,(c-name-of self)(obj))"))
      (else
       ;; we don't want to segfault: if the object provides no way to ensure that it's not deallocated, we cannot provide a
       ;; "once a pointer to it".  So we provide what we are sure about: the `data' slot.
       ;; even worse?                                                         V   sort-of inline: the `data' slot itself is the value.
       (print #`"#define ,(c-unboxer-of self)(obj)      (SCM_FALSEP(obj)?NULL:&((Scm,(c-name-of self)*)(obj))->data)")
       (print #`"#define ,(c-boxer-of self)(obj) (Scm_Make,(c-name-of self)(obj))")))


    ;;
    (print #`"#define ,(c-predicate-nullable-of self)(obj) (SCM_FALSEP(obj)||,(c-predicate-of self)(obj))")

    ;;; why is Gobject excluded?
    ;;  b/c we have  SCM_GOBJECT_BOX ?
    (case atype
      ((refcounted indirect simple)     ; ???? rest is what?
       ;; mmc:cannot all these be the same function? (w/o C type-checking)
       (print #`"extern ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data);")))
    (print)))

(define-method emit.h ((self <top>) commenter) #f)


(define-method emit.stub ((self <gtk-struct>) commenter)
  (print (commenter #`" struct ,(c-name-of self)"))
  (print)
  (unless (internal? self)              ; once again: when?  functions  _ref _unref _get_type. But structs ??
                                        ; `disable-cclass' in the hints file!
    (case (allocation-type-of self)
     ((gobject)    (emit.stub-gobject self commenter))
     ((indirect)   (emit.stub-indirect self commenter)) ; again: what is `indirect'?     just pointer at a memory!
     ((refcounted) (emit.stub-refcounted self commenter))
     (else (emit.stub-simple self commenter)))))


;; self is a <gtk-struct> object.
(define (emit.stub-gobject self commenter)
  (print #`"(define-cclass ,(scm-class-name-of self) :,(keyword->string (qualifier-of self))") ; (define-cclass <gtk-xxx>  :base
  (print #`"  \"ScmGObject*\" \"Scm_,(c-name-of self)Class\"")
  (emit.stub-class-hierarchy self)
  ;; Exclude the first slot which is an instance of superclass
  (let1 fields (fields-of self)
    (if (null? fields)
        (emit.stub-fields '())
        (emit.stub-fields (cdr fields))))
  ;; Allocator
  (print #`"(allocator (c \"Scm_GtkObjectAllocate\"))") ;
  ;; for when i want to create an object in scheme: calling (make <klass> where klass is a subclass(!) of a gtk one.
  ;; /p/gauche-gtk-0.4.1/work/Gauche-gtk-0.4.1/src/gauche-gtk.c

  ;; mmc: what is this parameter to `define-cclass' ?  This is only for Multiple-Inheritance! (otherwise (first cpi))
  ;; Direct supers, if it has mixin
  (when (not (null? (direct-supers-of self)))
    (write (cons 'direct-supers (direct-supers-of self))) (newline))
  (print #`"  )")
  (print)
  ;; Register initialization code
  (print #`"(initcode \"Scm_GtkRegisterClass(,(gtk-type-name-of self), ,(c-class-macro-of self));\n\")")
  ;; why is this needed?  why don't i use initcode in my hand-written .stub files?
  (print))




;; it's all pieces of C embedded !
(define (emit.stub-refcounted self commenter)
  (let ((finalizer #`"scm_,(c-name-of self)_finalize")
        (cfn (string-tr (mixed-case-name->hyphenated-name
                         (x->string (c-name-of self)))
                        "-" "_")))
    (print #`"\"static void ,|finalizer|(ScmObj obj, void* data)")
    (print #`" {")
    (print #`"   Scm,(c-name-of self) *p = (Scm,(c-name-of self)*)obj;")
    (print #`"   ,(c-name-of self) *d = ,(c-unboxer-of self)(obj);")
    (print #`"   ,|cfn|_unref(d);")
    (print #`"   p->data = NULL;")
    (print #`" }\"")
    (print)
    (print #`"\"ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data)")
    (print #`" {")
    (print #`"   Scm,(c-name-of self) *z = SCM_NEW(Scm,(c-name-of self));")
    (print #`"   SCM_SET_CLASS(z, ,(c-class-macro-of self));")
    (print #`"   z->data = data;")
    (print #`"   Scm_RegisterFinalizer(SCM_OBJ(z), ,|finalizer|, NULL);")
    (print #`"   ,|cfn|_ref(z->data);")
    (print #`"   return SCM_OBJ(z);")
    (print #`" }\"")
    (print)
    (print #`"(define-cclass ,(scm-class-name-of self)")
    (print #`"  \",(c-name-of self)*\" \"Scm_,(c-name-of self)Class\"")
    (emit.stub-class-hierarchy self)
    (emit.stub-fields (fields-of self))
    (print #`"  )")
    (print)))

(define (emit.stub-indirect self commenter)
  (let ((finalizer #`"scm_,(c-name-of self)_finalize"))
    (when (c-free-proc-of self)
      (print #`"\"static void ,|finalizer|(ScmObj obj, void* data)")
      (print #`" {")
      (print #`"   Scm,(c-name-of self) *p = (Scm,(c-name-of self)*)obj;")
      (print #`"   ,(c-name-of self) *d = ,(c-unboxer-of self)(obj);")
      (print #`"   ,(c-free-proc-of self)(d);")
      (print #`"   p->data = NULL;")
      (print #`" }\"")
      (print))
    (print #`"\"ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data)")
    (print #`" {")
    (print #`"   Scm,(c-name-of self) *z = SCM_NEW(Scm,(c-name-of self));")
    (print #`"   SCM_SET_CLASS(z, ,(c-class-macro-of self));")
    (if (c-copy-proc-of self)
        (print #`"  z->data = ,(c-copy-proc-of self)(data);")
      (print #`"   z->data = data;"))
    (when (c-free-proc-of self)
      (print #`"    Scm_RegisterFinalizer(SCM_OBJ(z), ,|finalizer|, NULL);"))
    (print #`"   return SCM_OBJ(z);")
    (print #`" }")
    (print #`"\"")
    (print)
    (print #`"(define-cclass ,(scm-class-name-of self)")
    (print #`"  \",(c-name-of self)*\" \"Scm_,(c-name-of self)Class\"")
    (emit.stub-class-hierarchy self)
    (emit.stub-fields (fields-of self))
    ;; What if we provide a printer!?
    (when (slot-bound? self 'printer)
      (print "(printer")
      (write (slot-ref self 'printer))
      (print ")"))
    (print #`"  )")
    (print)))

(define (emit.stub-simple self commenter)
  (print #`"\"ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data)")
  (print #`" {")
  (cond
   ((allocator-of self) => print)       ;
   (else
    (print #`"   Scm,(c-name-of self) *z = SCM_NEW(Scm,(c-name-of self));")
    (print #`"   SCM_SET_CLASS(z, ,(c-class-macro-of self));")
    (print #`"   if (data) z->data = *data; /*copy*/")
    (print #`"   return SCM_OBJ(z);")))
  (print #`" }")
  (print #`"\"")
  (print)
  (print #`"(define-cclass ,(scm-class-name-of self)")
  (print #`"  \",(c-name-of self)*\" \"Scm_,(c-name-of self)Class\"")
  (emit.stub-class-hierarchy self)
  (emit.stub-fields (fields-of self))
  (print #`"  (allocator \"return Scm_Make,(c-name-of self)(NULL);\")")
  (print #`"  )")
  (print))

(define (emit.stub-class-hierarchy self)
  (format #t "  ~s\n" (cpl-of self)))

(define (emit.stub-fields fields)
  (print "  (")
  (for-each emit.stub-field fields)
  (print "   )"))

(define (emit.stub-field field)
  (let ((sname (scm-name-of field))
        (type  (scm-type-of (type-of field))))
    (if (accessible? field)
        (format #t "   ~s\n"            ; we are outputing the .stub: this is sexp!
                `(,sname
                  ,@(if (not (pair? type))
                        (list :type type) ; the best option!
                      '())
                  ,@(if (not (eq? (getter-of field) #t))
                        (list :getter (getter-of field))
                      ;; no getter!
                      '())
                  ,@(if (not (eq? (setter-of field) #t))
                        (list :setter (setter-of field))
                      '())))
      (begin
                                        ;(logformat-color 207 "field ~a not accessible!\n" sname)
        (print #`"   ;; ,sname :type ,type")))))

(define-method emit.stub ((self <gtk-enum>) commenter)
  (print (commenter #`" enum ,(c-name-of self)"))
  (for-each (lambda (v)
              (print #`"(define-enum ,v)"))
            (values-of self))
  (print))

;; (define-cproc gtk-entry-new () (return <gtk-widget> "gtk_entry_new"))
(define-method emit.stub ((self <gtk-function>) commenter)
  (print (commenter (c-name-of self)))
  (unless (internal? self)
    (let* ((unknown-arg? #f)
           (s-name (scm-name-of self))  ;function!
           (args   (map (lambda (arg)
                          (cond
                           ((null? arg) (error "huh?" (c-name-of self)))
                           ((symbol? arg) arg) ; these come e.g.  from fix-arguments! (in .hints files)
                           (else
                            (let ((name (c-name-of arg))
                                  (s-type (scm-type-of (type-of arg))))  ;; scm-type-of is method of <gtk-type> (or <gtk-alias>)
                              (when (pair? s-type)
                                (if debug (logformat-color 'red
                                             "type ~a is unknown (~a)\n"
                                           (type-of arg) (scm-type-of (type-of arg))))
                                (set! unknown-arg? #t))
                              (string->symbol #`",|name|::,|s-type|")))))
                     (arguments-of self)))
           (ret    (or (and-let* ((r (return-type-of self))
                                  (t (scm-type-of r)))
                         (when (pair? t)
                           (set! unknown-arg? #t))
                         (list t))
                       '()))
           (sig    `(define-cproc ,s-name ,args
                      ,(or (body-of self)
                           `(call ,@ret ,(x->string (c-name-of self)))))))
      (if unknown-arg?
          (begin
            ;(print "this is wrong, right?")
            (print (commenter sig)))       ;mmc: wrong....
        (begin (write sig) (newline))))
    (print)))

(define-method emit.stub ((self <extra-stub>) commenter)
  (unless (type? self)
    (print-body self)))

(define (c-commenter content) #`"/* ,|content| */")
;(define (s-commenter content) #`";; ,content")

(define (s-commenter content); #`";; ,content")
  (let1 content (if (string? content) content
                  #`",content")
    (string-append ";; " (string-join (string-split content "\n") "\n;; "))))

(define (emitter method commenter)
  ;; mmc: fixme:
  (if (eq? commenter s-commenter)
      (print (commenter "-*-scheme-*-"))
    (print (commenter "-*-c-*-")))

  (print (commenter "Automatically generated - DO NOT EDIT"))
  (print)
  (for-each-source-file
   (lambda (file defined)               ;defined is the list of definitions
     (unless (string=? file "archive")  ;fixme:  this is the one i use for DB: can it be a #f ?
       (print (commenter (sys-basename file)))
       (print)
       (for-each (cut method <> commenter) defined)))))

;; Emit gtk-lib.inits
;; The order of initialization is important, since the superclasses have
;; to be initialized before initializing subclass.   Sort-files-for-inits
;; takes care of it.

(define (emit.inits init-function)
  (define (base file)
    (string-tr (string-drop-right (sys-basename file) 2) "-" "_"))
  (define files (sort-files-for-inits))

  (if debug (logformat "emit.inits: ~a\n" files))
  (print (c-commenter "-*-c-*-"))
  (print (c-commenter "Automatically generated - DO NOT EDIT"))
  (print)
  (for-each (lambda (file)
              (print #`"extern void Scm_Init_,(base file)(ScmModule*);"))
            files)
  (print)
  (print init-function)                 ;mmc ! fixed
  (print #`"{")
  (for-each (lambda (file)
              (print #`"  Scm_Init_,(base file)(mod);"))
            files)
  (print #`"}"))



;; Sort files so that classes are initialized in the right order.
(define (sort-files-for-inits)
  (let* ((files&defs
          (alist-delete "archive"
                        (get-files&definitions)))
         ;; Quick mapper from <gtk-struct> to is defining file
         (struct->file
          (let ((table (make-hash-table)))
            (for-each (lambda (file&defs)
                        (for-each (lambda (def)
                                    (when (is-a? def <gtk-struct>)
                                      (hash-table-put! table def (car file&defs))))
                                  (cdr file&defs)))
                      files&defs)
            (lambda (struct)
              (hash-table-get table struct #f))))
         ;; returns a file that defines supertype of the given struct, or #f.
         (get-super-file
          (lambda (src-file struct)
            (and-let* ((super (superclass-of struct))
                       ((is-a? (body-of super) <gtk-struct>))
                       (file (struct->file (body-of super)))
                       ((not (equal? src-file file))))
              file)))
         (dependency-table
          (let ((table (make-hash-table 'string=?)))
            (for-each (lambda (file&defs)
                        (hash-table-put! table (car file&defs) '()))
                      files&defs)
            (for-each (lambda (file&defs)
                        (receive (file defs) (car+cdr file&defs)
                          (for-each (lambda (def)
                                      (and-let* (((is-a? def <gtk-struct>))
                                                 (super (get-super-file file def))
                                                 ((not (member super (hash-table-get table file)))))
                                        (hash-table-push! table file super)))
                                    defs)))
                      files&defs)
            table)))

    (if debug (logformat "sort-files-for-inits: ~a\n" files&defs))
    (logformat "~s\n" (hash-table-map dependency-table cons))
    (reverse (topological-sort (hash-table-map dependency-table cons)))))



;; mmc: i think these are added because they are defined below the .h files we scan. I.e. in glib.
;; 

;; These types will be added to gtk-lib.types
(define *predefined-types*
  '((define-type <const-char*> "const char *" #f
      "SCM_STRINGP" "CONST_CHAR_PTR")
    (define-type <const-gchar*> "const gchar *" #f
      "SCM_STRINGP" "CONST_GCHAR_PTR")
    (define-type <gchar*> "gchar *" #f
      "SCM_STRINGP" "CONST_GCHAR_PTR" "GCHAR_PTR_BOX")
    (define-type <const-gchar*>-or-null "const gchar *" #f
      "SCM_STRING_OR_NULL_P" "CONST_GCHAR_PTR_NULLABLE")
    (define-type <g-object> "GObject *" #f
      "SCM_GOBJECT_P" "SCM_GOBJECT_OBJECT" "SCM_GOBJECT_BOX")
    (define-type <g-timer> "GTimer *" #f
      "SCM_GTIMER_P" "SCM_GTIMER" "SCM_MAKE_GTIMER")
    (define-type <gdk-atom> "GdkAtom" #f
      #f #f "SCM_MAKE_GDK_ATOM")
    (define-type <gdk-event> "GdkEvent*" #f
      #f #f "SCM_MAKE_GDK_EVENT")
    (define-type <gdk-point-vector> "ScmGdkPointVector*")
    (define-type <gdk-segment-vector> "ScmGdkSegmentVector*")
    (define-type <gdk-rectangle-vector> "ScmGdkRectangleVector*")
    (define-type <gdk-color-vector> "ScmGdkColorVector*")
    (define-type <gtk-radio-group> "ScmGtkRadioGroup*")

    ;; we need a standard mechanism to incorporate these, really.
    (define-type <u8vector> "ScmU8Vector*")
    (define-type <s8vector> "ScmS8Vector*")
    (define-type <u16vector> "ScmU16Vector*")
    (define-type <s16vector> "ScmS16Vector*")
    (define-type <u32vector> "ScmU32Vector*")
    (define-type <s32vector> "ScmS32Vector*")
    (define-type <u64vector> "ScmU64Vector*")
    (define-type <s64vector> "ScmS64Vector*")
    (define-type <f32vector> "ScmF32Vector*")
    (define-type <f64vector> "ScmF64Vector*")
    ))

;; overwrite the original file iff it is changed; avoiding triggering
;; excessive make.
(define (with-output-to-file-if-changed file thunk)
  (let1 tmpfile #`",|file|.tmp"
    (with-output-to-file tmpfile thunk)
    (if (and (file-exists? file)
             (file-equal? tmpfile file))
        (begin
          (if debug (report #`"  no change in ,file"))
          (sys-unlink tmpfile))
        (begin
          (report #`"  writing ,file")
          (sys-rename tmpfile file)))))

(define (emit-all types-file central-header-file include-h-file init-function inits-file)

  ;;  stub type definitions

  ;; This is to a common .types file, included by all .stubs.
  ;; It contains the (define-type    )     sort-of opaque definition? `<extra-stub>'
  (with-output-to-file-if-changed types-file
    (lambda ()
      (emitter emit.types s-commenter)
      ;; some additional stuff
      (for-each (lambda (t) (write t) (newline)) *predefined-types*)))


  ;; i also need a central .h where i #include stuff (gauche.h) ...
  
  ;;central repository for generated types.
  (with-output-to-file-if-changed central-header-file
    (lambda () (emitter emit.h c-commenter)))

  ;; all these include that central type repo ^^^ b/c we don't track the .h files dependency, we define just all, in case...
  ;; 
  (for-each-source-file
   (lambda (file defined)
     (unless (string=? file "archive")  ;fixme:  this is the one i use for DB: can it be a #f ?

       (receive (extra normal)
           (partition (cut is-a? <> <extra-stub>) defined)
         ;; 
         (with-output-to-file-if-changed
          #`",(string-drop-right (sys-basename file) 2).stub"
          (lambda ()
            (print (s-commenter "-*-scheme-*-"))
            (print (s-commenter "Automatically generated - DO NOT EDIT"))
            (print)

            ;; fixme!

            ;; this was original: 
            (write `(include ,types-file))


            ;(print #`"(include \",types-file\"")
           
            (unless (string=? types-file "gtk-lib.types")
              ;; include even this one:
              (print)
              (write `(include ,(string-append "/usr/lib/gauche/" (gauche-version) "/include/gtk-lib.types")))) ;fixme

           
            (print)
            ;; something which includes not only the central type repo. but also gauche specific .h files, and the .h file
            ;; which defines the macros used in our type repo!!  gobject_unbox ...


            ;(for-each-reverse include-h-file
             
            (write #`"#include \",include-h-file\"")
            (print)
            (print)
            ;; need to emit <extra-stub> first, for they may define
            ;; static fns.   mmc: and? - C compiler want things defined _before_ used. (used in fix-body! )
            (for-each (cut emit.stub <> s-commenter) extra)
            (for-each (cut emit.stub <> s-commenter) normal)))))))
  ;mmc:
  (with-output-to-file-if-changed inits-file
                                  (lambda ()
                                    (emit.inits init-function))))


(provide "h2s/emit")
