;;
;; parse gtk headers to generate stub file
;; $Id: h2stub.scm,v 1.51 2004/06/24 11:49:19 shirok Exp $

;; OK.  Defs file should be the way to go.  However, gtk+ distribution
;; doesn't include defs file, and I couldn't find one for gtk-2.0 in
;; guile archive.   For now, I hack this script.
;;
;; This script scans Gdk/Gtk headers and generate stub files.  It doesn't
;; parse entire C syntax, though.  It relies on gtk's coding convention.
;; Furthermore, it takes a manually created 'hints' file.

(use srfi-1)
(use srfi-2)
(use srfi-11)
(use srfi-13)
(use file.util)
(use text.tr)
(use util.toposort)
(use gauche.process)
(use gauche.parameter)
(use gauche.mop.instance-pool)

;;================================================================
;; PARAMETERS (explicit global states)
;;

;; Keeps input file name during parsing
(define input-file (make-parameter #f))

;; Verbose flag
(define verbose (make-parameter #t))

;; Gtk and Pango version (major.minor) we're dealing with
(define gtk-version   (make-parameter "2.0"))
(define pango-version (make-parameter "1.0"))

;; Directories to search input header files.
;; We first try pkg-config.  If it fails, does
;; heuristic search.  
(define *header-search-paths*
  (delete-duplicates
   (append (call-with-input-process
               #`"pkg-config --variable=includedir gtk+-,(gtk-version)"
             (cut port->string-list <>)
             :on-abnormal-exit :ignore)
           (call-with-input-process
               #`"pkg-config --variable=includedir pango-,(pango-version)"
             (cut port->string-list <>)
             :on-abnormal-exit :ignore)
           '("/usr/include" "/usr/local/include"))
   string=?))

(define (find-header-dir target paths)
  (cond ((find-file-in-paths target
                             :paths paths
                             :pred file-is-readable?)
         => (lambda (p) (sys-dirname (sys-dirname p))))
        (else 
         (error #`",|target| couldn't find in ,*header-search-paths*"))))

(define gtk-directory
  (make-parameter (find-header-dir #`"gtk-,(gtk-version)/gtk/gtk.h"
                                   *header-search-paths* )))
(define pango-directory
  (make-parameter (find-header-dir #`"pango-,(pango-version)/pango/pango.h"
                                   *header-search-paths*)))

;;================================================================
;; CLASSES
;;

(define-method for-each-instance (proc (class <instance-pool-meta>))
  (for-each proc (instance-pool->list class)))

(define-class <source-tracker-mixin> ()
  ((source-file :accessor source-file-of)
   (files&definitions :allocation :class :initform '())
   ))

(define-method initialize ((self <source-tracker-mixin>) initargs)
  (next-method)
  (let1 file (sys-basename (input-file))
    (set! (source-file-of self) file)
    (let1 p (assoc file (slot-ref self 'files&definitions))
      (if p
          (push! (cdr p) self)
          (slot-push! self 'files&definitions (list file self))))))

;; call proc with source file name and a list of objects defined in that file.
(define (for-each-source-file proc)
  (for-each (lambda (def-list)
              (proc (car def-list) (reverse (cdr def-list))))
            (reverse (class-slot-ref <source-tracker-mixin>
                                     'files&definitions))))

(define (get-files&definitions)
  (reverse (class-slot-ref <source-tracker-mixin>
                           'files&definitions)))

;; <GTK-TYPE> - type
(define-class <gtk-type> (<instance-pool-mixin>)
  ((c-name    :init-keyword :c-name :accessor c-name-of)
   ;; symbol, such as 'GdkWindow*
   (body      :init-keyword :body :init-value #f :accessor body-of)
   ;; has <gtk-struct>, <gtk-enum> or <gtk-array> if applicable.
   ;; symbol when this is a primitive type.
   ))

(define-method write-object ((self <gtk-type>) port)
  (format port "<~a>" (c-name-of self)))

(define (find-type name)
  (instance-pool-find <gtk-type>
                      (lambda (item) (eq? (c-name-of item) name))))

(define (find-type-or-create name)
  (or (find-type name)
      (make <gtk-type> :c-name name)))

;; map <gtk-type> to stub type signature.
(define-method scm-type-of ((self <gtk-type>))
  (let1 body (body-of self)
    (cond
     ((symbol? body) body)
     ((is-a? body <gtk-enum>) '<int>)
     ((is-a? body <gtk-struct>) (scm-class-name-of body))
     (else (cons 'UNKNOWN (c-name-of self))))))

;; returns a fn that creates a C code fragment of unboxing/boxing slot value.
;; UGLY - this doesn't deal with array ref.
(define-method get-slot-boxer ((self <gtk-type>))
  (let1 body (body-of self)
    (cond
     ((is-a? body <gtk-struct>)
      (cut string-append (c-boxer-of body) "(" <> ")"))
     ((is-a? body <gtk-enum>)
      (cut string-append "Scm_MakeInteger(" <> ")"))
     ((symbol? body)
      ;; primitive type.  There should be an interface to get this kind
      ;; of information; maybe lang.c.type module?  For now, I hardcode them.
      (case body
        ((<char>) (cut string-append "SCM_MAKE_CHAR(" <> ")"))
        ((<boolean>) (cut (string-append "SCM_MAKE_BOOL(" <> ")")))
        ((<int> <int8> <int16> <int32> <long>)
         (cut string-append "Scm_MakeInteger(" <> ")"))
        ((<uint> <uint8> <uint16> <uint32> <ulong>)
         (cut string-append "Scm_MakeIntegerFromUI(" <> ")"))
        ((<float> <double>)
         (cut string-append "Scm_MakeFlonum(" <> ")"))
        ((<const-char*> <const-gchar*> <gchar*>)
         (cut string-append "SCM_MAKE_STR_COPYING_SAFELY(" <> ")"))
        (else #f)))
     (;; check if it is an embedded structure.
      (and-let* ((ptrtype
                  (find-type (string->symbol #`",(c-name-of self)*")))
                 (ptrbody (body-of ptrtype))
                 ((is-a? ptrbody <gtk-struct>)))
        (cut string-append (c-boxer-of ptrbody) "(&(" <> "))")))
     (else #f))))

;; define primitive types
(for-each
 (lambda (entry)
   (make <gtk-type> :c-name (car entry) :body (cadr entry)))
 '((gint       <int>)
   (gint8      <int8>)
   (gint16     <int16>)
   (gint32     <int32>)
   (glong      <long>)
   (gshort     <int16>)
   (guint      <uint>)
   (guint8     <uint8>)
   (guint16    <uint16>)
   (guint32    <uint32>)
   (gulong     <ulong>)
   (guchar     <uint8>)
   (gushort    <uint16>)
   (gboolean   <boolean>)
   (gfloat     <float>)
   (gdouble    <double>)
   (long       <long>)
   (int        <int>)
   (short      <int16>)
   (char       <int8>)
   (void       <void>)
   (float      <float>)
   (double     <double>)

   ;; C string business is tricky.  We can only treat the case that
   ;; passing const char * or const gchar * - in those cases, gtk copies
   ;; the passed string immediately, so we can safely pass the string
   ;; from ScmGetStringConst*.
   (const-char*  <const-char*>)
   (const-gchar* <const-gchar*>)
   ;; Generic GObject
   (GObject* <g-object>)
   ;; This is used to box the returned allocated gchar*
   (gchar*       <gchar*>)
   ;; Opaque types
   (PangoContext* <pango-context>)
   (PangoLanguage* <pango-language>)
   (PangoAttrList* <pango-attr-list>)
   (PangoLayoutIter* <pango-layout-iter>)
   (GdkAtom    <gdk-atom>)
   (GdkRegion* <gdk-region>)
   (GdkPixbufFormat* <gdk-pixbuf-format>)
   (GtkTreePath* <gtk-tree-path>)
   (GtkTreeRowReference* <gtk-tree-row-reference>)
   ;; GdkEvent is a union.
   (GdkEvent*  <gdk-event>)
   ;; GtkAllocation is simply an alias of GdkRectangle
   (GtkAllocation* <gdk-rectangle>)
   ;; Interfaces
   (GtkEditable* <gtk-editable>)
   (GtkTreeModel* <gtk-tree-model>)
   (GtkTreeSortable* <gtk-tree-sortable>)
   ))

;; <GTK-VAR> - used in fields and arguments
(define-class <gtk-var> ()
  ((type      :init-keyword :type   :accessor type-of)
   (c-name    :init-keyword :c-name :accessor c-name-of)
   (scm-name  :allocation :virtual :accessor scm-name-of
              :slot-ref  (lambda (o) (string->symbol (string-tr (x->string (c-name-of o)) "_" "-")))
              :slot-set! (lambda (o v) #f))
   ;; the following slots are used by field info
   (read-only? :initform #f :accessor read-only?)
   (accessible? :initform #t :accessor accessible?)
   (getter     :init-keyword :getter :initform #f :accessor getter-of)
   (setter     :init-keyword :setter :initform #f :accessor setter-of)
   ))

(define-method write-object ((self <gtk-var>) port)
  (format port "<~a ~a>" (type-of self) (c-name-of self)))

(define (grok-vardef type name)
  (let*-values (((ptrs var) (let1 brk (string-skip name #\*)
                              (values (string-take name brk)
                                      (string-drop name brk))))
                ((typesig)  (if (pair? type)
                                #`",(car type)-,(cdr type),|ptrs|"
                                #`",|type|,|ptrs|")))
    (make <gtk-var>
      :type (find-type-or-create (string->symbol typesig))
      :c-name var)))

(define (grok-arraydef type name dim)
  (receive (ptrs var)
      (let1 brk (string-skip name #\*)
        (values (string-take name brk) (string-drop name brk)))
    (let* ((elt-typesig  (if (pair? type)
                             #`",(car type)-,(cdr type),|ptrs|"
                             #`",|type|,|ptrs|"))
           (typesig  #`",elt-typesig[,dim]")
           (type     (find-type-or-create (string->symbol typesig)))
           (elt-type (find-type-or-create (string->symbol elt-typesig))))
      (set! (body-of type)
            (make <gtk-array> :size dim :element-type elt-type))
      (make <gtk-var> :type type :c-name var))))

;; <GTK-STRUCT>
(define-class <gtk-struct> (<instance-pool-mixin>
                            <source-tracker-mixin>)
  ((c-name    :init-keyword :c-name :accessor c-name-of)
   (fields    :init-keyword :fields :accessor fields-of)
   (internal?   :init-value #f :accessor internal?)
   ;; - true if this struct is not exposed to Scheme.  set by fixup
   (c-type    :accessor c-type-of)
   ;; - for struct _GdkFoo, keeps #<gtk-type GdkFoo*>
   (superclass :accessor superclass-of)
   ;; - if inherited, this one keeps <gtk-type> of the parent class.
   (cpl :accessor cpl-of)
   ;; - class precedence list derived from superclass field.  set by fixup.
   (allocation-type :accessor allocation-type-of :init-form 'simple)
   ;; - how the C structure should be allocated and freed
   ;;    simple     : ScmObj contains the entire structure.
   ;;    gobject    : ScmObj points to GObject*
   ;;    indirect   : ScmObj points to a mem that should be freed.
   ;;    refcounted : ScmObj points to refcounted object.
   (scm-class-name :accessor scm-class-name-of)
   ;; - <gdk-foo> ; set by make-struct
   (c-caster       :accessor c-caster-of)
   ;; - GDK_FOO   ; set by make-struct
   (c-predicate    :accessor c-predicate-of)
   ;; - SCM_GDK_FOO_P ; set by make-struct
   (c-predicate-nullable :accessor c-predicate-nullable-of)
   ;; - SCM_GDK_FOO_OR_NULL_P ; set by make-struct
   (c-unboxer      :accessor c-unboxer-of)
   ;; - SCM_GDK_FOO   ; set by make-struct
   (c-boxer        :accessor c-boxer-of)
   ;; - SCM_MAKE_GDK_FOO   ; set by make-struct
   (c-class-macro  :accessor c-class-macro-of)
   ;; -SCM_CLASS_GDK_FOO   ; set by make-struct
   (gtk-predicate  :accessor gtk-predicate-of)
   ;; - GDK_IS_FOO   ; set by make-struct
   (gtk-type-name  :accessor gtk-type-name-of)
   ;; - GDK_TYPE_FOO   ; set by make-struct
   (c-copy-proc :accessor c-copy-proc-of :init-value #f)
   ;; - Used by indirect struct, keeping C procedure name to copy
   ;;   the data part.  can be set in hints file.
   (c-free-proc :accessor c-free-proc-of :init-value #f)
   ;; - Used by indirect struct, keeping C procedure name to free
   ;;   the data part.  can be set in hints file.
   (allocator      :init-form #f :accessor allocator-of)
   ;; - Special allocator setting that overrides the default.
   ;;   May be set by hints file.   This can be a string for
   ;;   entire allocator body, or an assoc-list of required
   ;;   initargs and the constructor to call.
   (qualifier      :init-value :built-in :accessor qualifier-of)
   ;; - define-cclass qualifier.  adjusted in fixup.
   (direct-supers  :init-value () :accessor direct-supers-of)
   ;; - extra direct-supers if this class has a mixin.
   ))

(define-method write-object ((self <gtk-struct>) port)
  (format port "#<gtk-struct ~s>" (c-name-of self)))

(define-method gobject? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'gobject))

(define-method indirect? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'indirect))

(define-method refcounted? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'refcounted))

(define (make-struct name fields)
  (let* ((c-name (string-drop name 1))   ;; drop preceding '_'
         (s (make <gtk-struct> :c-name (string->symbol c-name) :fields fields))
         (tn (find-type-or-create (string->symbol #`",|c-name|*")))
         (scmname (mixed-case-name->hyphenated-name c-name))
         )
    (set! (c-type-of s) tn)
    (set! (body-of tn) s)
    (set! (scm-class-name-of s) (string->symbol #`"<,|scmname|>"))
    (let1 base (string-tr scmname "a-z-" "A-Z_")
      (set! (c-caster-of s) base)
      (set! (c-predicate-of s) #`"SCM_,|base|_P")
      (set! (c-predicate-nullable-of s) #`"SCM_,|base|_OR_NULL_P")
      (set! (c-unboxer-of s) #`"SCM_,|base|")
      (set! (c-boxer-of s) #`"SCM_MAKE_,|base|")
      (set! (c-class-macro-of s) #`"SCM_CLASS_,|base|")
      ;; Anormality: GdkWindowObject uses GDK_IS_WINDOW macro
      (set!-values
       ((gtk-predicate-of s) (gtk-type-name-of s))
       (cond
        ((equal? c-name "GdkWindowObject")
         (values "GDK_IS_WINDOW" "GDK_TYPE_WINDOW"))
        ((string-prefix? "PANGO" base)
         (values #`",(string-take base 6)IS_,(string-drop base 6)"
                 #`",(string-take base 6)TYPE_,(string-drop base 6)"))
        (else  ;; either GDK_ or GTK_
         (values #`",(string-take base 4)IS_,(string-drop base 4)"
                 #`",(string-take base 4)TYPE_,(string-drop base 4)"))))
      )
    s))

(define (find-struct scm-name)
  (instance-pool-find <gtk-struct>
                      (lambda (s) (eq? (scm-class-name-of s) scm-name))))

;; <GTK-ARRAY>
(define-class <gtk-array> ()
  ((size      :init-keyword :size :accessor size-of)
   (element-type :init-keyword :element-type :accessor element-type-of)))

;; <GTK-ENUM>
(define-class <gtk-enum> (<instance-pool-mixin>
                          <source-tracker-mixin>)
  ((c-name    :init-keyword :c-name :accessor c-name-of)
   (values    :init-keyword :values :accessor values-of)
   ))

(define (make-enum name values)
  (let* ((n (string->symbol name))
         (s (make <gtk-enum> :c-name n :values values)))
    (set! (body-of (find-type-or-create n)) s)
    s))

;; <GTK-FUNCTION>
(define-class <gtk-function> (<instance-pool-mixin>
                              <source-tracker-mixin>)
  ((c-name      :init-keyword :c-name :accessor c-name-of)
   (return-type :init-keyword :return-type :accessor return-type-of)
   (arguments   :init-keyword :arguments :accessor arguments-of)
   (internal?   :init-value #f :accessor internal?)
   ;; - true if this function is not exposed to Scheme.  set by fixup-functions
   (scm-name    :init-keyword :scm-name :accessor scm-name-of)
   ;; - scheme name, like gtk-foo for C-function gtk_foo.
   (body        :init-value #f :accessor body-of)
   ))

(define (make-function name ret args)
  (let1 scm-name
      (string->symbol (string-tr (x->string name) "_" "-"))
    (make <gtk-function>
      :c-name name :scm-name scm-name :return-type ret :arguments args)))

(define (find-function scm-name)
  (instance-pool-find <gtk-function>
                      (lambda (f) (eq? (scm-name-of f) scm-name))))

;; <EXTRA-STUB> - literal stub added by hints file
(define-class <extra-stub> (<source-tracker-mixin>)
  (;; s-expr to be placed in the stub file
   (body        :init-keyword :body :accessor body-of)
   ;; true if this should go to .types file instead of .stub file
   (type?       :init-keyword :type? :accessor type?)))

(define-method print-body ((self <extra-stub>))
  (write (body-of self))
  (newline)
  (newline))

;;================================================================
;; UTILITIES
;;

(define (report msg)
  (when (verbose)
    (display msg (current-error-port))
    (newline (current-error-port))))

;; FooBarBaz => foo-bar-baz
;; FooZBar   => foo-zbar
;; FooZZBar  => foo-zz-bar
(define (mixed-case-name->hyphenated-name name)
  (define (loop current prev ncaps)
    (cond ((eof-object? current) (write-char (char-downcase prev)))
          ((char-upper-case? current)
           (if (char-lower-case? prev)
               (begin
                 (write-char prev)
                 (write-char #\-)
                 (loop (read-char) current 1))
               (begin
                 (write-char (char-downcase prev))
                 (loop (read-char) current (+ ncaps 1)))))
          ((char-lower-case? current)
           (when (> ncaps 2) (write-char #\-))
           (write-char (char-downcase prev))
           (loop (read-char) current 0))
          (else
           (write-char (char-downcase prev))
           (loop (read-char) current 0))))
  (with-output-to-string
    (lambda ()
      (with-input-from-string name
        (lambda ()
          (let1 c0 (read-char)
            (unless (eof-object? c0)
              (loop (read-char) c0 0)))))))
  )

;;================================================================
;; PASS 1 - PARSER
;;

(define (parse-headers dir hlist)
  (for-each (lambda (hdr) (parse-header (build-path dir hdr))) hlist))

(define (parse-header filename)
  (parameterize ((input-file filename))
    (report #`"parsing ,filename")
    (with-input-from-file filename parse-body)))

(define (parse-body)
  (rxmatch-case (read-line)
    (test eof-object?)
    (#/^struct (_G[dt]k\w+)/ (#f name) (parse-struct name) (parse-body))
    (#/^struct (_Pango\w+)/  (#f name) (parse-struct name) (parse-body))
    (#/^typedef enum/        ()        (parse-enum) (parse-body))
    (#/^([\w\*_]+)\s+((\*+)\s*)?((g[dt]k|pango)_[\w_]+)\s*\((.+)$/ (#f ret #f ptr fn #f rest)
       (parse-function (if ptr #`",|ret|,|ptr|" ret) fn rest)
       (parse-body))
    (else (parse-body))))

(define (parse-struct name)
  (define (err-eof) (errorf "EOF while parsing struct ~s" name))
  (define (parse-struct-body)
    (let loop ((line (read-line))
               (fields '()))
      (rxmatch-case line
        (test eof-object? (err-eof))
        (#/^\s*$/ () (loop (read-line) fields))
        (#/^\{/ () (loop (read-line) fields))
        (#/^\}/ () (make-struct name (reverse fields)))
        (test has-comment? (skip-comment line (cut loop <> fields) err-eof))
        (#/^\s+([\w\*_]+)\s+([\w\*_]+)(\[([\w_]+)\])?\s*(:\s*\d+\s*)?\;/
           (#f type var #f array)
         (if array
             (loop (read-line) (cons (grok-arraydef type var array) fields))
             (loop (read-line) (cons (grok-vardef type var) fields))))
        (#/^\s+[\w\*_]+\s+\**\([\w\*_]+\)(.*)/ (#f rest)
         ;; function pointer - ignore
         (let loop2 ((line rest))
           (rxmatch-case line
             (test eof-object? (err-eof))
             (#/\)\;/ () (loop (read-line) fields))
             (else (loop2 (read-line))))))
        (#/^\s+([\w\*_]+)\s+([\w\*_]+)\s*,(.*)$/ (#f type var rest)
         ;; something like int x,y;
         (let loop2 ((rest rest)
                     (fields (cons (grok-vardef type var) fields)))
           (rxmatch-case rest
             (#/\s*([\w\*_]+)\s*\;/ (#f var)
              (loop (read-line) (cons (grok-vardef type var) fields)))
             (#/\s*([\w\*_]+)\s*,(.*)$/ (#f var rest)
              (loop2 rest (cons (grok-vardef type var) fields)))
             (else (warn "~s in ~a" line name) (loop (read-line) fields)))))
        (else (warn "~s in ~a" line name) (loop (read-line) fields))
        )))
  (define (skip-struct)
    (let loop ((line (read-line)))
      (rxmatch-case line
        (test eof-object? (err-eof))
        (#/^\{/ () (loop (read-line)))
        (#/^\}/ () '())
        (else (loop (read-line)))
        )))

  (if (string-suffix? "Class" name)
      (skip-struct)
      (parse-struct-body))
  )

(define (parse-enum)
  (define (err-eof) (error "EOF while parsing enum"))
  (let loop ((line (read-line))
             (enums '()))
    (rxmatch-case line
      (test eof-object? (err-eof))
      (#/^\{/ () (loop (read-line) enums))
      (#/^\}\s*([\w_]+)/ (#f name) (make-enum name (reverse enums)))
      (#/^\s*$/ () (loop (read-line) enums))
      (test has-comment? (skip-comment line (cut loop <> enums) err-eof))
      (#/\s+([\w_]+),?/ (#f enum)
        (loop (read-line) (cons enum enums)))
      (#/\s+([\w_]+)\s+=/ (#f enum)
        (loop (read-line) (cons enum enums)))
      (else (warn "~s in enum" line) (loop (read-line) enums))
      )))
  
(define (parse-function ret name rest)
  (define (err-eof) (errorf "EOF while parsing function ~s" name))
  (define (grok-arg argstr)
    (rxmatch-case (string-trim-both argstr)
      (#/^const\s+(.+)$/ (#f rest)
        (let1 r (grok-arg rest) (acons 'const (car r) (cdr r))))
      (#/^([\w\*_]+)\s+([\w\*_]+)$/ (#f type name) (cons type name))
      (#/^([\w\*_]+)\s+\(([\w\*_]+)\)/ (#f type name) (acons 'fn type name))
      (#/^void$/ () '())
      (#/^...$/ () (cons "VARARG" "..."))
      (else (warn "can't grok arg ~a in ~a" argstr name)
            '("UNKNOWN" . "UNKNOWN"))))
  (define (finish-function ret name rargs)
    (make-function name
                   (find-type-or-create (string->symbol ret))
                   (reverse (map (lambda (arg)
                                   (let1 a (grok-arg arg)
                                     (if (null? a)
                                         a
                                         (grok-vardef (car a) (cdr a)))))
                                 rargs))))

  (let loop ((line rest)
             (args '()))
    (rxmatch-case line
      (test eof-object? (err-eof))
      (#/^\s*$/ () (loop (read-line) args))
      (test has-comment? (skip-comment line (cut loop <> args) err-eof))
      (#/\s*([^,]+),(.*)/ (#f arg rest)
        (loop rest (cons arg args)))
      (#/\s*(.+)\).*\;/ (#f arg)
        (finish-function ret name (cons arg args)))
      (else
       (warn "~s in ~a" line name)
       (loop (read-line) args)))))

(define (has-comment? line) (string-scan line "/*"))

(define (skip-comment line cont error-eof)
  (receive (prev next) (string-scan line "/*" 'both)
    (cond
     ((string-scan next "*/" 'after)
      => (lambda (n) (cont #`",|prev| ,|n|")))
     (else
      (let loop ((line (read-line)))
        (rxmatch-case line
          (test eof-object? (error-eof))
          (#/\*\/(.*)/ (#f rest)
            (cont #`",|prev| ,|rest|"))
          (else (loop (read-line))))))))
  )

;;================================================================
;; PASS 2 - FIXUPS
;;

;; Load "hints" files
(define (load-hints)
  (parameterize ((input-file "hints.h")) ;;dummy
    (for-each (lambda (file)
                (when (file-exists? file)
                  (report #`"  loading ,file")
                  (load file)))
              '("./pango-lib.hints"
                "./gdk-lib.hints"
                "./gtk-lib.hints"
                ))))

;; utilities that can be used inside hints file
(define-macro (define-cproc-fix name . body)
  `(cproc-fix ',name (lambda (self) ,@body)))

(define (cproc-fix name body)
  (let1 self
      (or (find-function name)
          (make <gtk-function>
            :scm-name name
            :c-name (string->symbol (string-tr (x->string name) "-" "_"))
            :return-type (find-type 'void)
            :arguments '()))
    (body self)))

(define-macro (disable-cproc name)
  `(cond ((find-function ',name)
          => (lambda (f) (set! (internal? f) #t)))))

(define-macro (fix-arguments! args)
  `(set! (arguments-of self) ,args))

(define-macro (fix-body! body)
  `(begin
     (set! (return-type-of self) #f)
     (set! (body-of self) ,body)))

(define-macro (define-cclass-fix name . body)
  `(cclass-fix ',name (lambda (self) ,@body)))

(define (cclass-fix name body)
  (let1 self
      (or (find-struct name)
          (error "cclass-fix: no such struct" name))
    (body self)))

(define-macro (disable-cclass name)
  `(cond ((find-struct ',name)
          => (lambda (s) (set! (internal? s) #t)))))

(define-macro (fix-field! name . body)
  `((lambda (field) . ,body)
    (or (find (lambda (p) (eq? (scm-name-of p) ,name))
              (fields-of self))
        (error "no such field" ',name))))

(define-macro (add-field! c-name typesig . opts)
  `(set! (fields-of self)
         (append (fields-of self)
                 (make <gtk-var>
                   :c-name ,c-name
                   :type (find-type-or-create ',typesig)
                   ,@opts))))

;; some field may be missing in certain gtk versions.
;; use this to ignore such fields.  NB: there may be more than
;; one field registered, if such fields appears within union.
(define-macro (ignore-field! name)
  `(for-each (lambda (f) (set! (accessible? f) #f))
             (filter (lambda (p) (eq? (scm-name-of p) ,name))
                     (fields-of self))))

(define-macro (ignore-field-except! names)
  `(for-each (lambda (f) (set! (accessible? f) #f))
             (remove (lambda (p) (memq (scm-name-of p) ,names))
                     (fields-of self))))

(define-macro (add-mixin! . c-mixin-names)
  `(begin (set! (direct-supers-of self)
                (list ,@c-mixin-names (car (cpl-of self))))
          (set! (cpl-of self)
                (list* ,@c-mixin-names (cpl-of self)))))

;; adds opaque GObject.
(define-macro (define-opaque c-name type)
  `(make-opaque ',c-name ,type))

(define (make-opaque c-name type)
  (let* ((struct (make-struct #`"_,|c-name|" '())))
    (case type
      ((:gobject)
       (set! (allocation-type-of struct) 'gobject)
       (set! (superclass-of struct) (find-type 'GObject))
       (set! (cpl-of struct) '("Scm_GObjectClass")))
      ((:indirect)
       (set! (allocation-type-of struct) 'indirect)
       (set! (superclass-of struct) #f)
       (set! (cpl-of struct) '()))
      ((:refcounted)
       (set! (allocation-type-of struct) 'refcounted)
       (set! (superclass-of struct) #f)
       (set! (cpl-of struct) '()))
      (else (error "unknown opaque object type" type)))))

;; extra cproc and cclass defined in the fix file is copied to
;; the output stub file.
(define-macro (define-cclass . args)
  `(make <extra-stub> :body '(define-cclass ,@args) :type? #f))
(define-macro (define-cproc . args)
  `(make <extra-stub> :body '(define-cproc ,@args) :type? #f))
(define-macro (define-enum . args)
  `(make <extra-stub> :body '(define-enum ,@args) :type? #f))
(define-macro (define-constant . args)
  `(make <extra-stub> :body '(define-constant ,@args) :type? #f))
(define-macro (define-type . args)
  `(make <extra-stub> :body '(define-type ,@args) :type? #t))
(define-macro (raw-code . args)
  `(make <extra-stub> :body (string-join ',args "\n" 'suffix) :type? #f))

;; figure out what implemenation type each structure is,
;; by examining its first field.

(define-method set-superclass ((self <gtk-struct>))
  (if (slot-bound? self 'superclass)
      (superclass-of self)
      (receive (superclass gobject)
          (if (null? (fields-of self))
              (values #f #f)
              (let ((first-slot-type (type-of (car (fields-of self)))))
                (cond
                 ((eq? (c-name-of first-slot-type) 'GObject)
                  (values first-slot-type #t))
                 ((and-let* ((ptrname (string->symbol #`",(c-name-of first-slot-type)*"))
                             (ptrtype (find-type ptrname))
                             ((is-a? (body-of ptrtype) <gtk-struct>))
                             ((set-superclass (body-of ptrtype))))
                    (values ptrtype (gobject? (body-of ptrtype)))))
                 (else (values #f #f)))))
        (when gobject (set! (allocation-type-of self) 'gobject))
        (set! (superclass-of self) superclass)
        superclass)))

(define-method set-cpl ((self <gtk-struct>))
  ;; after setting up superclass field of all structs, sets up CPL.
  ;; the hints file may modify CPL afterwards.
  (set! (cpl-of self)
        (let loop ((super (superclass-of self))
                   (classes '()))
          (cond ((not super) (reverse classes))
                ((eq? (c-name-of super) 'GObject)
                 (reverse (cons "Scm_GObjectClass" classes)))
                ((eq? (c-name-of super) 'GdkEvent*)
                 (reverse (cons "Scm_GdkEventClass" classes)))
                (else (loop (superclass-of (body-of super))
                            (cons #`"Scm_,(c-name-of (body-of super))Class"
                                  classes))))))
  )

(define-method set-refcounted ((self <gtk-struct>))
  ;; use heuristics to find out if self is a ref-counting object (and not an
  ;; GObject).
  (when (find (lambda (field) (equal? (c-name-of field) "ref_count"))
              (fields-of self))
    (set! (allocation-type-of self) 'refcounted)))

(define-method set-fields ((self <gtk-struct>))
  ;; scan <gtk-var>'s in the fields and sets up it's default setter and getter
  (define (set-field-getter-n-setter field)
    (let ((stub-type (scm-type-of (type-of field))))
      (cond
       ((eq? stub-type '<gchar*>)
        (set! (ref field 'getter)
              #`"return SCM_MAKE_STR_COPYING_SAFELY(obj->,(c-name-of field));")
        (set! (ref field 'setter) #f))
       ((not (pair? stub-type)) ; has proper stub type, so no need of g&s.
        (set! (ref field 'getter) #t)
        (set! (ref field 'setter) #t))
       (;; check if it is an embedded structure.
        (and-let* ((ptrtype
                    (find-type (string->symbol #`",(c-name-of (type-of field))*")))
                   (ptrbody (body-of ptrtype))
                   ((is-a? ptrbody <gtk-struct>)))
          (set! (ref field 'getter)
                #`"return ,(c-boxer-of ptrbody)(&(obj->,(c-name-of field)));")
          (set! (ref field 'setter)
                #`"obj->,(c-name-of field) = *,(c-unboxer-of ptrbody)(value);")
          ))
       (;; check if it is an array reference.
        (and-let* ((arr (body-of (type-of field)))
                   ((is-a? arr <gtk-array>))
                   (elttype (element-type-of arr))
                   (unboxer (get-slot-boxer elttype)))
          (set! (ref field 'getter)
                #`"ScmObj vec = Scm_MakeVector(,(size-of arr), SCM_FALSE); int i;
                   for (i=0; i<,(size-of arr); i++) {
                     SCM_VECTOR_ELEMENTS(vec)[i] = ,(unboxer #`\"(obj->,(c-name-of field)[i])\");
                   }
                   return vec;")
          (set! (ref field 'setter) #f)))
       (else
        (set! (ref field 'accessible?) #f)))))
  (for-each set-field-getter-n-setter (fields-of self))
  )

(define-method set-qualifier ((self <gtk-struct>))
  ;; sets GtkObject subclasses :base class, so that Scheme subclass can
  ;; be defined.
  (when (eq? (allocation-type-of self) 'gobject)
    (set! (qualifier-of self) :base)))

(define (fixup-structs)
  ;; Special treatment : GdkBitmap*, GdkPixmap* and GdkWindow* are really
  ;; synonyms of GdkDrawable*.
  (let ((gdkdrawable (find-type 'GdkDrawable*)))
    (for-each (lambda (n)
                (set! (body-of (find-type n)) (body-of gdkdrawable)))
              '(GdkBitmap* GdkPixmap* GdkWindow*)))
  ;; GtkAllocation is an alias of GdkRectangle.
  (let ((gtk-allocation-type  (find-type-or-create 'GtkAllocation*))
        (gdk-rectangle-struct (find-struct '<gdk-rectangle>)))
    (set! (body-of gtk-allocation-type) gdk-rectangle-struct))
  (for-each-instance set-superclass <gtk-struct>)
  (for-each-instance set-cpl        <gtk-struct>)
  (for-each-instance set-refcounted <gtk-struct>)
  (for-each-instance set-fields     <gtk-struct>)
  )

(define (fixup-structs-after)
  ;; need to do this after loading hints, for the allocation type of
  ;; the struct may be modified in hints.
  (for-each-instance set-qualifier  <gtk-struct>)
  )

;; Some heuristics to remove irrelevant functions
(define-method set-internal ((self <gtk-function>))
  (let1 cnam (x->string (c-name-of self))
    (when (or (string-suffix? "_ref" cnam)
              (string-suffix? "_unref" cnam)
              (string-suffix? "_get_type" cnam))
      (set! (internal? self) #t))))

(define-method fix-arg ((self <gtk-function>)) 
  ;; This is for (void) argument list
  (when (equal? (arguments-of self) '(()))
    (set! (arguments-of self) '()))
  ;; Ignore 'const' qualifier (except const-char*)
  (for-each (lambda (arg)
              (let* ((type (type-of arg))
                     (typename (c-name-of type)))
                (when (and (not (memq typename '(const-char* const-gchar*)))
                           (string-prefix? "const-" (x->string typename)))
                  (set! (type-of arg)
                        (find-type-or-create (string->symbol
                                              (string-drop (x->string typename)
                                                           6)))))))
            (arguments-of self))
  )

(define (fixup-functions)
  (for-each-instance set-internal <gtk-function>)
  (for-each-instance fix-arg <gtk-function>)
  )

(define (fixup)
  (fixup-structs)
  (fixup-functions)
  (load-hints)
  (fixup-structs-after)
  )

;;================================================================
;; PASS 3 - EMITTER
;;

;; emit.types - generate *.types file
;; emit.h     - generate *.h file
;; emit.stub  - generate *.stub file

(define-method emit.types ((self <gtk-struct>) commenter)
  (print #`"(define-type ,(scm-class-name-of self) \",(c-name-of (c-type-of self))\" #f")
  (print #`"  \",(c-predicate-of self)\" \",(c-unboxer-of self)\" \",(c-boxer-of self)\")")
  (print #`"(define-type ,(scm-class-name-of self)-or-null  \",(c-name-of (c-type-of self))\" #f")
  (print #`"  \",(c-predicate-nullable-of self)\" \",(c-unboxer-of self)\" \",(c-boxer-of self)\")")
  (print))

(define-method emit.types ((self <extra-stub>) commenter)
  (when (type? self)
    (print-body self)))

(define-method emit.types ((self <top>) commenter) #f)

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
       (print #`"#define ,(c-predicate-of self)(obj)    (Scm_TypeP(obj, ,(c-class-macro-of self)))"))
      (else
       (print #`"#define ,(c-predicate-of self)(obj)     SCM_XTYPEP(obj, ,(c-class-macro-of self))")))
    ;; Boxer and unboxer
    (case atype
      ((gobject)
       (print #`"#define ,(c-unboxer-of self)(obj)      SCM_GOBJECT_UNBOX(,(c-caster-of self), obj)")
       (print #`"#define ,(c-boxer-of self)(obj) SCM_GOBJECT_BOX(obj)"))
      ((refcounted indirect)
       (print #`"#define ,(c-unboxer-of self)(obj)      (SCM_FALSEP(obj)?NULL:((Scm,(c-name-of self)*)(obj))->data)")
       (print #`"#define ,(c-boxer-of self)(obj) (Scm_Make,(c-name-of self)(obj))"))
      (else
       (print #`"#define ,(c-unboxer-of self)(obj)      (SCM_FALSEP(obj)?NULL:&((Scm,(c-name-of self)*)(obj))->data)")
       (print #`"#define ,(c-boxer-of self)(obj) (Scm_Make,(c-name-of self)(obj))")))
    (print #`"#define ,(c-predicate-nullable-of self)(obj) (SCM_FALSEP(obj)||,(c-predicate-of self)(obj))")
    (case atype
      ((refcounted indirect simple)
       (print #`"extern ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data);")))
    (print)
    ))

(define-method emit.h ((self <top>) commenter) #f)

(define-method emit.stub ((self <gtk-struct>) commenter)
  (print (commenter #`" struct ,(c-name-of self)"))
  (print)
  (unless (internal? self)
    (case (allocation-type-of self)
     ((gobject)    (emit.stub-gobject self commenter))
     ((indirect)   (emit.stub-indirect self commenter))
     ((refcounted) (emit.stub-refcounted self commenter))
     (else (emit.stub-simple self commenter)))))

(define (emit.stub-gobject self commenter)
  (print #`"(define-cclass ,(scm-class-name-of self) :,(qualifier-of self)")
  (print #`"  \"ScmGObject*\" \"Scm_,(c-name-of self)Class\"")
  (emit.stub-class-hierarchy self)
  ;; Exclude the first slot which is an instance of superclass
  (let1 fields (fields-of self)
    (if (null? fields)
        (emit.stub-fields '())
        (emit.stub-fields (cdr fields))))
  ;; Allocator
  (print #`"(allocator (c \"Scm_GtkObjectAllocate\"))")
  ;; Direct supers, if it has mixin
  (when (not (null? (direct-supers-of self)))
    (write (cons 'direct-supers (direct-supers-of self))) (newline))
  (print #`"  )")
  (print)
  ;; Register initialization code
  (print #`"(initcode \"Scm_GtkRegisterClass(,(gtk-type-name-of self), ,(c-class-macro-of self));\n\")")
  (print)
  )

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
    (print)
    ))

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
    (print #`"  )")
    (print)
    ))

(define (emit.stub-simple self commenter)
  (print #`"\"ScmObj Scm_Make,(c-name-of self)(,(c-name-of self) *data)")
  (print #`" {")
  (cond
   ((allocator-of self) => print)
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
  (print)
  )

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
        (format #t "   ~s\n"
                `(,sname
                  ,@(if (not (pair? type))
                        (list :type type)
                        '())
                  ,@(if (not (eq? (getter-of field) #t))
                        (list :getter (getter-of field))
                        '())
                  ,@(if (not (eq? (setter-of field) #t))
                        (list :setter (setter-of field))
                        '())))
        (print #`"   ;; ,sname :type ,type"))))

(define-method emit.stub ((self <gtk-enum>) commenter)
  (print (commenter #`" enum ,(c-name-of self)"))
  (for-each (lambda (v)
              (print #`"(define-enum ,v)"))
            (values-of self))
  (print))

(define-method emit.stub ((self <gtk-function>) commenter)
  (print (commenter (c-name-of self)))
  (unless (internal? self)
    (let* ((unknown-arg? #f)
           (s-name (scm-name-of self))
           (args   (map (lambda (arg)
                          (cond
                           ((null? arg) (error "huh?" (c-name-of self)))
                           ((symbol? arg) arg)
                           (else
                            (let ((name (c-name-of arg))
                                  (s-type (scm-type-of (type-of arg))))
                              (when (pair? s-type)
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
                           `(return ,@ret ,(x->string (c-name-of self))))))
           )
      (if unknown-arg?
          (print (commenter sig))
          (begin (write sig) (newline))))
    (print)))

(define-method emit.stub ((self <extra-stub>) commenter)
  (unless (type? self)
    (print-body self)))

(define (c-commenter content) #`"/* ,|content| */")
(define (s-commenter content) #`";; ,content")

(define (emitter method commenter)
  (print (commenter "Automatically generated - DO NOT EDIT"))
  (print)
  (for-each-source-file
   (lambda (file defined)
     (print (commenter (sys-basename file)))
     (print)
     (for-each (cut method <> commenter) defined)
     )))

;; Emit gtk-lib.inits
;; The order of initialization is important, since the superclasses have
;; to be initialized before initializing subclass.   Sort-files-for-inits
;; takes care of it.

(define (emit.inits)
  (define (base file)
    (string-tr (string-drop-right (sys-basename file) 2) "-" "_"))
  (define files (sort-files-for-inits))
  
  (print (c-commenter "Automatically generated - DO NOT EDIT"))
  (print)
  (for-each (lambda (file)
              (print #`"extern void Scm_Init_,(base file)(ScmModule*);"))
            files)
  (print)
  (print #`"void Scm_Init_gtk_lib(ScmModule *mod)")
  (print #`"{")
  (for-each (lambda (file)
              (print #`"  Scm_Init_,(base file)(mod);"))
            files)
  (print #`"}"))

;; Sort files so that classes are initialized in the right order.
(define (sort-files-for-inits)
  (let* ((files&defs (get-files&definitions))
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
            table))
         )
    (reverse (topological-sort (hash-table-map dependency-table cons)))
    ))

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
          (report #`"  no change in ,file")
          (sys-unlink tmpfile))
        (begin
          (report #`"  writing ,file")
          (sys-rename tmpfile file)))))

(define (emit-all)
  (with-output-to-file-if-changed "gtk-lib.types"
    (lambda ()
      (emitter emit.types s-commenter)
      ;; some additional stuff
      (for-each (lambda (t) (write t) (newline)) *predefined-types*)))
  (with-output-to-file-if-changed "gtk-lib.h"
    (lambda () (emitter emit.h c-commenter)))
  (for-each-source-file
   (lambda (file defined)
     (receive (extra normal)
         (partition (cut is-a? <> <extra-stub>) defined)
       (with-output-to-file-if-changed
          #`",(string-drop-right (sys-basename file) 2).stub"
         (lambda ()
           (print (s-commenter "Automatically generated - DO NOT EDIT"))
           (print)
           (write '(include "gtk-lib.types"))
           (print)
           (write "#include \"gauche-gtk.h\"")
           (print)
           (print)
           ;; need to emit <extra-stub> first, for they may define
           ;; static fns.
           (for-each (cut emit.stub <> s-commenter) extra)
           (for-each (cut emit.stub <> s-commenter) normal))))))
  (with-output-to-file-if-changed "gtk-lib.inits" emit.inits)
  )

;;================================================================
;; DRIVER
;;

(define (parse-gdk)
  (parse-headers #`",(gtk-directory)/gdk"
                 (call-with-input-file "GDKFILES" port->string-list)))

(define (parse-gdk-pixbuf)
  (parse-headers #`",(gtk-directory)/gdk-pixbuf"
                 (call-with-input-file "GDKPIXBUFFILES" port->string-list)))

(define (parse-gtk)
  (parse-headers #`",(gtk-directory)/gtk"
                 (call-with-input-file "GTKFILES" port->string-list)))

(define (parse-pango)
  (parse-headers #`",(pango-directory)/pango"
                 (call-with-input-file "PANGOFILES" port->string-list)))

(define (run-through)
  (report "Parsing ...")
  (parse-pango)
  (parse-gdk)
  (parse-gdk-pixbuf)
  (parse-gtk)
  (report "Fixing up ...")
  (fixup)
  (report "Generating ...")
  (emit-all))

(define (main args)
  (run-through)
  0)
