
;; concrete objects (not abstract types) which are  exchanged between
;;  parsing  &  emitting.

(define-module h2s.gtk-types
  (export

   ;; body-of |setter of body-of|
   c-name-of
   ;; needed in hint files?

   ;; implicitely exported?
   scm-name-of

   ;; this is `critical'
   ;; hopefully onle these needed for the fixup
   fields-of
   type-of
   superclass-of allocation-type-of
   gobject? cpl-of internal? 
   c-free-proc-of
   c-caster-of
   allocator-of

   
   ;; emiter:
   scm-class-name-of
   gtk-predicate-of
   gtk-type-name-of
   c-copy-proc-of
   qualifier-of
   c-type-of
   c-predicate-of
   c-predicate-nullable-of
   c-class-macro-of
   
   ;; needed by hint too.
   direct-supers-of


   ;; fixup:
   element-type-of size-of
   
   ;; needed by fixup & emit!
   c-boxer-of c-unboxer-of

   ;; fixup:
   arguments-of return-type-of
   ;; body-of

   <gtk-var>                            ; var ???
   accessible?

   ;; only emitter:
   getter-of
   setter-of

   <gtk-enum> make-enum
   find-enum
   values-of ;; c-name-of
   <gtk-struct>
   find-struct make-struct
   <gtk-array>

   <gtk-function>
   find-function make-function

   <extra-stub>
   ;; emiter:
   type?
   print-body

   ;scm-type-of
   get-slot-boxer

   gtk-base
   )

  (use gauche.mop.instance-pool)
  (use h2s.objects)
  (use h2s.track)

  (use h2s.utils)
  (use h2s.mmc-utils)
  (use srfi-13)
  (use srfi-2)
  (use text.tr)
  )
(select-module h2s.gtk-types)

(define debug #f)
(define gtk-base #t)                    ;backward compatible-> #t!

;; <GTK-VAR> - used in fields and arguments
(define-class <gtk-var> ()
  ((type      :init-keyword :type   :accessor type-of)
   (c-name    :init-keyword :c-name :accessor c-name-of)
   (scm-name  :allocation :virtual :accessor scm-name-of
              ;; what is this?
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




;; <GTK-STRUCT>
(define-class <gtk-struct> (<instance-pool-mixin>
                            <source-tracker-mixin>)
  ((c-name    :init-keyword :c-name :accessor c-name-of)
   (fields    :init-keyword :fields :accessor fields-of)

   (internal?   :init-value #f :accessor internal?) ;mmc:  typedef encountered, but no function signature contains it?
   ;; - true if this struct is not exposed to Scheme.  set `only' by fixup.... yes, by .hint files! `disable-cclass'
   ;; <gtk-cell-layout-iface> <gtk-cell-layout>


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



   (c-free-proc :accessor c-free-proc-of :init-value #f) ; example:    `gtk_tree_row_reference_free' gtk_tree_path_free
   ;; - Used by `indirect' struct, keeping C procedure name to free
   ;;   the data part.  can be set in hints file.
   (allocator      :init-form #f :accessor allocator-of) ; mmc: is this only relevant for the consturctor/destructor?
   ;; - Special allocator setting that overrides the default.
   ;;   May be set by hints file.   This can be a string for
   ;;   entire allocator body, or an assoc-list of required
   ;;   initargs and the constructor to call.
   (qualifier      :init-value :built-in :accessor qualifier-of) ; mmc:  ??  :base -> this means   see `genstub'!
   ;; - define-cclass qualifier.  adjusted in fixup.
   (direct-supers  :init-value () :accessor direct-supers-of)
   ;; - extra direct-supers if this class has a mixin.              mmc:  interfaces! + the parent

   ;; mmc:
   (printer :init-keyword :printer)
   ))

(define-method write-object ((self <gtk-struct>) port)
  (format port "#<gtk-struct ~s>" (c-name-of self)))

(define-method gobject? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'gobject))

(define-method indirect? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'indirect))

(define-method refcounted? ((self <gtk-struct>))
  (eq? (allocation-type-of self) 'refcounted))


;; we don't look at the:   typedef a struct _a;
;; This only creates the various default names/stubs
;; for C macros
;; Creates the type!
;; mmc: i have something similar in 
(define (make-struct name fields)
  (if debug (logformat-color 157 "make-struct ~a FIELDS:\n~a\n" name fields))
  (let* ((c-name (string-drop name 1))   ;; drop preceding '_'
         (s (make <gtk-struct> :c-name (string->symbol c-name) :fields fields))
         (tn (find-type-or-create (string->symbol #`",|c-name|*")))
         ;; ^^^ really, this is `create'
         (scmname (mixed-case-name->hyphenated-name c-name)))


    ;; mmc: so, the type of struct _abc_x  is abc_x*  ??
    
    (set! (c-type-of s) tn)             ; <gtk-type>   <--->  <gtk-struct>
    (set! (body-of tn) s)

    (set! (scm-class-name-of s) (string->symbol #`"<,|scmname|>"))
    (let1 base (string-tr scmname "a-z-" "A-Z_")
      (set! (c-caster-of s) base)       ;mmc:  ??
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
         (values #`",(string-take base 6)IS_,(string-drop base 6)" ;  mmc:  why 6? pango_ ?  
                 #`",(string-take base 6)TYPE_,(string-drop base 6)"))

        ((and gtk-base
              (string-prefix? "GTK" base))  ;; either GDK_ or GTK_
         (values #`",(string-take base 4)IS_,(string-drop base 4)"
                 #`",(string-take base 4)TYPE_,(string-drop base 4)"))
        ;; wrong!!!  GLADE_XML  ->  GLAD TYPE_E_XML
        (else
         (let ((prefix-len (+ 1 (string-scan base "_")))
               )
         (values
          (string-append
           (string-take base prefix-len) "IS_" (string-drop base prefix-len))
          (string-append
           (string-take base prefix-len) "TYPE_" (string-drop base prefix-len)))
         ;; mmc: i think it's   XXX_YYY -> XXXX_IS_YYYY ? and XXX_TYPE_YYY
         ;;find position of the left-first _
         )))))
    ;(logformat-color 157 "fields:\n\n" name fields)
    ;(describe s)
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

(define (find-enum c-name)
  (let1 c-name-as-symbol (string->symbol c-name)
    (instance-pool-find <gtk-enum>
                        ;; or keep a symbol of c-name & ...
                      
                        (lambda (s) (eq? c-name-as-symbol (c-name-of s))))))

(define (make-enum name values)         ;is in C the universe of Enums (their name) disjoint from that of Structs? or only in Gnome
  (let* ((n (string->symbol name))
         (s (make <gtk-enum> :c-name n :values values)))
    (set! (body-of (find-type-or-create n)) s) ; !!!!
    (if debug (logformat "enum ~a: ~a\n" n values))
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
  ;; print  doesn't include the \" in:
  ;; (define-type <gunichar> gunichar A type which can hold any UCS-4 character code SCM_CHARP Scm_char2gunichar Scm_gunichar2char)
  (write (body-of self))                ;mmc: was write !!! print ?
  (newline)
  (newline))

;;; from objects

;; map <gtk-type> to stub type signature.
(define-method scm-type-of ((self <gtk-type>))
  (let1 body (body-of self)
    (cond
     ((symbol? body) body)              ; basic types!
     ((is-a? body <gtk-enum>) '<int>)
     ((is-a? body <gtk-struct>) (scm-class-name-of body))
     ;; if the name of gtk-type is XX*, try the type XX.
     
     (else (cons 'UNKNOWN (c-name-of self))))))


;; returns a fn that creates a C code fragment of boxing slot value. mmc: was unboxing/  but it doesn't do it!
;; UGLY - this doesn't deal with array ref.
(define-method get-slot-boxer ((self <gtk-type>)) ;mmc: what slot???
  (let1 body (body-of self)
    (cond
                                        ; mmc: (lambda: x ->   scm_makeInteger(x)
     ((is-a? body <gtk-struct>)
      ;; so the type is a pointer!?
      (cut string-append (c-boxer-of body) "(" <> ")")) ;not cute ?
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
        ;; mmc: <gunichar> !! 
        (else #f)))
     (;; check if it is an embedded structure.
      (and-let* ((ptrtype
                  (find-type (string->symbol #`",(c-name-of self)*")))
                 (ptrbody (body-of ptrtype))
                 ((is-a? ptrbody <gtk-struct>)))

        (if debug (logformat-color 11 "embedded structure!\n"))
        (cut string-append (c-boxer-of ptrbody) "(&(" <> "))")))
     (else #f))))


(provide "h2s/gtk-types")
