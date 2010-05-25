

;;  functions to be used in the `fixup' stage:
;;  to modify the gtk objects generated in the parser,
;;   

(define-module h2s.fixup
  (export fixup)
  (use h2s.gtk-types-for-fixup)
  (use h2s.utils)
  (use h2s.track)

  (use h2s.objects)
  (use srfi-13)
  (use srfi-2)
  (use srfi-1)
  (use text.tr)


  (use gauche.parameter)
  )

(select-module h2s.fixup)


(define debug #f)

;;================================================================
;; PASS 2 - FIXUPS
;;

;; Load "hints" files
(define (load-hints hint-files)
  (parameterize ((input-file "hints.h")) ;;dummy
    (for-each (lambda (file)
                (when (file-exists? file)
                  (report #`"  loading ,file")
                  (with-error-handler
                      (lambda (e)
                        (logformat "Error in ~a ~a:\n~a\n" file (input-file) (ref e 'message))
                        (sys-exit -1))
                    (lambda ()
                      (load file :environment (current-module))))))         ; eval as scheme! good.
      hint-files)))

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
     (set! (return-type-of self) #f)    ;!!!
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
;; mmc: So this macro is not hygienic, and it modifies the `self' variable!
(define-macro (add-mixin! . c-mixin-names)
  ;; what if cpl-of fails?
  ;; 
  `(begin
     (if (null? (cpl-of self))
         (logformat "add-mixin! would fail on ~a\n" self)
       (set! (direct-supers-of self)
             (list ,@c-mixin-names (car (cpl-of self)))))
     (set! (cpl-of self)
           (list* ,@c-mixin-names (cpl-of self)))))

;; adds opaque GObject.

;; Fixme: This macro is used in the .hint files! To mark 
(define-macro (define-opaque c-name type) ; :indirect or :gobject 
  `(make-opaque ',c-name ,type))

(define (make-opaque c-name type)
  (let* ((struct (make-struct #`"_,|c-name|" '())))
    (case type
      ((:gobject)
       (set! (allocation-type-of struct) 'gobject)
       (set! (superclass-of struct) (find-type 'GObject)) ; fixme: GObject* ?
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



;; mmc:  haha!!! so this makes the .hints file a program! Compare w/ .stub files, which have partially(?) the _same_ syntax/commands!

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
  `(make <extra-stub> :body '(define-type ,@args) :type? #t)) ;  this is special!   this goes to a different file! the central type repo.
(define-macro (raw-code . args)
  `(make <extra-stub> :body (string-join ',args "\n" 'suffix) :type? #f)) ;mmc: bug: \n is not ok. 

;; figure out what implemenation type each structure is,
;; by examining its first field.

;;
(define-method set-superclass ((self <gtk-struct>))
  (if debug (logformat-color 118 "set-superclass: ~a\n" (c-name-of self)))
  (if (slot-bound? self 'superclass)
      (begin
        (if debug (logformat "we already know: ~a\n" (ref self 'superclass)))
        (superclass-of self))

    ;; 
    (receive (superclass gobject)
        (if (null? (fields-of self))
            (values #f #f)
          (let ((first-slot-type (type-of (car (fields-of self)))))
            ;;<------  the specifics of the  G-object inheritance system
            (cond
             ((memq (c-name-of first-slot-type) '(GObject GInitiallyUnowned))
              ;;(eq? (c-name-of first-slot-type) 'GObject) ; `top'
              (logformat-color 111 "\tfirst-slot-type: ~a\n" first-slot-type)
              (values
               ;; mmc: fake:  (was first-slot-type)
               (find-type 'GObject)
               #t))

             ;; mmc: why do we look for the pointer-type ?
             ;; why do we want the pointer type??         .... b/c we have only those!
             ((and-let*
                ((ptrname (string->symbol #`",(c-name-of first-slot-type)*")) 
                 (ptrtype (find-type ptrname))
                 ;;mmc: todo! todo! todo! todo! todo! todo! todo! todo! todo! todo! todo!
                 ((logformat "trying ~a\n" ptrtype))
                 ((is-a? (body-of ptrtype) <gtk-struct>))
                 ((set-superclass (body-of ptrtype)))) ; recurse!!! the structure?

                (if debug (logformat-color 51 "\t\tfound superclass: ~a: gobject? ~a\n"
                            (c-name-of ptrtype)
                            (gobject? (body-of ptrtype))))
                (values ptrtype (gobject? (body-of ptrtype))))) ;and this is already determined ??? 
             (else
              (logformat-color 10 "\t\t sorry: cannot find superclass of ~a\n" self)
              (values #f #f)))))

      ;; now we have (superclass gobject)
      ;; 
      (when gobject
        (set! (allocation-type-of self) 'gobject)) ; do we do it in topological order ??   No: we recurse!!!
      (set! (superclass-of self) superclass)
      superclass)))

(define-method set-cpl ((self <gtk-struct>))
  (if debug (logformat-color 118 "set-cpl: ~a\n" (c-name-of self)))
  ;; after setting up superclass field of all structs, sets up CPL.
  ;; the hints file may modify CPL afterwards.
  (set! (cpl-of self)
        (let loop ((super (superclass-of self))
                   (classes '()))
          (logformat "set-cpl: ~a, cname: ~a\n" super (if super (c-name-of super) "#f"))
          (cond ((not super)
                 (reverse classes))

                ((eq? (c-name-of super) 'GObject) ; ugly hack!
                 (reverse (cons "Scm_GObjectClass" classes))) ; ???

                ((eq? (c-name-of super) 'GdkEvent*)
                 (reverse (cons "Scm_GdkEventClass" classes)))
                (else (loop (superclass-of (body-of super))
                            (cons #`"Scm_,(c-name-of (body-of super))Class"
                                  classes)))))))

;; mmc: i would like an example of such data type/struct
;; i think the  old gdk-pixbuf used such thing.
(define-method set-refcounted ((self <gtk-struct>))
  (if debug (logformat-color 118 "set-refcounted: ~a\n" (c-name-of self)))
  ;; use heuristics to find out if self is a ref-counting object (and not an
  ;; GObject).           mmc: where do we avoid the GObject?
  (when (find (lambda (field) (equal? (c-name-of field) "ref_count"))
              (fields-of self))         ; it doesn't walk the inheritance tree !
    (set! (allocation-type-of self) 'refcounted)))


;; the last step of fixup. i.e before fixup-functions, and before reading .hints!
;; 
(define-method set-fields ((self <gtk-struct>))
  ;; scan <gtk-var>'s in the fields and sets up it's default setter and getter
  (define (set-field-getter-n-setter field)
    (let ((stub-type (scm-type-of (type-of field))))
      (cond
       ((eq? stub-type '<gchar*>)
        (set! (ref field 'getter)
              #`"return SCM_MAKE_STR_COPYING_SAFELY(obj->,(c-name-of field));")
        (set! (ref field 'setter)
              ;; Check if it is a string!!!
              #`"if (SCM_STRINGP(value)) {obj->,(c-name-of field) = Scm_GetString(SCM_STRING(value));};")
                                        ;Scm_GetStringConst(SCM_STRING(value))  fixme: this should use malloc, not GC_malloc_atomic!
        )                               ;mmc! was #f  mmc: what if it already has a value. Should i free it? fixme!



       ;; mmc:  what is it?   i would think it's symbol type ->
       ((not (pair? stub-type))         ; has proper stub type, so no need of g&s.
        ;; Basic types! <int> etc.
        (if debug (logformat-color 213 "unknown type of the slot: ~a ~a\n" (c-name-of field) stub-type))
        (set! (ref field 'getter) #t)
        (set! (ref field 'setter) #t))

       
       ((and-let* 
          ;; mmc: these don't even consider the scm-type!!
          ;; check if it is an embedded structure.
          ((ptrtype ;; we store as types pointers !!!  This is scheme!
            (find-type (string->symbol #`",(c-name-of (type-of field))*")))
                   
           (ptrbody (body-of ptrtype))  ;  can be symbol, struct, enum ...
           ;; why is this a condition ? What else ... symbol: char, int  what's wrong w/ it?
           ((is-a? ptrbody <gtk-struct>)))

          (set! (ref field 'getter)
                ;; getting the slot value and _immediately_ boxing it as the 
                #`"return ,(c-boxer-of ptrbody)(&(obj->,(c-name-of field)));")
          
          (set! (ref field 'setter)
                #`"obj->,(c-name-of field) = *,(c-unboxer-of ptrbody)(value);")

          (logformat-color 214 "slot ~a embedded: ~a\n" (c-name-of field) (c-name-of ptrtype))
          ))


       ( ;; check if it is an array reference.
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

          ;;  (UNKNOWN . XXXX)
          (set! (ref field 'setter) #f)))
       (else
        (set! (ref field 'accessible?) #f)))))
  (if debug
      (logformat-color 118 "set-fields: ~a\n" (c-name-of self)))

  (for-each set-field-getter-n-setter (fields-of self)))


;; mmc: ???
(define-method set-qualifier ((self <gtk-struct>))
  ;; sets GtkObject subclasses :base class, so that Scheme subclass can   !! mmc!!
  ;; be defined.
  (when (eq? (allocation-type-of self) 'gobject)
    (set! (qualifier-of self) :base)))  ;  mmc: what does this do?  -> genstub!



;; i have to know this well:
(define (fixup-structs)
  (logformat-color 118 "fixup-structs\n=========\n")
  
  ;; Special treatment : GdkBitmap*, GdkPixmap* and GdkWindow* are really
  ;; synonyms of GdkDrawable*.

  ;; mmc: this should be meat for me/aliases!! 
  ;; no more needed!
  '(let ((gdkdrawable (find-type 'GdkDrawable*)))
    (for-each (lambda (n)
                (set! (body-of (find-type n)) (body-of gdkdrawable)))
      '(GdkBitmap* GdkPixmap* GdkWindow*)))

  ;; GtkAllocation is an alias of GdkRectangle.
  (let ((gtk-allocation-type  (find-type-or-create 'GtkAllocation*))
        (gdk-rectangle-struct (find-struct '<gdk-rectangle>)))
    (set! (body-of gtk-allocation-type) gdk-rectangle-struct))

  ;; mmc: this should be meat for me/aliases!!  typedef 	GdkRectangle	   GtkAllocation;

  (for-each-instance set-superclass <gtk-struct>)
  (for-each-instance set-cpl        <gtk-struct>)
  (for-each-instance set-refcounted <gtk-struct>)
  (for-each-instance set-fields     <gtk-struct>))

(define (fixup-structs-after)
  ;; need to do this after loading hints, for the allocation type of
  ;; the struct may be modified in hints.
  (logformat "fixup-structs-after\n")
  (for-each-instance set-qualifier  <gtk-struct>))

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
    (set! (arguments-of self) '()))     ; mmc: '(()) -> '()  why is it needed.       in fact void?
  ;; Ignore 'const' qualifier (except const-char*)
  (for-each (lambda (arg)
              (let* ((type (type-of arg))
                     (typename (c-name-of type)))
                (when (and (not (memq typename '(const-char* const-gchar*)))
                           (string-prefix? "const-" (x->string typename))) ;mmc:!  typename  includes all of that!!!
                  (set! (type-of arg)
                        (find-type-or-create (string->symbol
                                              (string-drop (x->string typename) ; const- is 6 chars, hehe
                                                           6)))))))
            (arguments-of self)))

(define (fixup-functions)
  (for-each-instance set-internal <gtk-function>)
  (for-each-instance fix-arg <gtk-function>))

(define (fixup hint-files)
  (fixup-structs)
  (fixup-functions)
  (load-hints hint-files)
  (fixup-structs-after))



(provide "h2s/fixup")
