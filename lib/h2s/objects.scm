
;;;mmc:  base class  for objects passed from parsing to emitting?
;;


(define-module h2s.objects
  (export
   <gtk-type>
   find-type
   find-type-or-create
   find-type-in-archive-function
   ;; accessors ... generics!!
   get-slot-boxer
   c-name-of
   scm-type-of
   body-of
   |setter of body-of|

    ;; useless?
   <gtk-type-alias>
   ;; late-commer: for <instance-pool-meta>
   for-each-instance
   ;;
   )

  (use gauche.mop.instance-pool)
  ;; utils?
  (use h2s.utils)
  )
(select-module h2s.objects)


(define debug #f)
; (define-generic body-of)
; (define-generic |setter of body-of|)
; (define-generic c-name-of)


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


;; this is a hack: we need acyclic dependency of modules.
;; But we need a hook from this, low level module, to call
;; function from a higher module (which looks-up in database).
(define find-type-in-archive-function #f)

(define (find-type name . rest)
  (or
   (instance-pool-find <gtk-type>
                      (lambda (item) (eq? (c-name-of item) name)))
   (if find-type-in-archive-function
       ;; could be a list!
       (find-type-in-archive-function name)
     (begin
       (if (null? rest)
           (logformat-color 10 "find-type: not found ~a\n" name))
       #f))))

(define (find-type-or-create name)
  (or (find-type name #t)
      (begin
        (if debug (logformat-color 10 "find-type-or-create ~a\n" name))
        (make <gtk-type> :c-name name))))  ;mmc: no body for now


;; mmc:
(define-class <gtk-type-alias> (<gtk-type>)
  ((alias :init-keyword :alias)))


;; get-slot-boxer depends only on the body? almost.
;; The embedded uses the c-name, to get at another type: the pointer one! So, we should keep a canonical type!
;; 
(define-method get-slot-boxer ((self <gtk-type-alias>))
  (get-slot-boxer (slot-ref self 'alias)))
  
(define-method scm-type-of ((self <gtk-type-alias>)) ;mmc:  this could be the alias name!
  ;(exit)
  (scm-type-of (slot-ref self 'alias)))


;; c-name-of   remains. b/c  find-type uses it :(
(define-method write-object ((self <gtk-type-alias>) port)
  (write-object (slot-ref self 'alias) port))




;;================================================================
;; CLASSES
;;

(define-method for-each-instance (proc (class <instance-pool-meta>)) ; mmc:  i would have thought this is standard
  (for-each proc (instance-pool->list class)))


(provide "h2s/objects")

