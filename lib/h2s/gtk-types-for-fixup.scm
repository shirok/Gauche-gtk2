
;; This is the Interface to the `h2s.gtk-types' module
;; provided to `h2s.fixup'

(define-module h2s.gtk-types-for-fixup
  (extend h2s.gtk-types)
  (export

   c-name-of
   scm-name-of
   
;    scm-class-name-of
;    gtk-predicate-of
;    gtk-type-name-of
;    c-copy-proc-of
;    qualifier-of
;    c-type-of
;    c-predicate-of
;    c-predicate-nullable-of
;    c-class-macro-of



   ;; needed by fixup & emit!
   c-boxer-of c-unboxer-of


   ;; only emitter:
;   getter-of
;   setter-of

   
   <gtk-enum> make-enum
   values-of ;; c-name-of
   <gtk-struct>
   find-struct make-struct
   <gtk-array>


   <gtk-function>
   find-function make-function
   ;;
   <extra-stub>
   ;; emiter:
   type?
   print-body
   )
  
  )
(select-module h2s.gtk-types-for-fixup)


(provide "h2s/gtk-types-for-fixup")