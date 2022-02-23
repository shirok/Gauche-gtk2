;; mmc:    by the means of a parameter,  we record a value (filename) for each created (derived) object.
;;         Then we can get a list of objects (in the right order) for each (used) value.


;; consumers should keep the parameter  `input-file',
;; and generate the derived objects.
;;   we keep a class-wide (per derived ?)  alist of (file . object) from which ...

;; in the end, it can use:
;; `for-each-source-file'




;; fixme: i don't like this?
(define-module h2s.track
  (export
   <source-tracker-mixin>
   get-files&definitions
   for-each-source-file
   input-file
   )
  (use gauche.parameter)
  (use gauche.mop.instance-pool)
  (use h2s.utils)
  )
(select-module h2s.track)




(define-class <source-tracker-mixin> ()
  ((source-file)                        ;:accessor source-file-of
   (files&definitions :allocation :class :initform '())))

(define-method initialize ((self <source-tracker-mixin>) initargs)
  (next-method)
  ;(logformat "initialize <source-tracker-mixin>: ~s->~s\n" initargs (ref self 'c-name))
  (let1 file (sys-basename (input-file)) ;mmc: parameter
    ;; buggy! (set! (source-file-of self) file)
    (slot-set! self 'source-file file)

    (let1 p (assoc file (slot-ref self 'files&definitions)) ; we have this global alist   filename -> list of definitions
      (if p
          (push! (cdr p) self)
        (slot-push! self 'files&definitions (list file self)))))
  ;(logformat "initialize <source-tracker-mixin>: ~s->~s\n" initargs (ref self 'c-name))
  self)

;; call proc with source file name and a list of objects defined in that file.
(define (for-each-source-file proc)
  (for-each (lambda (def-list)
              ;;     filename        definitions. in order of appearance in the file!!
              (proc (car def-list) (reverse (cdr def-list))))
    ;; mmc:
    (get-files&definitions)
    ;(reverse (class-slot-ref <source-tracker-mixin>
    ;                         'files&definitions))

    ))

(define (get-files&definitions)
  (reverse (class-slot-ref <source-tracker-mixin>
                           'files&definitions)))

;; Keeps input file name during parsing
(define input-file (make-parameter #f))


(provide "h2s/track")
