(define-module h2s.parse
  (export
   parse-headers
   )
  (use h2s.objects)
  (use h2s.gtk-types)
  (use h2s.utils)

  (use h2s.track)                     ;for the inpupt-file param!

;  (use macros.aif)                      ;fixme!
  (use gauche.parameter)
  (use file.util)
  (use srfi-13)
  (use srfi-11)

  (use srfi-2)
  )
(select-module h2s.parse)

(define-syntax aif (syntax-rules () [(_ . args) (if-let1 . args)]))

(define debug #f)

;; we canonize the C definition into:   type******  var-name
(define (grok-vardef type name)
  (if debug
      (if (and (pair? type)
               (not (eq? (car type) 'const)))
          ;; this is only possible from function arguments:  search for acons:  const XXX *YYYY
          (logformat-color 87 "grok-vardef: type ~a\n" type)
        ))
  (let*-values (((ptrs var) (let1 brk (string-skip name #\*)
                              (values (string-take name brk)
                                      (string-drop name brk))))
                ((typesig)  (if (pair? type)
                                #`",(car type)-,(cdr type),|ptrs|" ;mmc:  ???
                                #`",|type|,|ptrs|")))
    ;;
    (if debug (logformat "grok-vardef: type: ~a\n" (string->symbol typesig)))
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



;;================================================================
;; PASS 1 - PARSER
;;

;; mmc: Q does the parse phase create the tree, the relation between types?
(define (parse-headers dir hlist)
  (for-each (lambda (hdr) (parse-header (build-path dir hdr))) hlist))
;;   |
;;   |
;;   V
(define (parse-header filename)
  (parameterize ((input-file filename))
    (if debug (report #`"*** parsing ,filename"))
    (with-input-from-file filename parse-body)))
;;       |
;;       |
;;       V
(define (parse-body)
                                        ;; mmc: why is (parse-body) not outside of rxmatch-case ?
    (let step ((line (read-line)))
      (rxmatch-case line
    (test eof-object?)
    (#/^struct (_G[dt]k\w+)/ (#f name) (parse-struct name) (parse-body))
    (#/^struct (_Pango\w+)/  (#f name) (parse-struct name) (parse-body))
    (#/^struct (_Glade\w+)/  (#f name) (parse-struct name) (parse-body))
    (#/^struct (_GnomeCanvas\w+)/  (#f name) (parse-struct name) (parse-body))

    (#/^struct (_Eog\w+)/  (#f name) (parse-struct name) (parse-body))
    ;; only conveniently named are recognized. Others might confuse us!!!
    ;; Also, note that it must start at bol.
    ;; and no way to combine  typedef struct _a{....}  a; !!!  Must be separate.


    (#/^PANGO_AVAILABLE_IN_/ ()        (parse-body))
    ;; this new addition of preprocessor symbols in Pango interferes with
    ;; parsing.  we just skip.

    (#/^typedef enum/        ()        (parse-enum) (parse-body))

    ;; mmc:
    ;; typedef GtkTextBuffer           GtksourceviewTextBuffer;
    (#/^typedef\s+(\w+)\s+(\w+)\s*\;$/        (#f old new)
      (unless (string-suffix? "Class" old) ;new?
        (copy-type! old new))
      (parse-body))



    ;; fixme:  "const GSlist" functions !
    (#/^(?:const\s+)?([\w\*_]+)\s+((\*+)\s*)?((g[dt]k|pango|glade|gnome_canvas|eog)_[\w_]+)\s*\((.+)$/ (#f ret #f ptr fn #f rest)
        ;; fixme:  const -> i could recycle the memory!
       (parse-function (if ptr #`",|ret|,|ptr|" ret) fn rest)
;; fixme: (parse-body)
       (step (read-line)))


    ;; typedef struct _GdkDrawable           GdkPixmap;
    ;; typedefa struct _a  a;    is ignored!!
    (#/^typedef\s+struct\s+(\w+)\s+(\w+)\s*\;$/ (#f struct-name type-name)
       (unless (string=? struct-name (string-append "_" type-name))

         (if debug (logformat-color 172 "new alias for the struct ~a: ~a\n" struct-name type-name))
         (copy-type! (string-drop struct-name 1) type-name)  ;; fixme:  why did I comment it out?
         ;; later!
         '(let1 old-type (find-type (string-drop struct-name 1))
           (set! (body-of
                  (find-type-or-create (string->symbol type-name)))
                 ;; struct !!
                 (body-of old-type))))
       (parse-body))

  ;;  workaround for /usr/include/gtkextra-2.0/gtkextra/gtksheet.h
  ;;  GtkWidget *
  ;;  gtk_sheet_new_browser                     (guint rows, guint columns, const gchar *title);
  ;;
  (#/^(?:const\s+)?([\w\*_]+)\s*(\s+(\*+))?$/ (all ret pointers)
   ;; append the second line and see:
  (if debug (logformat "another line read to form a parsing unit\n"))
  (step (string-append line " " (read-line))));; (string-drop-right line 1)

;; fixme:  this doesn't skip over multi-line `fat';
    (#/^.*$/ (all)
     ;;else
     (if debug (logformat "non-matched line: ~a\n" all))
     (parse-body)))
))


(define (copy-type! old new)
  ;; Shiro made this hack:  (& i try to extend it)
  ;; GdkWindow
  ;;(set! (body-of (find-type n)) (body-of gdkdrawable))

  ;; body is important for:  `get-slot-boxer'
  (if debug (logformat-color 196 "copy-type!: ~a -> ~a\n" old new))
                                        ;(sys-exit 0)
  (if (find-type new #t)
      (logformat-color (color* yellow) "copy-type!: ~a has already been defined\n" new)
                                        ;(logformat-color '(5 5 1) "copy-type!: ~a ~a\n" old new) ;yellow '(1 1 1)gunichar

    (and-let* ((c-name old) ;; (string-drop name 1) drop preceding '_'
               (scmname (mixed-case-name->hyphenated-name c-name)))
                                        ;((logformat-color (color* green) "scm name of the old type: ~a\n" scmname)))
      (cond
       ;; gtk doesn't distinguish it. i can/do for now.
       ;;  so, i don't want to rely on the Glib type system, hmmmmm.
       ;;  SCM_MAKE_GTK_SOURCE_MARKER -> SCM_GOBJECT_BOX -> ScmObj Scm_MakeGObject(void *obj) -> g_object_get_qdata

       ;; i would need, that SCM_MAKE_GTK_SOURCE_MARKER added a tag to tell more:
       ;;  or
       ;;  ignore the distinction:  then the source_marker functions should `just' accept the more general type.

       ;; that is: everywhere i use the stricter type, i should output the general.

       ((find-struct (string->symbol #`"<,|scmname|>"))
        => (lambda (old-struct)
             ;;
             (if #t
                 ;; version 2
                 (let* ((old-type (find-type (string->symbol (string-append old "*")) #t))
                        (new-type-name (string->symbol (string-append new "*")))
                        )
                   (aif new-type (find-type new-type-name)
                        (begin
                          ;; fixme:
                          (slot-set! new-type 'body
                            (body-of old-type)))


                                        ;(set! new-type
                     (make <gtk-type-alias> :c-name
                           :alias old-type))
                   ;; string-append
                                        ;(logformat-color (color* green) "\tit is a struct ~a\t type ~a found\n" #`"<,|scmname|>"
                                        ;                 (if old-type "" "NOT"))
                   ;; (slot-set! new-type 'alias old-type)

                                        ;(logformat "...so we created a <gtk-type-alias> c-named: ~a\n" (c-name-of new-type))
                   1;new-type
                   )
               ;; version 1
               (begin
                 ;; fixme: base types:  uint & etc
                                        ;(logformat-color (color* green) "\tit is a struct ~a\n" #`"<,|scmname|>")
                 ;; i could even make it inherit.
                 (make-struct (string-append "_" new) (fields-of old-struct))))))
       ;;
       ((find-type (string->symbol old) #t)
        =>
        (lambda (old-type)
                                        ;body (body-of self)
                                        ;(symbol? body)
          (set! (body-of
                 (find-type-or-create (string->symbol new)))
                (body-of old-type))
                                        ;(make <gtk-type> :c-name (car entry) :body (cadr entry))
                                        ;(make <gtk-type> :c-name (string->symbol new))
          ))
       ;;
       (else
        (logformat-color 'red "PROBLEM: copy-type! from ~a but that type is unknown now!" old)))


      (let1 old-type (find-type (string->symbol old) #t)
        (if old-type
            (set! (body-of
                   (find-type-or-create (string->symbol new)))
                  (body-of old-type))))

      (let1 old-type (find-type (string->symbol (string-append old "*")))
        (when old-type
          (make <gtk-type> :c-name (string->symbol (string-append new "*"))
                :body (body-of old-type))
          (logformat-color 196 "(typedef) copied the body into: from ~a\n" old-type))))))


;;  collect the slots and the call .... `make-struct'
;; what are the slots:
;;
;;
;;  (   (  ) symbol ..... )
;;
;;
(define (parse-struct name)
  (define (err-eof) (errorf "EOF while parsing struct ~s" name))
  (define (remove-gseal line) ;; an ad-hoc stuff to remove GSEAL macro.
    (regexp-replace #/GSEAL\s*\(([^\)]+)\)/ line "\\1"))

  (define (parse-struct-body)
    (let loop ((line (read-line))
               (fields '()))
      (rxmatch-case (remove-gseal line)
        (test eof-object? (err-eof))
        ;; empty
        (#/^\s*$/ () (loop (read-line) fields))

        ;; G style  wants them on separate line!?
        (#/^\{/ () (loop (read-line) fields))
        (#/^\}/ () (make-struct name (reverse fields))) ;mmc: exit here!

        (test has-comment? (skip-comment line (cut loop <> fields) err-eof))


        ;;  a_b_*c*           a*b*c_ef[abcde_dl]    :  3;
                                        ;(#/^\s+([\w\*_]+)\s+([\w\*_]+)(\[([\w_]+)\])?\s*(:\s*\d+\s*)?\;/

        ;;       a_b***       **a_b     [a_b]          : 23:
        (#/^\s+([\w_]+\**)\s+(\**[\w_]+)(\[([\w_]+)\])?\s*(?::\s*\d+\s*)?\;/
               (match type var array bits) ;
               ;; #f
               ;; mmc: i see it (#f type var array bits)      bug?
               (if (and (not array)
                        bits)
                   (logformat-color 123 "mmc: bits: ~a\n" bits))
               (if array
                   (begin
                     (logformat-color 197 "mmc: found C array: ~a\n\tline was: ~a\n" array line)
                     (loop (read-line) (cons (grok-arraydef type var bits) fields)))
                 (loop (read-line) (cons (grok-vardef type var) fields))))

        ;; the original:
        (#/^\s+([\w\*_]+)\s+([\w\*_]+)(\[([\w_]+)\])?\s*(:\s*\d+\s*)?\;/
               (#f type var #f array)
               (logformat-color 123 "mmc: your regexp is wrong!\n~a\n" line)
               (sys-exit 1))

        ;; function pointer - `ignore!'   But the function cannot be used then! ... err. here we are parsing Struct!
        (#/^\s+[\w\*_]+\s+\**\([\w\*_]+\)(.*)/ (#f rest)
               (let loop2 ((line rest))
                 (rxmatch-case line
                   (test eof-object? (err-eof))
                   (#/\)\;/ () (loop (read-line) fields))
                   (else (loop2 (read-line))))))

        (#/^\s+([\w\*_]+)\s+([\w\*_]+)\s*,(.*)$/ (#f type var rest)
               ;; something like:  int x,y,
               ;;                      z,w;
               (let loop2 ((rest rest)
                           (fields (cons (grok-vardef type var) fields)))
                 (rxmatch-case rest
                   (#/\s*([\w\*_]+)\s*\;/ (#f var)
                         (loop (read-line) (cons (grok-vardef type var) fields)))
                   (#/\s*([\w\*_]+)\s*,(.*)$/ (#f var rest)
                         (loop2 rest (cons (grok-vardef type var) fields)))
                   (else (warn "~s in ~a" line name) (loop (read-line) fields)))))

        (else (warn "~s in ~a" line name) (loop (read-line) fields)))))

  (define (skip-struct name)
    (if debug (logformat-color 200 "Skipping a class/interface struct ~a.\n" name))
    (let loop ((line (read-line)))
      (rxmatch-case line
        (test eof-object? (err-eof))
        (#/^\{/ () (loop (read-line)))
        (#/^\}/ () '())
        (else (loop (read-line))))))

  (if debug (logformat-color 190 "parsing struct ~a\n" name))
  ;; we are not interested in these !!!  What do they do?  keep gobject `methods' only?
  (if (string-suffix? "Class" name)
      (skip-struct name)
    (parse-struct-body)))

;; mmc: we have already thrown the name ? No the C enum is  enum {....} _Name_;
(define (parse-enum)
  (define (err-eof) (error "EOF while parsing enum"))
  (let loop ((line (read-line))
             (enums '()))
    (rxmatch-case line
      (test eof-object? (err-eof))
      (#/^\{/ () (loop (read-line) enums))
      (#/^\}\s*([\w_]+)/ (#f name) (make-enum name (reverse enums))) ; ----->
      ;; empty line:
      (#/^\s*$/ () (loop (read-line) enums))
      ;; preprocessor directive:
      (#/^\s*#\s*\w+/ () (loop (read-line) enums))
      ;;
      (test has-comment? (skip-comment line (cut loop <> enums) err-eof)) ;  what if?:      new_value,  /* boring comment*/
      (#/\s+([\w_]+),?/ (#f enum)
        (loop (read-line) (cons enum enums)))
      (#/\s+([\w_]+)\s+=/ (#f enum)     ;   name  = ...whatever... we are not interested in that detail!
        (loop (read-line) (cons enum enums)))
      (else (warn "~s in enum" line) (loop (read-line) enums))
      )))

;; return-type  name-including-* (pointers!)  rest is rest-of-line, i.e. the arguments, after the "("
(define (parse-function ret name rest)
  (define (err-eof) (errorf "EOF while parsing function ~s" name))


  (define (grok-arg argstr)             ; "int a"
    (rxmatch-case (string-trim-both argstr) ; remove spaces at extremes


      (#/^const\s+(.+)$/ (#f rest)
        (let1 r (grok-arg rest) (acons 'const (car r) (cdr r)))) ;(acons 1 2 3) -> ((1 . 2) . 3)


      (#/^([\w\*_]+)\s+([\w\*_]+)$/ (#f type name) (cons type name))
      ;; function:      ,  int (f)(int*, char*)   !!!
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
                                     (if debug (logformat "arg: ~a\n" a))
                                     (if (null? a)
                                         a
                                         (grok-vardef (car a) (cdr a))))) ; ???
                                 rargs))))
  (if debug (logformat-color 245 "parsing functions ~a\n" name))

  (let loop ((line rest)                ;First line
             (args '()))
    (rxmatch-case line
      (test eof-object? (err-eof))
      ;;empty line
      (#/^\s*$/ () (loop (read-line) args))
      ;;
      (test has-comment? (skip-comment line (cut loop <> args) err-eof)) ;Is this the reason we cannot invoke loop after rxmatch-case?
      ;;
      (#/\s*([^,]+),(.*)/ (#f arg rest) ; get XXX XXX, <--rest--->
        (loop rest (cons arg args)))
      ;;
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
          (else (loop (read-line)))))))))



(provide "h2s/parse")
