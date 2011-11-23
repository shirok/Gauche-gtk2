#! /usr/bin/gosh

;; usage:
;;  h2stub.scm  -i file -o file.db


(use h2s.top)
(use h2s.parse)

;; Why was it used?  (use gauche.parameter)

;; special:
(use file.util)
(use srfi-1)
(use srfi-37)
(use gauche.process)


;; Gtk and Pango version (major.minor) we're dealing with
(define gtk-version "2.0")
(define pango-version "1.0")

;; Directories to search input header files.
;; We first try pkg-config.  If it fails, does
;; heuristic search.
;; mmc: this needs a fix for different libs
(define *header-search-paths*
  (guard (e [(<process-abnormal-exit> e)
             (exit 1 "Couldn't find necessary libraries; make sure you have \
                      development enviroment for gtk+-~a and pango-~a."
                   gtk-version pango-version)])
    (delete-duplicates
     (append (process-output->string-list
              `(pkg-config --variable=includedir ,#`"gtk+-,|gtk-version|"))
             (process-output->string-list
              `(pkg-config --variable=includedir pango))
             '("/usr/include" "/usr/local/include"))
     string=?)))

(define (find-header-dir target paths)
  (cond ((find-file-in-paths target
                             :paths paths
                             :pred file-is-readable?)
         => (lambda (p) (sys-dirname (sys-dirname p))))
        (else 
         (error #`",|target| couldn't find in ,*header-search-paths*"))))

(define gtk-directory (find-header-dir #`"gtk-,|gtk-version|/gtk/gtk.h"
                                       *header-search-paths* ))
(define pango-directory
  (find-header-dir #`"pango-,|pango-version|/pango/pango.h"
                   *header-search-paths*))
(define gdk-pixbuf-directory
  (guard (e [(<error> e)
             gtk-directory])
    (find-header-dir #`"gdk-pixbuf-,|gtk-version|/gdk-pixbuf/gdk-pixbuf.h"
                     *header-search-paths* )))


(define (parse-gdk)
  (parse-headers #`",|gtk-directory|/gdk"
                 (call-with-input-file "GDKFILES" port->string-list)))
(define (parse-gtk)
  (parse-headers #`",|gtk-directory|/gtk"
                 (call-with-input-file "GTKFILES" port->string-list)))

(define (parse-pango)
  (parse-headers #`",|pango-directory|/pango"
                 (call-with-input-file "PANGOFILES" port->string-list)))

(define (parse-gdk-pixbuf)
  (parse-headers #`",|gdk-pixbuf-directory|/gdk-pixbuf"
                 (call-with-input-file "GDKPIXBUFFILES" port->string-list)))

;; defaults for the gauche-gtk package:
(define gtk-recipe
  (make <parse-2-stub>
    :hint-files '("./pango-lib.hints"
                     "./gdk-lib.hints"
                     "./gtk-lib.hints")
    :header-file  "gtk-lib.h"     ; declaration of Scm classes! pointers ....
    :include-file "gauche-gtk.h"  ; where do we use this... in some of our C templates!
    :types-file "gtk-lib.types"   ; repo

    :inits-file "gtk-lib.inits"   ; a list of funcitons to call at module loading

    :init-function "void Scm_Init_gtk_lib(ScmModule *mod)"

    :parsing-function
    (lambda ()
      (parse-pango)
      (parse-gdk)
      (parse-gdk-pixbuf)
      (parse-gtk))))


(define (main args)
  (let ((input #f)
        (output "gtk.db"))       ;  (let1 filename (string-append (sys-tmpnam) ".db")
    (let* ((options
            (list
             (option '(#\i "input") #t #t ;or numero??
               (lambda (option name arg seed)
                 ;; fixme:  a parameter!!!
                 (set! input arg)
                 1))

             (option '(#\o "output") #t #t ;or numero??
               (lambda (option name arg seed)
                 ;; fixme:  a parameter!!!
                 (set! output arg)
                 1)))))

      (args-fold (cdr args)
        options
        (lambda (unrecognized name arg seed)
          (logformat "unrecognized: ~a\n" name)) ;unrecognized
      
        (lambda (operand seed)
          ;(push! operands operand)
          (error "unexpected argument" operand))
        ;; seeds:
        #f)
      
      (standard-parse-n-emit gtk-recipe input output)
      (sys-exit 0))))
