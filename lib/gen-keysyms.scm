;;
;; Generates gdk/gdkkeysyms.scm from gdk/gdkkeysyms.h
;;

(use gauche.process)
(use srfi-13)

(define (main args)
  (let1 prefix (process-output->string "pkg-config --variable=prefix gtk+-2.0")
    (when (string-null? prefix)
      (error "can't get the gtk+-2.0 install location.  pkg-config problem?"))
    (with-output-to-file "gtk/gdkkeysyms.scm"
      (lambda ()
        (print ";; Automatically generated")
        (print "(select-module gtk)")
        (with-input-from-file #`",|prefix|/include/gtk-2.0/gdk/gdkkeysyms.h"
          (cut port-for-each filter read-line))
        (print "(provide \"gtk/gdkkeysyms\")"))))
  0)

(define (filter line)
  (rxmatch-if (rxmatch #/^#define\s+([\w_]+)\s+0x([\w]+)/ line)
      (#f name value)
    (print #`"(define-constant ,|name| #x,|value|)")
    #f))


        
  
