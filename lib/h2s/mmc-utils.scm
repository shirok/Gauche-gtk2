;;; The last pieces originally used from `gauche-mmc', here a local version:
;; todo: does logformat conflict with Shiro's version?

(define-module h2s.mmc-utils
  (export
   logformat
   logformat-color                      ; fixme!
   s+
   eof-object
   )
  )
(select-module h2s.mmc-utils)

(define s+ string-append)

(define (logformat . args)
  (apply format (current-error-port) args))


(define eof-object (read (open-input-string "")))


;; quick fix:
(define (logformat-color color . args)
  (let ((text (apply format #f args)))
    (cond
     ((sys-getenv "INSIDE_EMACS")
      (display text (current-error-port)))
      ;; xterm:
     (else
      (display text (current-error-port))
      #;(cdisplay (current-error-port)
                color text))
     )))


(provide "h2s/mmc-utils")
