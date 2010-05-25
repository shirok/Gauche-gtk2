
;; mmc: here i extracted some utils: not strictly connected with generating & parsing C 
;; 

(define-module h2s.utils
  ;; (extend mmc.log)
  (export
   report
   mixed-case-name->hyphenated-name
   verbose                              ;param
   )
  (use gauche.parameter)
  
  )
(select-module h2s.utils)


(define verbose (make-parameter #t))
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
              (loop (read-char) c0 0))))))))


(provide "h2s/utils")
