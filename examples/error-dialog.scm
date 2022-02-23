;; sample to use gtk.error-dialog

(use gtk)
(use gtk.error-dialog)

(define *error-count* 0)

(define (main args)
  (gtk-init args)
  (let ((w (gtk-window-new GTK_WINDOW_TOPLEVEL))
        (b (gtk-button-new-with-label "don't press me")))
    (g-signal-connect w "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-add w b)
    (g-signal-connect b "clicked"
                      (lambda _
                        (inc! *error-count*)
                        (case *error-count*
                          ((1) (error "I said not to press me."))
                          ((2) (error "Stop pressing me, please."))
                          ((3) (error "What do you want?  No white rabbit will come out!"))
                          ((4) (error "OK, You are so bored.  But I can offer nothing to you.
       I'm just a small script with lots of parenthesis."))
                          ((5) (error "You gotta do something more constructive.
       How about start reading SICP?"))
                          ((6) (error "SICP is \"Structure and Intepretation of Computer Programs.\"
       The book is published from MIT Press, but also on-line at
       http://mitpress.mit.edu/sicp/full-text/book/book.html"))
                          ((7) (error "If you keep doing this, I assure you will keep making errors.  You're warned."))
                          (else (errorf "You made ~a errors." *error-count*))))
                      )
    (gtk-widget-show-all w)
    (gtk-scheme-enable-error-dialog w))
  (gtk-main)
  0)
