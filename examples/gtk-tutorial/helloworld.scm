;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;;  Difference from C version:  Scheme version's signal callback
;;  doesn't take extra "user data": you can use closure if you need
;;  extra data.  With the same reason, there's no 'g-signal-connect-swapped'.
;;
;; $Id: helloworld.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (hello w)
  (format #t "Hello world (~s)\n" w))

(define (destroy w)
  (format #t "Destroying ~s\n" w)
  (gtk-main-quit))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect window "destroy" destroy)
    (gtk-container-set-border-width window 10)
    (let1 button (gtk-button-new-with-label "Hello world")
      (g-signal-connect button "clicked" hello)
      (g-signal-connect button "clicked" (lambda _ (destroy window)))
      (gtk-container-add window button)
      (gtk-widget-show button)
      (gtk-widget-show window)
      ))
  (gtk-main)
  0)
