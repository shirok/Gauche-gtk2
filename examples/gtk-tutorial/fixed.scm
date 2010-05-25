;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: fixed.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Fixed Container")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)

    (let ((fixed (gtk-fixed-new))
          (x 50) (y 50))
      (gtk-container-add window fixed)
      (gtk-widget-show fixed)

      (dotimes (i 3)
        (let1 button (gtk-button-new-with-label "Press me")
          (g-signal-connect button "clicked"
                            (lambda (w)
                              (set! x (modulo (+ x 30) 300))
                              (set! y (modulo (+ y 50) 300))
                              (gtk-fixed-move fixed w x y)))
          (gtk-fixed-put fixed button (* (+ i 1) 50) (* (+ i 1) 50))
          (gtk-widget-show button)))
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)

      
        