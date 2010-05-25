;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: arrow.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (create-arrow-button arrow-type shadow-type)
  (let ((button (gtk-button-new))
        (arrow  (gtk-arrow-new arrow-type shadow-type)))
    (gtk-container-add button arrow)
    (gtk-widget-show button)
    (gtk-widget-show arrow)
    button))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Arrow Buttons")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)
    (let1 box (gtk-hbox-new #f 0)
      (gtk-container-set-border-width box 2)
      (gtk-container-add window box)
      (gtk-widget-show box)
      (let1 button (create-arrow-button GTK_ARROW_UP GTK_SHADOW_IN)
        (gtk-box-pack-start box button #f #f 3))
      (let1 button (create-arrow-button GTK_ARROW_DOWN GTK_SHADOW_OUT)
        (gtk-box-pack-start box button #f #f 3))
      (let1 button (create-arrow-button GTK_ARROW_LEFT GTK_SHADOW_ETCHED_IN)
        (gtk-box-pack-start box button #f #f 3))
      (let1 button (create-arrow-button GTK_ARROW_RIGHT GTK_SHADOW_ETCHED_OUT)
        (gtk-box-pack-start box button #f #f 3))
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)

      
        