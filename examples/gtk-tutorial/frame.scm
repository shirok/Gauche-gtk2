;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Frame Example")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-widget-set-size-request window 300 300)
    (gtk-container-set-border-width window 10)

    (let1 frame (gtk-frame-new #f)
      (gtk-container-add window frame)
      (gtk-frame-set-label frame "GTK Frame Widget")
      (gtk-frame-set-label-align frame 1.0 1.0)
      (gtk-frame-set-shadow-type frame GTK_SHADOW_ETCHED_OUT)
      (gtk-widget-show frame)
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)
