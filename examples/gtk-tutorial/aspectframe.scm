;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Aspect Frame")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)

    (let1 aspect-frame (gtk-aspect-frame-new "2x1" 0.5 0.5 2 #f)
      (gtk-container-add window aspect-frame)
      (gtk-widget-show aspect-frame)

      ;; Create drawingarea and request it to be 200x200; but the aspect
      ;; frame forces 2x1 aspect, making it 200x100.
      (let1 drawing-area (gtk-drawing-area-new)
        (gtk-widget-set-size-request drawing-area 200 200)
        (gtk-container-add aspect-frame drawing-area)
        (gtk-widget-show drawing-area)
        )
      )

    (gtk-widget-show window)
    )
  (gtk-main)
  0)
