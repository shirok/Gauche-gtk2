;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Event Box")
    (g-signal-connect window "destroy" (lambda _ (exit 0)))
    (gtk-container-set-border-width window 10)

    (let1 event-box (gtk-event-box-new)
      (gtk-container-add window event-box)
      (gtk-widget-show event-box)
      (let1 label (gtk-label-new "Click here to quit, quit, quit, quit, quit")
        (gtk-container-add event-box label)
        (gtk-widget-show label)
        (gtk-widget-set-size-request label 110 20)
        )
      (gtk-widget-set-events event-box GDK_BUTTON_PRESS_MASK)
      (g-signal-connect event-box "button_press_event"
                        (lambda _ (exit 0)))

      (gtk-widget-realize event-box)
      (gdk-window-set-cursor (ref event-box 'window)
                             (gdk-cursor-new GDK_HAND1))
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)
