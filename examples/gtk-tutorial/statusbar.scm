;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define push-item
  (let ((count 1))
    (lambda (sbar data)
      (gtk-statusbar-push sbar data #`"item ,count")
      (inc! count))))

(define (pop-item sbar data)
  (gtk-statusbar-pop sbar data))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-widget-set-size-request window 200 100)
    (gtk-window-set-title window "GTK Statusbar Example")
    (g-signal-connect window "delete_event" (lambda _ (exit)))
    (let1 vbox (gtk-vbox-new #f 1)
      (gtk-container-add window vbox)
      (gtk-widget-show vbox)
      (let1 status-bar (gtk-statusbar-new)
        (gtk-box-pack-start vbox status-bar #t #t 0)
        (gtk-widget-show status-bar)
        (let1 context-id (gtk-statusbar-get-context-id status-bar
                                                       "Statusbar example")
          (let1 button (gtk-button-new-with-label "push item")
            (g-signal-connect button "clicked"
                              (lambda _ (push-item status-bar context-id)))
            (gtk-box-pack-start vbox button #t #t 2)
            (gtk-widget-show button))
          (let1 button (gtk-button-new-with-label "pop last item")
            (g-signal-connect button "clicked"
                              (lambda _ (pop-item status-bar context-id)))
            (gtk-box-pack-start vbox button #t #t 2)
            (gtk-widget-show button))
          ))
      )
    (gtk-widget-show window))
  (gtk-main)
  0)
