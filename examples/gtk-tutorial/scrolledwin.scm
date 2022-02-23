;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-dialog-new)
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit) #f))
    (gtk-window-set-title window "GtkScrolledWindow example")
    (gtk-container-set-border-width window 0)
    (gtk-widget-set-size-request window 300 300)

    (let1 scrolled-window (gtk-scrolled-window-new #f #f)
      (gtk-container-set-border-width scrolled-window 10)
      (gtk-scrolled-window-set-policy scrolled-window
                                      GTK_POLICY_AUTOMATIC
                                      GTK_POLICY_ALWAYS)
      (gtk-box-pack-start (ref window 'vbox) scrolled-window #t #t 0)
      (gtk-widget-show scrolled-window)

      (let1 table (gtk-table-new 10 10 #f)
        (gtk-table-set-row-spacings table 10)
        (gtk-table-set-col-spacings table 10)
        (gtk-scrolled-window-add-with-viewport scrolled-window table)
        (gtk-widget-show table)

        (dotimes (i 10)
          (dotimes (j 10)
            (let1 button (gtk-toggle-button-new-with-label
                          (format #f "button (~s,~s)" i j))
              (gtk-table-attach-defaults table button i (+ i 1) j (+ j 1))
              (gtk-widget-show button))))
        ) ;table
      ) ; scrolled-window
    (let1 button (gtk-button-new-with-label "close")
      (g-signal-connect button "clicked"
                        (lambda _ (gtk-widget-destroy window)))
      (gtk-widget-set-flags button GTK_CAN_DEFAULT)
      (gtk-box-pack-start (ref window 'action-area) button #t #t 0)
      (gtk-widget-grab-default button)
      (gtk-widget-show button))
    (gtk-widget-show window)
    )
  (gtk-main)
  0)
