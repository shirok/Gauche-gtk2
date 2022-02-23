;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-widget-set-size-request window 200 100)
    (gtk-window-set-title window "GTK Menu Test")
    (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))

    (let1 menu (gtk-menu-new)
      (dotimes (i 3)
        (let* ((s #`"Test-undermenu - ,i")
               (menu-item (gtk-menu-item-new-with-label s)))
          (gtk-menu-shell-append menu menu-item)
          (g-signal-connect menu-item "activate" (lambda _ (print s)))
          (gtk-widget-show menu-item)))
      (let1 root-menu (gtk-menu-item-new-with-label "Root Menu")
        (gtk-widget-show root-menu)
        (gtk-menu-item-set-submenu root-menu menu)
        (let1 vbox (gtk-vbox-new #f 0)
          (gtk-container-add window vbox)
          (gtk-widget-show vbox)
          (let1 menu-bar (gtk-menu-bar-new)
            (gtk-box-pack-start vbox menu-bar #f #f 2)
            (gtk-widget-show menu-bar)
            (let1 button (gtk-button-new-with-label "press me")
              (g-signal-connect button "event"
                                (lambda (w event)
                                  (if (eqv? (ref event 'type)
                                            GDK_BUTTON_PRESS)
                                      (begin
                                        (gtk-menu-popup menu
                                                        #f #f #f
                                                        (ref event 'button)
                                                        (ref event 'time))
                                        #t)
                                      #f)))
              (gtk-box-pack-end vbox button #t #t 2)
              (gtk-widget-show button))
            (gtk-menu-shell-append menu-bar root-menu)
            ))))
    (gtk-widget-show window)
    )
  (gtk-main)
  0)
