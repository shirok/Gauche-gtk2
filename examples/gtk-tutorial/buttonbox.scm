;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (create-bbox horizontal? title spacing child-w child-h layout)
  (let ((frame (gtk-frame-new title))
        (bbox  (if horizontal? (gtk-hbutton-box-new) (gtk-vbutton-box-new))))
    (gtk-container-set-border-width bbox 5)
    (gtk-container-add frame bbox)
    (gtk-button-box-set-layout bbox layout)
    (gtk-box-set-spacing bbox spacing)
    (let1 button (gtk-button-new-from-stock GTK_STOCK_OK)
      (gtk-container-add bbox button))
    (let1 button (gtk-button-new-from-stock GTK_STOCK_CANCEL)
      (gtk-container-add bbox button))
    (let1 button (gtk-button-new-from-stock GTK_STOCK_HELP)
      (gtk-container-add bbox button))
    frame))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Button Boxes")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)
    (let1 main-vbox (gtk-vbox-new #f 0)
      (gtk-container-add window main-vbox)
      (let1 frame-horz (gtk-frame-new "Horizontal Button Boxes")
        (gtk-box-pack-start main-vbox frame-horz #t #t 10)
        (let1 vbox (gtk-vbox-new #f 0)
          (gtk-container-set-border-width vbox 10)
          (gtk-container-add frame-horz vbox)
          (gtk-box-pack-start vbox
                              (create-bbox #t "Spread (spacing 40)" 40 85 20
                                           GTK_BUTTONBOX_SPREAD)
                              #t #t 0)
          (gtk-box-pack-start vbox
                              (create-bbox #t "Edge (spacing 30)" 30 85 20
                                           GTK_BUTTONBOX_EDGE)
                              #t #t 5)
          (gtk-box-pack-start vbox
                              (create-bbox #t "Start (spacing 20)" 20 85 20
                                           GTK_BUTTONBOX_START)
                              #t #t 5)
          (gtk-box-pack-start vbox
                              (create-bbox #t "End (spacing 10)" 10 85 20
                                           GTK_BUTTONBOX_END)
                              #t #t 5)
          ))
      (let1 frame-vert (gtk-frame-new "Vertical Button Boxes")
        (gtk-box-pack-start main-vbox frame-vert #t #t 10)
        (let1 hbox (gtk-hbox-new #f 0)
          (gtk-container-set-border-width hbox 10)
          (gtk-container-add frame-vert hbox)
          (gtk-box-pack-start hbox
                              (create-bbox #f "Spread (spacing 5)" 5 85 20
                                           GTK_BUTTONBOX_SPREAD)
                              #t #t 0)
          (gtk-box-pack-start hbox
                              (create-bbox #f "Edge (spacing 30)" 30 85 20
                                           GTK_BUTTONBOX_EDGE)
                              #t #t 5)
          (gtk-box-pack-start hbox
                              (create-bbox #f "Start (spacing 20)" 20 85 20
                                           GTK_BUTTONBOX_START)
                              #t #t 5)
          (gtk-box-pack-start hbox
                              (create-bbox #f "End (spacing 10)" 20 85 20
                                           GTK_BUTTONBOX_END)
                              #t #t 5)
          ))
      (gtk-widget-show-all window)))
  (gtk-main)
  0)
