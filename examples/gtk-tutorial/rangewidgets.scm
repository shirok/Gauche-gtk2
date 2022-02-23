;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)


(define (make-menu-item name callback)
  (let1 item (gtk-menu-item-new-with-label name)
    (g-signal-connect item "activate" callback)
    (gtk-widget-show item)
    item))

(define (create-range-controls)
  (let* ((window (gtk-window-new GTK_WINDOW_TOPLEVEL))
         (adj1   (gtk-adjustment-new 0.0 0.0 101.0 0.1 1.0 1.0))
         (vscale (gtk-vscale-new adj1))
         (hscale (gtk-hscale-new adj1)))

    (define (scale-set-default-values scale)
      (gtk-range-set-update-policy scale GTK_UPDATE_CONTINUOUS)
      (gtk-scale-set-digits scale 1)
      (gtk-scale-set-value-pos scale GTK_POS_TOP)
      (gtk-scale-set-draw-value scale #t))

    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-window-set-title window "range controls")
    (let1 box1 (gtk-vbox-new #f 0)
      (gtk-container-add window box1)
      (gtk-widget-show box1)

      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2)

        (scale-set-default-values vscale)
        (gtk-box-pack-start box2 vscale #t #t 0)
        (gtk-widget-show vscale)

        (let1 box3 (gtk-vbox-new #f 10)
          (gtk-box-pack-start box2 box3 #t #t 0)
          (gtk-widget-show box3)
          (gtk-widget-set-size-request hscale 200 -1)
          (scale-set-default-values hscale)
          (gtk-box-pack-start box3 hscale #t #t 0)
          (gtk-widget-show hscale)
          (let1 scrollbar (gtk-hscrollbar-new adj1)
            (gtk-range-set-update-policy scrollbar GTK_UPDATE_CONTINUOUS)
            (gtk-box-pack-start box3 scrollbar #t #t 0)
            (gtk-widget-show scrollbar))
          )
        )
      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2)

        (let1 button (gtk-check-button-new-with-label "Display value on scale widgets")
          (gtk-toggle-button-set-active button #t)
          (g-signal-connect button "toggled"
                            (lambda _
                              (for-each (cut gtk-scale-set-draw-value <>
                                             (not (zero? (ref button 'active))))
                                        (list hscale vscale))))
          (gtk-box-pack-start box2 button #t #t 0)
          (gtk-widget-show button)))
      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (let1 label (gtk-label-new "Scale Value Position:")
          (gtk-box-pack-start box2 label #f #f 0)
          (gtk-widget-show label))
        (let ((opt (gtk-option-menu-new))
              (menu (gtk-menu-new)))
          (for-each
           (lambda (label pos)
             (let1 item
                 (make-menu-item label
                                 (lambda _
                                   (for-each (cut gtk-scale-set-value-pos
                                                  <> pos)
                                             (list vscale hscale))))
               (gtk-menu-shell-append menu item)))
           '("Top" "Bottom" "Left" "Right")
           `(,GTK_POS_TOP ,GTK_POS_BOTTOM ,GTK_POS_LEFT ,GTK_POS_RIGHT))
          (gtk-option-menu-set-menu opt menu)
          (gtk-box-pack-start box2 opt #t #t 0)
          (gtk-widget-show opt))
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2))
      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (let1 label (gtk-label-new "Scale Update Policy:")
          (gtk-box-pack-start box2 label #f #f 0)
          (gtk-widget-show label))
        (let ((opt (gtk-option-menu-new))
              (menu (gtk-menu-new)))
          (for-each
           (lambda (label policy)
             (let1 item
                 (make-menu-item label
                                 (lambda _
                                   (for-each (cut gtk-range-set-update-policy
                                                  <> policy)
                                             (list vscale hscale))))
               (gtk-menu-shell-append menu item)))
           '("Continuous" "Discontinuous" "Delayed")
           `(,GTK_UPDATE_CONTINUOUS ,GTK_UPDATE_DISCONTINUOUS ,GTK_UPDATE_DELAYED))
          (gtk-option-menu-set-menu opt menu)
          (gtk-box-pack-start box2 opt #t #t 0)
          (gtk-widget-show opt))
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2))
      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (let1 label (gtk-label-new "Scale Digits:")
          (gtk-box-pack-start box2 label #f #f 0)
          (gtk-widget-show label))
        (let1 adj2 (gtk-adjustment-new 1.0 0.0 5.0 1.0 1.0 0.0)
          (g-signal-connect adj2 "value_changed"
                            (lambda _
                              (for-each (cut gtk-scale-set-digits <>
                                             (inexact->exact
                                              (round (ref adj2 'value))))
                                        (list hscale vscale))))
          (let1 scale (gtk-hscale-new adj2)
            (gtk-scale-set-digits scale 0)
            (gtk-box-pack-start box2 scale #t #t 0)
            (gtk-widget-show scale))
          )
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2))
      (let1 box2 (gtk-hbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (let1 label (gtk-label-new "Scrollbar Page Size:")
          (gtk-box-pack-start box2 label #f #f 0)
          (gtk-widget-show label))
        (let1 adj2 (gtk-adjustment-new 1.0 1.0 101.0 1.0 1.0 0.0)
          (g-signal-connect adj2 "value_changed"
                            (lambda _
                              (set! (ref adj1 'page-size)
                                    (ref adj2 'value))
                              (set! (ref adj1 'page-increment)
                                    (ref adj2 'value))
                              (gtk-adjustment-set-value adj1
                                                        (clamp (ref adj1 'value)
                                                               (ref adj1 'lower)
                                                               (- (ref adj1 'upper) (ref adj1 'page-size))))))
          (let1 scale (gtk-hscale-new adj2)
            (gtk-scale-set-digits scale 0)
            (gtk-box-pack-start box2 scale #t #t 0)
            (gtk-widget-show scale))
          )
        (gtk-box-pack-start box1 box2 #t #t 0)
        (gtk-widget-show box2))
      (let1 separator (gtk-hseparator-new)
        (gtk-box-pack-start box1 separator #f #t 0)
        (gtk-widget-show separator))
      (let1 box2 (gtk-vbox-new #f 10)
        (gtk-container-set-border-width box2 10)
        (gtk-box-pack-start box1 box2 #f #t 0)
        (gtk-widget-show box2)
        (let1 button (gtk-button-new-with-label "Quit")
          (g-signal-connect button "clicked"
                            (lambda _ (gtk-main-quit)))
          (gtk-box-pack-start box2 button #t #t 0)
          (gtk-widget-set-flags button GTK_CAN_DEFAULT)
          (gtk-widget-grab-default button)
          (gtk-widget-show button)))
      ) ; box1
    (gtk-widget-show window)
    )
  )

(define (main args)
  (gtk-init args)
  (create-range-controls)
  (gtk-main)
  0)
