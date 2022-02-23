;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define *spinner1* #f)

(define (toggle-snap button spin)
  (gtk-spin-button-set-snap-to-ticks spin
                                     (not (zero? (ref button 'active)))))

(define (toggle-numeric button spin)
  (gtk-spin-button-set-numeric spin (not (zero? (ref button 'active)))))

(define (change-digits spin)
  (gtk-spin-button-set-digits *spinner1*
                              (gtk-spin-button-get-value-as-int spin)))

(define (get-value widget data)
  (let ((spin *spinner1*)
        (label (g-object-get-data widget 'user_data)))
    (gtk-label-set-text label
                        (if (= data 1)
                            (number->string
                             (gtk-spin-button-get-value-as-int spin))
                            (number->string
                             (gtk-spin-button-get-value spin))))))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect window "destroy"
                      (lambda _ (gtk-main-quit) #f))
    (gtk-window-set-title window "Spin Button")

    (let1 main-vbox (gtk-vbox-new #f 5)
      (gtk-container-set-border-width main-vbox 10)
      (gtk-container-add window main-vbox)

      (let1 frame (gtk-frame-new "Not accelerated")
        (gtk-box-pack-start main-vbox frame #t #t 0)

        (let1 vbox (gtk-vbox-new #f 0)
          (gtk-container-set-border-width vbox 5)
          (gtk-container-add frame vbox)

          (let1 hbox (gtk-hbox-new #f 0)
            (gtk-box-pack-start vbox hbox #t #t 5)
            (let1 vbox2 (gtk-vbox-new #f 0)
              (gtk-box-pack-start hbox vbox2 #t #t 5)
              (let1 label (gtk-label-new "Day :")
                (gtk-misc-set-alignment label 0 0.5)
                (gtk-box-pack-start vbox2 label #f #t 0))
              (let* ((adj (gtk-adjustment-new 1.0 1.0 31.0 1.0 5.0 0.0))
                     (spinner (gtk-spin-button-new adj 0 0)))
                (gtk-spin-button-set-wrap spinner #t)
                (gtk-box-pack-start vbox2 spinner #f #t 0)))
            (let1 vbox2 (gtk-vbox-new #f 0)
              (gtk-box-pack-start hbox vbox2 #t #t 5)
              (let1 label (gtk-label-new "Month :")
                (gtk-misc-set-alignment label 0 0.5)
                (gtk-box-pack-start vbox2 label #f #t 0))
              (let* ((adj (gtk-adjustment-new 1.0 1.0 12.0 1.0 5.0 0.0))
                     (spinner (gtk-spin-button-new adj 0 0)))
                (gtk-spin-button-set-wrap spinner #t)
                (gtk-box-pack-start vbox2 spinner #f #t 0)))
            (let1 vbox2 (gtk-vbox-new #f 0)
              (gtk-box-pack-start hbox vbox2 #t #t 5)
              (let1 label (gtk-label-new "Year :")
                (gtk-misc-set-alignment label 0 0.5)
                (gtk-box-pack-start vbox2 label #f #t 0))
              (let* ((adj (gtk-adjustment-new 1998.0 0.0 2100.0 1.0 100.0 0.0))
                     (spinner (gtk-spin-button-new adj 0 0)))
                (gtk-spin-button-set-wrap spinner #f)
                (gtk-widget-set-size-request spinner 55 -1)
                (gtk-box-pack-start vbox2 spinner #f #t 0)))
            ) ;hbox
          ) ;vbox
        ) ;frame
      (let1 frame (gtk-frame-new "Accelerated")
        (gtk-box-pack-start main-vbox frame #t #t 0)
        (let1 vbox (gtk-vbox-new #f 0)
          (gtk-container-set-border-width vbox 5)
          (gtk-container-add frame vbox)
          (let1 hbox (gtk-hbox-new #f 0)
            (gtk-box-pack-start vbox hbox #f #t 5)
            (let1 vbox2 (gtk-vbox-new #f 0)
              (gtk-box-pack-start hbox vbox2 #t #t 5)
              (let1 label (gtk-label-new "Value :")
                (gtk-misc-set-alignment label 0 0.5)
                (gtk-box-pack-start vbox2 label #f #t 0))
              (let ((adj (gtk-adjustment-new 0.0 -10000.0 10000.0 0.5
                                             100.0 0.0)))
                (set! *spinner1* (gtk-spin-button-new adj 1.0 2)))
                (gtk-spin-button-set-wrap *spinner1* #t)
                (gtk-widget-set-size-request *spinner1* 100 -1)
                (gtk-box-pack-start vbox2 *spinner1* #f #t 0))
            (let1 vbox2 (gtk-vbox-new #f 0)
              (gtk-box-pack-start hbox vbox2 #t #t 0)
              (let1 label (gtk-label-new "Digits :")
                (gtk-misc-set-alignment label 0 0.5)
                (gtk-box-pack-start vbox2 label #f #t 0)
                (let* ((adj (gtk-adjustment-new 2 1 5 1 1 0))
                       (spinner2 (gtk-spin-button-new adj 0.0 0)))
                  (gtk-spin-button-set-wrap spinner2 #t)
                  (g-signal-connect adj "value_changed"
                                    (lambda _ (change-digits spinner2)))
                  (gtk-box-pack-start vbox2 spinner2 #f #t 0))))
            ) ; hbox
          (let1 hbox (gtk-hbox-new #f 0)
            (gtk-box-pack-start vbox hbox #f #t 5)
            (let1 button (gtk-check-button-new-with-label "Snap to 0.5-ticks")
              (g-signal-connect button "clicked"
                                (lambda _ (toggle-snap button *spinner1*)))
              (gtk-box-pack-start vbox button #t #t 0)
              (gtk-toggle-button-set-active button #t))
            (let1 button (gtk-check-button-new-with-label "Numeric only input mode")
              (g-signal-connect button "clicked"
                                (lambda _ (toggle-numeric button *spinner1*)))
              (gtk-box-pack-start vbox button #t #t 0)
              (gtk-toggle-button-set-active button #t)))
          (let ((val-label (gtk-label-new ""))
                (hbox (gtk-hbox-new #f 0)))
            (gtk-box-pack-start vbox hbox #f #t 5)
            (let1 button (gtk-button-new-with-label "Value as Int")
              (g-object-set-data button 'user_data val-label)
              (g-signal-connect button "clicked"
                                (lambda _ (get-value button 1)))
              (gtk-box-pack-start hbox button #t #t 5))
            (let1 button (gtk-button-new-with-label "Value as Float")
              (g-object-set-data button 'user_data val-label)
              (g-signal-connect button "clicked"
                                (lambda _ (get-value button 2)))
              (gtk-box-pack-start hbox button #t #t 5))
            (gtk-box-pack-start vbox val-label #t #t 0)
            (gtk-label-set-text val-label "0"))
          ) ; vbox
        ) ; frame
      (let1 hbox (gtk-hbox-new #f 0)
        (gtk-box-pack-start main-vbox hbox #f #t 0)
        (let1 button (gtk-button-new-with-label "Close")
          (g-signal-connect button "clicked"
                            (lambda _ (gtk-widget-destroy window)))
          (gtk-box-pack-start hbox button #t #t 5)))
      ) ; main-vbox
    (gtk-widget-show-all window))
  (gtk-main)
  0)
