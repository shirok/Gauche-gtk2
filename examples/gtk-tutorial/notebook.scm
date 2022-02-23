;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (rotate-book notebook)
  (gtk-notebook-set-tab-pos notebook
                            (modulo (+ (ref notebook 'tab-pos) 1) 4)))

(define (tabsborder-book notebook)
  (let ((tval (zero? (ref notebook 'show-tabs)))
        (bval (zero? (ref notebook 'show-border))))
    (gtk-notebook-set-show-tabs notebook tval)
    (gtk-notebook-set-show-border notebook bval)))

(define (remove-book notebook)
  (let1 page (gtk-notebook-get-current-page notebook)
    (gtk-notebook-remove-page notebook page)
    (gtk-widget-queue-draw notebook)))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)

    (let1 table (gtk-table-new 3 6 #f)
      (gtk-container-add window table)

      (let1 notebook (gtk-notebook-new)
        (gtk-notebook-set-tab-pos notebook GTK_POS_TOP)
        (gtk-table-attach-defaults table notebook 0 6 0 1)
        (gtk-widget-show notebook)

        (dotimes (i 5)
          (let1 frame (gtk-frame-new #`"Append Frame ,(+ i 1)")
            (gtk-container-set-border-width frame 10)
            (gtk-widget-set-size-request frame 100 75)
            (gtk-widget-show frame)
            (let1 label (gtk-label-new #`"Append Frame ,(+ i 1)")
              (gtk-container-add frame label)
              (gtk-widget-show label))
            (let1 label (gtk-label-new #`"Page ,(+ i 1)")
              (gtk-notebook-append-page notebook frame label))))

        (let1 checkbutton (gtk-check-button-new-with-label "Check me please!")
          (gtk-widget-set-size-request checkbutton 100 75)
          (gtk-widget-show checkbutton)

          (let1 label (gtk-label-new "Add page")
            (gtk-notebook-insert-page notebook checkbutton label 2)))

        (dotimes (i 5)
          (let1 frame (gtk-frame-new #`"Prepend Frame ,(+ i 1)")
            (gtk-container-set-border-width frame 10)
            (gtk-widget-set-size-request frame 100 75)
            (gtk-widget-show frame)
            (let1 label (gtk-label-new #`"Prepend Frame ,(+ i 1)")
              (gtk-container-add frame label)
              (gtk-widget-show label))
            (let1 label (gtk-label-new #`"PPage ,(+ i 1)")
              (gtk-notebook-prepend-page notebook frame label))))

        (gtk-notebook-set-current-page notebook 3)

        (let1 button (gtk-button-new-with-label "close")
          (g-signal-connect button "clicked"
                            (lambda _ (gtk-main-quit)))
          (gtk-table-attach-defaults table button 0 1 1 2)
          (gtk-widget-show button))
        (let1 button (gtk-button-new-with-label "next page")
          (g-signal-connect button "clicked"
                            (lambda _ (gtk-notebook-next-page notebook) #t))
          (gtk-table-attach-defaults table button 1 2 1 2)
          (gtk-widget-show button))
        (let1 button (gtk-button-new-with-label "prev page")
          (g-signal-connect button "clicked"
                            (lambda _ (gtk-notebook-prev-page notebook) #t))
          (gtk-table-attach-defaults table button 2 3 1 2)
          (gtk-widget-show button))
        (let1 button (gtk-button-new-with-label "tab position")
          (g-signal-connect button "clicked"
                            (lambda _ (rotate-book notebook) #t))
          (gtk-table-attach-defaults table button 3 4 1 2)
          (gtk-widget-show button))
        (let1 button (gtk-button-new-with-label "tabs/border on/off")
          (g-signal-connect button "clicked"
                            (lambda _ (tabsborder-book notebook) #t))
          (gtk-table-attach-defaults table button 4 5 1 2)
          (gtk-widget-show button))
        (let1 button (gtk-button-new-with-label "remove page")
          (g-signal-connect button "clicked"
                            (lambda _ (remove-book notebook) #t))
          (gtk-table-attach-defaults table button 5 6 1 2)
          (gtk-widget-show button))
        )
      (gtk-widget-show table)
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)
