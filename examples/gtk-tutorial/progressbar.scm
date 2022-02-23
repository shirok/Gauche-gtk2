;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define-class <progress-data> ()
  ((window :init-keyword :window)
   (pbar   :init-keyword :pbar)
   (timer  :init-keyword :timer)
   (activity-mode :init-keyword :activity-mode :initform #f)
   ))

(define-method progress-timeout ((pdata <progress-data>))
  (if (ref pdata 'activity-mode)
      (gtk-progress-bar-pulse (ref pdata 'pbar))
      (let1 new-val (fmod (+ (gtk-progress-bar-get-fraction (ref pdata 'pbar))
                             0.01)
                          1.0)
        (gtk-progress-bar-set-fraction (ref pdata 'pbar) new-val)))
  #t)

(define-method toggle-show-text ((pdata <progress-data>))
  (let1 text (gtk-progress-bar-get-text (ref pdata 'pbar))
    (if (and text (not (string=? text "")))
        (gtk-progress-bar-set-text (ref pdata 'pbar) "")
        (gtk-progress-bar-set-text (ref pdata 'pbar) "some text"))))

(define-method toggle-activity-mode ((pdata <progress-data>))
  (update! (ref pdata 'activity-mode) not)
  (if (ref pdata 'activity-mode)
      (gtk-progress-bar-pulse (ref pdata 'pbar))
      (gtk-progress-bar-set-fraction (ref pdata 'pbar) 0.0)))

(define-method toggle-orientation ((pdata <progress-data>))
  (let1 mode (gtk-progress-bar-get-orientation (ref pdata 'pbar))
    (cond
     ((eqv? mode GTK_PROGRESS_LEFT_TO_RIGHT)
      (gtk-progress-bar-set-orientation (ref pdata 'pbar)
                                        GTK_PROGRESS_RIGHT_TO_LEFT))
     ((eqv? mode GTK_PROGRESS_RIGHT_TO_LEFT)
      (gtk-progress-bar-set-orientation (ref pdata 'pbar)
                                        GTK_PROGRESS_LEFT_TO_RIGHT))
     )))

(define-method destroy-progress ((pdata <progress-data>))
  (gtk-timeout-remove (ref pdata 'timer))
  (gtk-main-quit))

(define (main args)
  (gtk-init args)
  (let1 pdata (make <progress-data>
                :window (gtk-window-new GTK_WINDOW_TOPLEVEL)
                :pbar   (gtk-progress-bar-new))
    (gtk-window-set-resizable (ref pdata 'window) #t)
    (g-signal-connect (ref pdata 'window) "destroy"
                      (lambda _ (destroy-progress pdata)))
    (gtk-window-set-title (ref pdata 'window) "GtkProgressBar")
    (gtk-container-set-border-width (ref pdata 'window) 0)

    (let1 vbox (gtk-vbox-new #f 5)
      (gtk-container-set-border-width vbox 10)
      (gtk-container-add (ref pdata 'window) vbox)
      (gtk-widget-show vbox)

      (let1 align (gtk-alignment-new 0.5 0.5 0 0)
        (gtk-box-pack-start vbox align #f #f 5)
        (gtk-widget-show align)

        (gtk-container-add align (ref pdata 'pbar))
        (gtk-widget-show (ref pdata 'pbar))
        (set! (ref pdata 'timer)
              (gtk-timeout-add 100 (lambda _ (progress-timeout pdata))))
        )
      (let1 separator (gtk-hseparator-new)
        (gtk-box-pack-start vbox separator #f #f 0)
        (gtk-widget-show separator))
      (let1 table (gtk-table-new 2 2 #f)
        (gtk-box-pack-start vbox table #f #t 0)
        (gtk-widget-show table)
        (let1 check (gtk-check-button-new-with-label "Show text")
          (gtk-table-attach table check 0 1 0 1
                            (logior GTK_EXPAND GTK_FILL)
                            (logior GTK_EXPAND GTK_FILL)
                            5 5)
          (g-signal-connect check "clicked"
                            (lambda _ (toggle-show-text pdata)))
          (gtk-widget-show check))
        (let1 check (gtk-check-button-new-with-label "Activity mode")
          (gtk-table-attach table check 0 1 1 2
                            (logior GTK_EXPAND GTK_FILL)
                            (logior GTK_EXPAND GTK_FILL)
                            5 5)
          (g-signal-connect check "clicked"
                            (lambda _ (toggle-activity-mode pdata)))
          (gtk-widget-show check))
        (let1 check (gtk-check-button-new-with-label "Right to Left")
          (gtk-table-attach table check 0 1 2 3
                            (logior GTK_EXPAND GTK_FILL)
                            (logior GTK_EXPAND GTK_FILL)
                            5 5)
          (g-signal-connect check "clicked"
                            (lambda _ (toggle-orientation pdata)))
          (gtk-widget-show check))
        )
      (let1 button (gtk-button-new-with-label "close")
        (g-signal-connect button "clicked"
                          (lambda _ (gtk-widget-destroy (ref pdata 'window))))
        (gtk-box-pack-start vbox button #f #f 0)
        (gtk-widget-set-flags button GTK_CAN_DEFAULT)
        (gtk-widget-grab-default button)
        (gtk-widget-show button))
      )
    (gtk-widget-show (ref pdata 'window))
    )
  (gtk-main)
  0)
