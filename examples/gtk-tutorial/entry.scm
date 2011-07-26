;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: entry.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-widget-set-size-request window 200 100)
    (gtk-window-set-title window "GTK Entry")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (g-signal-connect window "delete_event"
                      (lambda _ (gtk-widget-destroy window)))
    (let1 vbox (gtk-vbox-new #f 0)
      (gtk-container-add window vbox)
      (gtk-widget-show vbox)
      (let1 entry (gtk-entry-new)
        (gtk-entry-set-max-length entry 50)
        (g-signal-connect entry "activate"
                          (lambda (entry)
                            (format #t "Entry contents: ~a\n" (gtk-entry-get-text entry))
                            ))
        (gtk-entry-set-text entry "hello")
        (let1 pos (slot-ref entry 'text-length)
          (gtk-editable-insert-text entry " world" pos))
        (gtk-editable-select-region entry 0 (slot-ref entry 'text-length))
        (gtk-box-pack-start vbox entry #t #t 0)
        (gtk-widget-show entry)
        
        (let1 hbox (gtk-hbox-new #f 0)
          (gtk-container-add vbox hbox)
          (gtk-widget-show hbox)
          (let1 check (gtk-check-button-new-with-label "Editable")
            (gtk-box-pack-start hbox check #t #t 0)
            (g-signal-connect check "toggled"
                              (lambda (check)
                                (gtk-editable-set-editable entry
                                                           (not (zero? (slot-ref check 'active))))))
            (gtk-toggle-button-set-active check #t)
            (gtk-widget-show check))
          (let1 check (gtk-check-button-new-with-label "Visible")
            (gtk-box-pack-start hbox check #t #t 0)
            (g-signal-connect check "toggled"
                              (lambda (check)
                                (gtk-entry-set-visibility entry
                                                          (not (zero? (slot-ref check 'active))))))
            (gtk-toggle-button-set-active check #t)
            (gtk-widget-show check))
          )
        )
      (let1 button (gtk-button-new-from-stock GTK_STOCK_CLOSE)
        (g-signal-connect button "clicked"
                          (lambda _ (gtk-widget-destroy window)))
        (gtk-box-pack-start vbox button #t #t 0)
        (gtk-widget-set-flags button GTK_CAN_DEFAULT)
        (gtk-widget-grab-default button)
        (gtk-widget-show button))
      )
    (gtk-widget-show window)
    )
  (gtk-main)
  0)

      
        