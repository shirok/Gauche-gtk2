;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 filew (gtk-file-selection-new "File selection")
    (g-signal-connect filew "destroy" (lambda _ (gtk-main-quit)))
    (g-signal-connect (slot-ref filew 'ok-button)
                      "clicked" (lambda (w)
                                  (format #t "~a\n" (gtk-file-selection-get-filename filew))))
    (gtk-file-selection-set-filename filew "penguin.png")
    (gtk-widget-show filew)
    )
  (gtk-main)
  0)
