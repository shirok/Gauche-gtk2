;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-widget-show window))
  (gtk-main)
  0)
