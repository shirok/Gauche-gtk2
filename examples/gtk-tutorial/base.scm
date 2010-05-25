;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: base.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-widget-show window))
  (gtk-main)
  0)


      
        