;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: pixmap.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define *xpm-data*
  '("16 16 3 1"
    "       c None"
    ".      c #000000000000"
    "X      c #FFFFFFFFFFFF"
    "                "
    "   ......       "
    "   .XXX.X.      "
    "   .XXX.XX.     "
    "   .XXX.XXX.    "
    "   .XXX.....    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .XXXXXXX.    "
    "   .........    "
    "                "
    "                "))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)
    (gtk-widget-show window)

    (let1 style (gtk-widget-get-style window)
      (receive (pixmap mask)
          (gdk-pixmap-create-from-xpm-d (ref window 'window)
                                        (ref (ref style 'bg) GTK_STATE_NORMAL)
                                        *xpm-data*)
        (let1 pixmapwid (gtk-pixmap-new pixmap mask)
          (gtk-widget-show pixmapwid)

          (let1 button (gtk-button-new)
            (gtk-container-add button pixmapwid)
            (gtk-container-add window button)
            (gtk-widget-show button)
            (g-signal-connect button "clicked"
                              (lambda _ (print "button clicked"))))
          )))
    )
  (gtk-main)
  0)

      
        