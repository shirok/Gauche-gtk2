;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: buttons.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (xpm-label-box xpm-filename label-text)
  (let ((box   (gtk-hbox-new #f 0))
        (image (gtk-image-new-from-file xpm-filename))
        (label (gtk-label-new label-text)))
    (gtk-container-set-border-width box 2)
    (gtk-box-pack-start box image #f #f 3)
    (gtk-box-pack-start box label #f #f 3)
    (gtk-widget-show image)
    (gtk-widget-show label)
    box))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Pixmap'd Buttons!")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)
    (let1 button (gtk-button-new)
      (g-signal-connect button "clicked"
                        (lambda _ (format #t "cool button clicked\n")))
      (let1 box (xpm-label-box "info.xpm" "cool button")
        (gtk-widget-show box)
        (gtk-container-add button box))
      (gtk-widget-show button)
      (gtk-container-add window button))
    (gtk-widget-show window))
  (gtk-main)
  0)
