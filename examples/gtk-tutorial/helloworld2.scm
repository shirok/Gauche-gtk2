;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: helloworld2.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (callback w button)
  (format #t "Hello again (~s) - ~s was pressed.\n" w button))

(define (delete-event w event)
  (gtk-main-quit))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Hello Buttons!")
    (g-signal-connect window "delete_event" delete-event)
    (gtk-container-set-border-width window 10)
    (let ((box1    (gtk-hbox-new #f 0))
          (button1 (gtk-button-new-with-label "Button 1"))
          (button2 (gtk-button-new-with-label "Button 2")))
      (gtk-container-add window box1)
      (g-signal-connect button1 "clicked" (cut callback <> "button 1"))
      (gtk-box-pack-start box1 button1 #t #t 0)
      (gtk-widget-show button1)
      (g-signal-connect button2 "clicked" (cut callback <> "button 2"))
      (gtk-box-pack-start box1 button2 #t #t 0)
      (gtk-widget-show button2)
      (gtk-widget-show box1)
      (gtk-widget-show window)
      ))
  (gtk-main)
  0)
