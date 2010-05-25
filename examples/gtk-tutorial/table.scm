;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: table.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define (callback data)
  (format #t "Hello again - ~s was pressed\n" data))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Table")
    (g-signal-connect window "delete_event"
                      (lambda _ (gtk-main-quit) #f))
    (gtk-container-set-border-width window 20)
    (let1 table (gtk-table-new 2 2 #t)
      (gtk-container-add window table)
      (let1 button (gtk-button-new-with-label "button 1")
        (g-signal-connect button "clicked"
                          (lambda (w) (callback "button 1")))
        (gtk-table-attach-defaults table button 0 1 0 1)
        (gtk-widget-show button))
      (let1 button (gtk-button-new-with-label "button 2")
        (g-signal-connect button "clicked"
                          (lambda (w) (callback "button 2")))
        (gtk-table-attach-defaults table button 1 2 0 1)
        (gtk-widget-show button))
      (let1 button (gtk-button-new-with-label "Quit")
        (g-signal-connect button "clicked"
                          (lambda _ (gtk-main-quit)))
        (gtk-table-attach-defaults table button 0 2 1 2)
        (gtk-widget-show button))
      (gtk-widget-show table))
    (gtk-widget-show window))
  (gtk-main)
  0)
