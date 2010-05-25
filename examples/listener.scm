;; sample to use gtk.listener

(use gtk)

(define (main args)
  (gtk-init args)
  (let1 w (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect w "destroy" (lambda _ (gtk-main-quit)))
    (gtk-widget-set-size-request w 100 100)
    (gtk-widget-show w))
  (gtk-scheme-listener-add :finalizer gtk-main-quit)
  (gtk-main)
  0)

