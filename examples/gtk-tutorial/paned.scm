;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;

(use gtk)

(define (create-list)
  (let1 scrolled-window (gtk-scrolled-window-new #f #f)
    (gtk-scrolled-window-set-policy scrolled-window
                                    GTK_POLICY_AUTOMATIC
                                    GTK_POLICY_AUTOMATIC)
    (let ((model (gtk-list-store-new <string>))
          (tree-view (gtk-tree-view-new)))
      (gtk-scrolled-window-add-with-viewport scrolled-window tree-view)
      (gtk-tree-view-set-model tree-view model)
      (gtk-widget-show tree-view)
      (dotimes (i 10)
        (let1 iter (gtk-list-store-append model)
          (gtk-list-store-set-value model iter 0 #`"Message #,i")))
      (let* ((cell   (gtk-cell-renderer-text-new))
             (column (gtk-tree-view-column-new-with-attributes "Messages"
                                                               cell
                                                               "text" 0)))
        (gtk-tree-view-append-column tree-view column)))
    scrolled-window)
  )

(define (insert-text buffer)
  (let1 iter (gtk-text-buffer-get-iter-at-offset buffer 0)
    (gtk-text-buffer-insert buffer iter
                            "From: pathfinder@nasa.gov\nTo: mom@nasa.gov\nSubject: Made it!\n\nWe just got in this morning. The weather has been\ngreat - clear but cold, and there are lots of fun sights.\nSojourner says hi. See you soon.\n -Path\n"
                            -1))
  )

(define (create-text)
  (let* ((view (gtk-text-view-new))
         (buffer (gtk-text-view-get-buffer view))
         (scrolled-window (gtk-scrolled-window-new #f #f)))
    (gtk-scrolled-window-set-policy scrolled-window
                                    GTK_POLICY_AUTOMATIC
                                    GTK_POLICY_AUTOMATIC)
    (gtk-container-add scrolled-window view)
    (insert-text buffer)
    (gtk-widget-show-all scrolled-window)
    scrolled-window))

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "Paned Window")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))
    (gtk-container-set-border-width window 10)
    (gtk-widget-set-size-request window 450 400)

    (let1 vpaned (gtk-vpaned-new)
      (gtk-container-add window vpaned)
      (gtk-widget-show vpaned)

      (let1 list (create-list)
        (gtk-paned-add1 vpaned list)
        (gtk-widget-show list))
      (let1 text (create-text)
        (gtk-paned-add2 vpaned text)
        (gtk-widget-show text)))

    (gtk-widget-show window)
    )
  (gtk-main)
  0)
