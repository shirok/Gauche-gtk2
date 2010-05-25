;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: list.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

(define-constant *list-item-data-key* "list-item-data")

(define (main args)
  (gtk-init args)
  (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (gtk-window-set-title window "GtkList Example")
    (g-signal-connect window "destroy" (lambda _ (gtk-main-quit)))

    (let1 vbox (gtk-vbox-new #f 5)
      (gtk-container-set-border-width vbox 5)
      (gtk-container-add window vbox)
      (gtk-widget-show vbox)

      (let1 scrolled-window (gtk-scrolled-window-new #f #f)
        (gtk-widget-set-size-request scrolled-window 250 150)
        (gtk-container-add vbox scrolled-window)
        (gtk-widget-show scrolled-window)

        (let1 gtklist (gtk-list-new)
          (gtk-scrolled-window-add-with-viewport scrolled-window gtklist)
          (gtk-widget-show gtklist)
          (g-signal-connect gtklist "selection_changed"
                            sigh-print-selection)
          
          (let1 frame (gtk-frame-new "Prison")
            (gtk-widget-set-size-request frame 200 50)
            (gtk-container-set-border-width frame 5)
            (gtk-frame-set-shadow-type frame GTK_SHADOW_OUT)
            (gtk-container-add vbox frame)
            (gtk-widget-show frame)

            (g-signal-connect gtklist "button_release_event"
                              (lambda (w e)
                                (sigh-button-event w e frame))))

          (let1 separator (gtk-hseparator-new)
            (gtk-container-add vbox separator)
            (gtk-widget-show separator))

          (let1 button (gtk-button-new-with-label "Close")
            (gtk-container-add vbox button)
            (gtk-widget-show button)
            (g-signal-connect button "clicked"
                              (lambda _ (gtk-widget-destroy window))))

          ;; list items
          (dotimes (i 5)
            (let ((label (gtk-label-new #`"ListItemContainer with Label #,i"))
                  (list-item (gtk-list-item-new)))
              (gtk-container-add list-item label)
              (gtk-widget-show label)
              (gtk-container-add gtklist list-item)
              (gtk-widget-show list-item)
              (g-object-set-data list-item *list-item-data-key*
                                 (gtk-label-get-text label))))
          ;; more list items, using gtk-list-append-items
          (let ((items '()))
            (dotimes (i 10)
              (let1 list-item (gtk-list-item-new-with-label
                               #`"List Item with Label ,i")
                (push! items list-item)
                (gtk-widget-show list-item)
                (g-object-set-data list-item *list-item-data-key*
                                   "ListItem with integrated Label")))
            (gtk-list-append-items gtklist items))
          )
        )
      )
    (gtk-widget-show-all window)
    )
  (gtk-main)
  0)

(define (sigh-button-event gtklist event frame)
  (when (and (eqv? (slot-ref event 'type) GDK_BUTTON_RELEASE)
             (eqv? (slot-ref event 'button) 3))
    (let* ((selection (slot-ref gtklist 'selection))
           (new-prisoner (if (null? selection) #f (car selection))))
      (for-each (lambda (w)
                  (when (is-a? w <gtk-list-item>)
                    (gtk-widget-reparent w gtklist)))
                (gtk-container-get-children frame))
      (when new-prisoner
        (gtk-list-unselect-child gtklist new-prisoner)
        (gtk-widget-reparent new-prisoner frame))))
  #f)

(define (sigh-print-selection gtklist)
  (let1 selection (slot-ref gtklist 'selection)
    (if (null? selection)
        (print "Selection cleared")
        (format #t "The selection is a ~s\n"
                (map (cut g-object-get-data <> *list-item-data-key*)
                     selection))))
  #f)
