;;
;; Simple example, ported from the one in Gtk+2.0 tutorial.
;;
;; $Id: packbox.scm,v 1.2 2007/01/13 01:36:30 maruska Exp $

(use gtk)

;; Note: the delete_event handler is directly created in the main function

;; Make a new hbox filled with button-labels.
;; Note the use of internal function, compared to C version that
;; has to repeat same patterns.
(define (make-box homogeneous? spacing expand? fill? padding)
  (let ((box    (gtk-hbox-new homogeneous? spacing)))

    (define (make-packed-button label)
      (let ((button (gtk-button-new-with-label label)))
        (gtk-box-pack-start box button expand? fill? padding)
        (gtk-widget-show button)))

    (make-packed-button "gtk_box_pack")
    (make-packed-button "(box,")
    (make-packed-button "button,")
    (make-packed-button (if expand? "TRUE," "FALSE,"))
    (make-packed-button (if fill? "TRUE," "FALSE,"))
    (make-packed-button #`",|padding|);")
    box))

(define (main args)
  (gtk-init args)
  (unless (= (length args) 2)
    (error "usage: packbox num, where num is 1, 2, or 3."))
  (let* ((which  (string->number (cadr args)))
         (window (gtk-window-new GTK_WINDOW_TOPLEVEL)))
    (g-signal-connect window "delete_event"
                      (lambda (w e) (gtk-main-quit) #f))
    (gtk-container-set-border-width window 10)
    (let1 box1 (gtk-vbox-new #f 0)
      (case which
        ((1)
         (let ((make-packed-box
                (lambda params
                  (let1 box2 (apply make-box params)
                    (gtk-box-pack-start box1 box2 #f #f 0)
                    (gtk-widget-show box2)))))
           (let1 label (gtk-label-new "gtk-hbox-new (FALSE, 0);")
             (gtk-misc-set-alignment label 0 0)
             (gtk-box-pack-start box1 label #f #f 0)
             (gtk-widget-show label)

           (make-packed-box #f 0 #f #f 0)
           (make-packed-box #f 0 #t #f 0)
           (make-packed-box #f 0 #t #t 0)

           (let1 separator (gtk-hseparator-new)
             (gtk-box-pack-start box1 separator #f #t 5)
             (gtk-widget-show separator))

           (let1 label (gtk-label-new "gtk-hbox-new (TRUE, 0);")
             (gtk-misc-set-alignment label 0 0)
             (gtk-box-pack-start box1 label #f #f 0)
             (gtk-widget-show label))

           (make-packed-box #t 0 #t #f 0)
           (make-packed-box #t 0 #t #t 0)

           (let1 separator (gtk-hseparator-new)
             (gtk-box-pack-start box1 separator #f #t 5)
             (gtk-widget-show separator))
           )))
        
        ((2)
         (let ((make-packed-box
                (lambda params
                  (let1 box2 (apply make-box params)
                    (gtk-box-pack-start box1 box2 #f #f 0)
                    (gtk-widget-show box2)))))
           (let1 label (gtk-label-new "gtk-hbox-new (FALSE, 10);")
             (gtk-misc-set-alignment label 0 0)
             (gtk-box-pack-start box1 label #f #f 0)
             (gtk-widget-show label)

           (make-packed-box #f 10 #t #f 0)
           (make-packed-box #f 10 #t #t 0)

           (let1 separator (gtk-hseparator-new)
             (gtk-box-pack-start box1 separator #f #t 5)
             (gtk-widget-show separator))

           (let1 label (gtk-label-new "gtk-hbox-new (FALSE, 0);")
             (gtk-misc-set-alignment label 0 0)
             (gtk-box-pack-start box1 label #f #f 0)
             (gtk-widget-show label))

           (make-packed-box #f 0 #t #f 10)
           (make-packed-box #f 0 #t #t 10)

           (let1 separator (gtk-hseparator-new)
             (gtk-box-pack-start box1 separator #f #t 5)
             (gtk-widget-show separator))
           )))

        ((3)
         (let ((box2 (make-box #f 0 #f #f 0)))

           (let1 label (gtk-label-new "end")
             (gtk-box-pack-end box2 label #f #f 0)
             (gtk-widget-show label))

           (gtk-box-pack-start box1 box2 #f #f 0)
           (gtk-widget-show box2)
           
           (let1 separator (gtk-hseparator-new)
             (gtk-widget-set-size-request separator 400 5)
             (gtk-box-pack-start box1 separator #f #t 5)
             (gtk-widget-show separator))
           ))
        )
      (let ((quitbox (gtk-hbox-new #f 0))
            (button (gtk-button-new-with-label "Quit")))
        (g-signal-connect button "clicked" (lambda _ (gtk-main-quit)))
        (gtk-box-pack-start quitbox button #t #f 0)
        (gtk-box-pack-start box1 quitbox #f #f 0)
        (gtk-container-add window box1)
        (gtk-widget-show button)
        (gtk-widget-show quitbox))

      (gtk-widget-show box1))
    (gtk-widget-show window))
  (gtk-main)
  0)




           
