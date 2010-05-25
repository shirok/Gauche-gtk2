;;
;; Somebody asked me a sample of doing simple animation in Gauche-gtk,
;; and I hacked this up.  If you know better way, let me know.
;; Public domain -- use this as you like.  [SK]
;; $Id: gdk-animation.scm,v 1.3 2007/01/13 01:36:29 maruska Exp $
;;

(use math.const)
(use gtk)

(define *angle* 0)

(define (draw drawable fg bg)
  ;; clear
  (gdk-draw-rectangle drawable bg #t 0 0 100 100)
  ;; draw line
  (let ((x (inexact->exact (round (+ 50 (* (cos *angle*) 50)))))
        (y (inexact->exact (round (+ 50 (* (sin *angle*) 50))))))
    (gdk-draw-line drawable fg 50 50 x y)
    #t))

(define (main args)
  (gtk-init args)
  (let1 w (gtk-window-new GTK_WINDOW_TOPLEVEL)
    (g-signal-connect w "destroy" (lambda _ (gtk-main-quit)))
    (let* ((area (gtk-drawing-area-new))
           (drawable #f)  ;; initialized by realize callback
           (fg-gc #f)     ;; initialized by realize callback
           (bg-gc #f)     ;; initialized by realize callback
           )
      (gtk-widget-set-size-request area 100 100)
      (gtk-container-add w area)
      (g-signal-connect area "realize"
                        (lambda _
                          (set! drawable (ref area 'window))
                          (set! fg-gc (gdk-gc-new drawable))
                          (set! bg-gc (gdk-gc-new drawable))
                          (gdk-gc-set-foreground bg-gc
                                                 (ref (ref (ref area 'style) 'bg) 0))
                          ))
      (g-signal-connect area "expose_event"
                        (lambda (w event) (draw drawable fg-gc bg-gc)))
      (gtk-timeout-add 10
                       (lambda ()
                         (inc! *angle* (* pi 0.02))
                         (when drawable (draw drawable fg-gc bg-gc))))
      (gtk-widget-show area)
      )
    (gtk-widget-show w))
  (gtk-main)
  0)



