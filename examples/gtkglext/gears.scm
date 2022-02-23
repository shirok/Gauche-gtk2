;;
;; 3-D gear wheels.  This program is in the public domain.
;;
;; Brian Paul
;;
;; Conversion to GLUT by Mark J. Kilgard
;; Conversion to GtkGLExt by Naofumi Yasufuku
;; Port to Scheme by Shiro Kawai

(use math.const)
(use gtk)
(use gtk.gtkgl)
(use gl)

;; Draw a gear wheel.  You'll probably want to call this function when
;; building a display list since we do a lot of trig here.
;;
;; Input:  inner_radius - radius of hole at center
;; outer_radius - radius at center of teeth
;; width - width of gear
;; teeth - number of teeth
;; tooth_depth - depth of tooth

(define (gear inner-radius outer-radius width teeth tooth-depth)
  (let ((r0 inner-radius)
        (r1 (- outer-radius (/ tooth-depth 2.0)))
        (r2 (+ outer-radius (/ tooth-depth 2.0)))
        (da (* 2.0 (/ pi teeth 4.0))))
    (gl-shade-model GL_FLAT)
    (gl-normal 0.0 0.0 1.0)

    ;; draw front face
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 angle (* i 2.0 (/ pi teeth))
        (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
        (when (< i teeth)
          (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))
          (gl-vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width 0.5)))))
    (gl-end)

    ;; draw front sides of teeth
    (gl-begin GL_QUADS)
    (dotimes (i teeth)
      (let1 angle (* i 2.0 (/ pi teeth))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
        (gl-vertex (* r2 (cos (+ angle da)))
                   (* r2 (sin (+ angle da)))
                   (* width 0.5))
        (gl-vertex (* r2 (cos (+ angle (* 2 da))))
                   (* r2 (sin (+ angle (* 2 da))))
                   (* width 0.5))
        (gl-vertex (* r1 (cos (+ angle (* 3 da))))
                   (* r1 (sin (+ angle (* 3 da))))
                   (* width 0.5))))
    (gl-end)

    (gl-normal 0.0 0.0 -1.0)

    ;; draw back face
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 angle (* i 2.0 (/ pi teeth))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))
        (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))
        (when (< i teeth)
          (gl-vertex (* r1 (cos (+ angle (* 3 da))))
                     (* r1 (sin (+ angle (* 3 da))))
                     (* width -0.5))
          (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5)))))
    (gl-end)

    ;; draw back sides of teeth
    (gl-begin GL_QUADS)
    (dotimes (i teeth)
      (let1 angle (* i 2.0 (/ pi teeth))
        (gl-vertex (* r1 (cos (+ angle (* 3 da))))
                   (* r1 (sin (+ angle (* 3 da))))
                   (* width -0.5))
        (gl-vertex (* r2 (cos (+ angle (* 2 da))))
                   (* r2 (sin (+ angle (* 2 da))))
                   (* width -0.5))
        (gl-vertex (* r2 (cos (+ angle da)))
                   (* r2 (sin (+ angle da)))
                   (* width -0.5))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))))
    (gl-end)

    ;; draw outward faces of teeth
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i teeth)
      (let* ((angle (* i 2.0 (/ pi teeth)))
             (u (- (* r2 (cos (+ angle da))) (* r1 (cos angle))))
             (v (- (* r2 (sin (+ angle da))) (* r1 (sin angle))))
             (len (sqrt (+ (* u u) (* v v))))
             (uu (/ u len))
             (vv (/ v len)))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width 0.5))
        (gl-vertex (* r1 (cos angle)) (* r1 (sin angle)) (* width -0.5))
        (gl-normal v (- u) 0.0)
        (gl-vertex (* r2 (cos (+ angle da)))
                   (* r2 (sin (+ angle da)))
                   (* width 0.5))
        (gl-vertex (* r2 (cos (+ angle da)))
                   (* r2 (sin (+ angle da)))
                   (* width -0.5))
        (gl-normal (cos angle) (sin angle) 0.0)
        (gl-vertex (* r2 (cos (+ angle da da)))
                   (* r2 (sin (+ angle da da)))
                   (* width 0.5))
        (gl-vertex (* r2 (cos (+ angle da da)))
                   (* r2 (sin (+ angle da da)))
                   (* width -0.5))
        (gl-normal (- (* r1 (sin (+ angle da da da)))
                      (* r2 (sin (+ angle da da))))
                   (- (- (* r1 (cos (+ angle da da da)))
                      (* r2 (cos (+ angle da da)))))
                   0.0)
        (gl-vertex (* r1 (cos (+ angle da da da)))
                   (* r1 (sin (+ angle da da da)))
                   (* width 0.5))
        (gl-vertex (* r1 (cos (+ angle da da da)))
                   (* r1 (sin (+ angle da da da)))
                   (* width -0.5))
        (gl-normal (cos angle) (sin angle) 0.0)))
    (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width 0.5))
    (gl-vertex (* r1 (cos 0)) (* r1 (sin 0)) (* width -0.5))
    (gl-end)

    (gl-shade-model GL_SMOOTH)
    ;; draw inside radius cylinder
    (gl-begin GL_QUAD_STRIP)
    (dotimes (i (+ teeth 1))
      (let1 angle (* i 2.0 (/ pi teeth))
        (gl-normal (- (cos angle)) (- (sin angle)) 0.0)
        (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width -0.5))
        (gl-vertex (* r0 (cos angle)) (* r0 (sin angle)) (* width 0.5))))
    (gl-end)
    ))

(define *view-rotx* 20.0)
(define *view-roty* 30.0)
(define *view-rotz* 0.0)
(define *gear1* 0)
(define *gear2* 0)
(define *gear3* 0)
(define *angle* 0.0)
(define *timer* #f)
(define *frames* 0)

(define (draw widget . _)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (begin
        (gl-push-matrix)
        (gl-rotate *view-rotx* 1.0 0.0 0.0)
        (gl-rotate *view-roty* 0.0 1.0 0.0)
        (gl-rotate *view-rotz* 0.0 0.0 1.0)
        (begin
          (gl-push-matrix)
          (gl-translate -3.0 -2.0 0.0)
          (gl-rotate *angle* 0.0 0.0 1.0)
          (gl-call-list *gear1*)
          (gl-pop-matrix))
        (begin
          (gl-push-matrix)
          (gl-translate 3.1 -2.0 0.0)
          (gl-rotate (- (* -2.0 *angle*) 9.0) 0.0 0.0 1.0)
          (gl-call-list *gear2*)
          (gl-pop-matrix))
        (begin
          (gl-push-matrix)
          (gl-translate -3.1 4.2 0.0)
          (gl-rotate (- (* -2.0 *angle*) 25.0) 0.0 0.0 1.0)
          (gl-call-list *gear3*)
          (gl-pop-matrix))
        (gl-pop-matrix))
      (if (gdk-gl-drawable-is-double-buffered gldrawable)
          (gdk-gl-drawable-swap-buffers gldrawable)
          (gl-flush))
      (gdk-gl-drawable-gl-end gldrawable))

    (inc! *frames*)
    (let1 seconds (g-timer-elapsed *timer*)
      (when (>= seconds 5.0)
        (print #`",*frames* in ,seconds seconds = ,(/ *frames* seconds) FPS")
        (g-timer-reset *timer*)
        (set! *frames* 0)))
    #t))

;; new window size or exposure
(define (reshape widget . _)
  (let* ((glcontext (gtk-widget-get-gl-context widget))
         (gldrawable (gtk-widget-get-gl-drawable widget))
         (wsize (ref widget 'allocation))
         (h (/ (ref wsize 'height) (ref wsize 'width))))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-viewport 0 0 (ref wsize 'width) (ref wsize 'height))
      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (gl-frustum -1.0 1.0 (- h) h 5.0 60.0)
      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (gl-translate 0.0 0.0 -40.0)
      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    #t))

(define (init widget)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-light GL_LIGHT0 GL_POSITION '#f32(5.0 5.0 10.0 0.0))
      (gl-enable GL_CULL_FACE)
      (gl-enable GL_LIGHTING)
      (gl-enable GL_LIGHT0)
      (gl-enable GL_DEPTH_TEST)

      ;; make the gears
      (set! *gear1* (gl-gen-lists 1))
      (gl-new-list *gear1* GL_COMPILE)
      (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.8 0.1 0.0 1.0))
      (gear 1.0 4.0 1.0 20 0.7)
      (gl-end-list)

      (set! *gear2* (gl-gen-lists 1))
      (gl-new-list *gear2* GL_COMPILE)
      (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.0 0.8 0.2 1.0))
      (gear 0.5 2.0 2.0 10 0.7)
      (gl-end-list)

      (set! *gear3* (gl-gen-lists 1))
      (gl-new-list *gear3* GL_COMPILE)
      (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(0.2 0.2 1.0 1.0))
      (gear 1.3 2.0 0.5 10 0.7)
      (gl-end-list)

      (gl-enable GL_NORMALIZE)

      (print)
      (print #`"GL_RENDERER   = ,(gl-get-string GL_RENDERER)")
      (print #`"GL_VERSION    = ,(gl-get-string GL_VERSION)")
      (print #`"GL_VENDOR     = ,(gl-get-string GL_VENDOR)")
      (print #`"GL_EXTENSIONS = ,(gl-get-string GL_EXTENSIONS)")
      (print)

      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***

    ;; create timer
    (unless *timer* (set! *timer* (g-timer-new)))
    (g-timer-start *timer*)
    ))

(define (idle widget)
  (inc! *angle* 0.5)
  (if (> *angle* 360) (set! *angle* (fmod *angle* 360)))
  (gtk-widget-queue-draw widget)
  #t)

(define *idle-id* 0)

(define (map widget . _)
  (when (zero? *idle-id*)
    (set! *idle-id* (gtk-idle-add-priority GDK_PRIORITY_REDRAW
                                           (lambda _ (idle widget)))))
  #t)

(define (unmap widget . _)
  (unless (zero? *idle-id*)
    (gtk-idle-remove *idle-id*)
    (set! *idle-id* 0))
  #t)

(define (visible widget event)
  (if (= (ref event 'state) GDK_VISIBILITY_FULLY_OBSCURED)
      (unless (zero? *idle-id*)
              (gtk-idle-remove *idle-id*)
              (set! *idle-id* 0))
      (when (zero? *idle-id*)
        (set! *idle-id* (gtk-idle-add-priority GDK_PRIORITY_REDRAW
                                               (lambda _ (idle widget))))))
  #t)

;; change view angle, exit upon ESC
(define (key widget event)
  (let ((kv (ref event 'keyval))
        (q  (lambda () (gtk-widget-queue-draw widget))))
    (cond
     ((= kv GDK_KEY_z)      (set! *view-rotz* (fmod (+ *view-rotz* 5.0) 360)) (q))
     ((= kv GDK_KEY_Z)      (set! *view-rotz* (fmod (- *view-rotz* 5.0) 360)) (q))
     ((= kv GDK_KEY_Up)     (set! *view-rotx* (fmod (+ *view-rotx* 5.0) 360)) (q))
     ((= kv GDK_KEY_Down)   (set! *view-rotx* (fmod (- *view-rotx* 5.0) 360)) (q))
     ((= kv GDK_KEY_Left)   (set! *view-roty* (fmod (+ *view-roty* 5.0) 360)) (q))
     ((= kv GDK_KEY_Right)  (set! *view-roty* (fmod (- *view-roty* 5.0) 360)) (q))
     ((= kv GDK_KEY_Escape) (gtk-main-quit))))
  #t)

(define (main args)
  (gtk-init args)
  (unless (gdk-gl-query-extension)
    (error "*** OpenGL is not supported."))

  ;;
  ;; Configure OpenGL-capable visual.
  ;;
  (let1 glconfig (or (gdk-gl-config-new-by-mode (logior GDK_GL_MODE_RGB
                                                        GDK_GL_MODE_DEPTH
                                                        GDK_GL_MODE_DOUBLE))
                     (begin
                       (warn "*** Cannot find the double-buffered visual.\n*** Trying single-buffered visual.\n")
                       (gdk-gl-config-new-by-mode (logior GDK_GL_MODE_RGB
                                                          GDK_GL_MODE_DEPTH)))
                     (error "*** No appropriate OpenGL-capable visual found.")
                     )
    ;;
    ;; Top-level window.
    ;;
    (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
      (gtk-window-set-title window "gears")
      (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
      (g-signal-connect window "key_press_event" key)
      (g-signal-connect window "unmap_event" unmap)
      (let1 vbox (gtk-vbox-new #f 0)
        (gtk-container-add window vbox)
        (gtk-widget-show vbox)
        ;;
        ;; Drawing area for drawing OpenGL scene.
        ;;
        (let1 drawing-area (gtk-drawing-area-new)
          (gtk-widget-set-size-request drawing-area 300 300)
          ;; Set OpenGL-capability to the widget.
          (gtk-widget-set-gl-capability drawing-area glconfig #f #t
                                        GDK_GL_RGBA_TYPE)
          (gtk-box-pack-start vbox drawing-area #t #t 0)
          (gtk-widget-set-events drawing-area
                                 (logior GDK_EXPOSURE_MASK
                                         GDK_BUTTON_PRESS_MASK
                                         GDK_VISIBILITY_NOTIFY_MASK))
          (g-signal-connect drawing-area "realize" init)
          (g-signal-connect drawing-area "configure_event" reshape)
          (g-signal-connect drawing-area "expose_event" draw)
          (g-signal-connect drawing-area "map_event" map)
          (g-signal-connect drawing-area "unmap_event" unmap)
          (g-signal-connect drawing-area "visibility_notify_event" visible)
          (gtk-widget-show drawing-area))
        ;;
        ;; Simple quit button.
        ;;
        (let1 button (gtk-button-new-with-label "Quit")
          (gtk-box-pack-start vbox button #f #f 0)
          (g-signal-connect button "clicked" (lambda _ (gtk-main-quit)))
          (gtk-widget-show button))
        );vbox
      (gtk-widget-show window)
      )
    (gtk-main)
    0))
