;;
;; Simple OpenGL Graph Display.  This program is in the public domain.
;;
;; Shawn Taras
;; $Id: simple.scm,v 1.21 2007/01/13 01:36:30 maruska Exp $

(use math.const)
(use math.mt-random)
(use gtk)
(use gtk.gtkgl)
(use gtk.glgd)
(use gl)

(define *attr-geometry* 0)
(define *attr-skeleton* 1)
(define *attr-current* GLGD_ATTR_FORCEVISIBLE)
(define *graph* (glgd-graph-create))
(define *mt* (make <mersenne-twister>))

;; GLGDGRAPH_FN_MOUSE_LEFT callback
;; --------------------------------
(define (mouse-left-callback graph node link event)
  (when (= (ref event 'type) GDK_BUTTON_PRESS)
    (if (= *attr-current* GLGD_ATTR_FORCEVISIBLE)
      (set! *attr-current* *attr-geometry*)
      (set! *attr-current* (+ *attr-current* 1)))
    (if (> *attr-current* *attr-skeleton*)
      (set! *attr-current* GLGD_ATTR_FORCEVISIBLE))
    (glgd-graph-attribute-clear graph)
    (glgd-graph-attribute-set graph *attr-current*)
    (glgd-graph-auto-organize graph 0.0 0.0)
    (print #`"*attr-current* now ,*attr-current*")
    (print #`"left mouse click on node ,(glgd-node-id-get node)")
    (print #`"left mouse click on link ,(glgd-graph-link-index graph link)"))
  #t)
  
;; GLGDGRAPH_FN_KEY callback
;; -------------------------
(define (key-callback graph node link event)
  (let1 kv (ref event 'keyval)
    (cond
     ((= kv GDK_Escape) (gtk-main-quit))))
  #t)

;; GLGDGRAPH_FN_PRERENDER callback
;; -------------------------------
(define (pre-draw-callback node)
  (glgd-node-color-set node
                       (mt-random-real *mt*)
                       (mt-random-real *mt*)
                       (mt-random-real *mt*) 1.0)
  #t)

(define (draw widget . _)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glgd-graph-draw *graph*)
      (if (gdk-gl-drawable-is-double-buffered gldrawable)
        (gdk-gl-drawable-swap-buffers gldrawable)
        (gl-flush))
      (gdk-gl-drawable-gl-end gldrawable))
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
      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    #t)
  (glgd-graph-reshape *graph*))

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

      (gl-enable GL_NORMALIZE)

      (print)
      (print #`"GL_RENDERER   = ,(gl-get-string GL_RENDERER)")
      (print #`"GL_VERSION    = ,(gl-get-string GL_VERSION)")
      (print #`"GL_VENDOR     = ,(gl-get-string GL_VENDOR)")
      (print #`"GL_EXTENSIONS = ,(gl-get-string GL_EXTENSIONS)")
      (print)

      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    ))

;; exit upon ESC 
(define (key widget event)
  (let ((kv (ref event 'keyval))
        (q  (lambda () (gtk-widget-queue-draw widget))))
    (cond
     ((= kv GDK_Escape) (gtk-main-quit))))
  #t)
  
;; create a simple graph
(define (glgd-graph-build-simple graph)
  (glgd-graph-init graph)
  (let* ((model (glgd-node-create))
         (geometry (glgd-node-create))
         (torso (glgd-node-create))
         (arms (glgd-node-create))
         (legs (glgd-node-create))
         (skeleton (glgd-node-create))
         (hip (glgd-node-create))
         (thighLeft (glgd-node-create))
         (thighRight (glgd-node-create)))
    (glgd-node-info-set model "model" 0)
    (glgd-node-attribute-set model *attr-geometry*)
    (glgd-node-info-set geometry "geometry" 1)
    (glgd-node-attribute-set geometry *attr-geometry*)
    (glgd-node-info-set skeleton "skeleton" 2)
    (glgd-node-attribute-set skeleton *attr-skeleton*)
    (glgd-node-info-set torso "torso" 3)
    (glgd-node-attribute-set torso *attr-geometry*)
    (glgd-node-info-set arms "arms" 4)
    (glgd-node-attribute-set arms *attr-geometry*)
    (glgd-node-info-set legs "legs" 5)
    (glgd-node-attribute-set legs *attr-geometry*)
    (glgd-node-info-set hip "hip" 6)
    (glgd-node-attribute-set hip *attr-skeleton*)
    (glgd-node-info-set thighLeft "thighLeft" 7)
    (glgd-node-attribute-set thighLeft *attr-skeleton*)
    (glgd-node-info-set thighRight "thighRight" 8)
    (glgd-node-attribute-set thighRight *attr-skeleton*)
    (glgd-graph-node-add graph model)
    (glgd-graph-node-add graph geometry)
    (glgd-graph-node-add graph skeleton)
    (glgd-graph-node-add graph torso)
    (glgd-graph-node-add graph arms)
    (glgd-graph-node-add graph legs)
    (glgd-graph-node-add graph hip)
    (glgd-graph-node-add graph thighLeft)
    (glgd-graph-node-add graph thighRight)
    (let* ((list (glgd-link-list-create))
           (m2g (glgd-link-create))
           (g2t (glgd-link-create))
           (g2a (glgd-link-create))
           (g2l (glgd-link-create))
           (m2s (glgd-link-create))
           (s2h (glgd-link-create))
           (h2tl (glgd-link-create))
           (h2tr (glgd-link-create))
           (tr2h (glgd-link-create))
           (tr2s (glgd-link-create)))
      (glgd-link-set m2g model geometry)
      (glgd-link-set g2t geometry torso)
      (glgd-link-set g2a geometry arms)
      (glgd-link-set g2l geometry legs)
      (glgd-link-set m2s model skeleton)
      (glgd-link-set s2h skeleton hip)
      (glgd-link-set h2tl hip thighLeft)
      (glgd-link-set h2tr hip thighRight)
      (glgd-link-set tr2h thighRight hip)
      (glgd-link-set tr2s thighRight skeleton)
      (glgd-graph-link-add graph list m2g)
      (glgd-graph-link-add graph list m2s)
      (glgd-graph-link-add graph list g2t)
      (glgd-graph-link-add graph list g2a)
      (glgd-graph-link-add graph list g2l)
      (glgd-graph-link-add graph list s2h)
      (glgd-graph-link-add graph list h2tl)
      (glgd-graph-link-add graph list h2tr)
      (glgd-graph-link-add graph list tr2h)
      (glgd-graph-link-add graph list tr2s)
      (glgd-graph-link-list-add graph list)))
  (glgd-graph-attribute-set graph *attr-current*)
  (glgd-graph-auto-organize graph 0.0 0.0)
  (glgd-graph-link-list-dump graph)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_PRERENDER pre-draw-callback)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_KEY key-callback)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_MOUSE_LEFT mouse-left-callback)
  #t)

(define (main args)
  (gtk-init args)
  (glgd-verbosity 1)
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
      (gtk-window-set-title window "Simple Graph Demo")
      (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
      (let1 vbox (gtk-vbox-new #f 0)
        (gtk-container-add window vbox)
        (gtk-widget-show vbox)
        ;;
        ;; Drawing area for drawing OpenGL scene.
        ;;
        (let1 drawing-area (gtk-drawing-area-new)
          (gtk-widget-set-size-request drawing-area 640 480)
          ;; Set OpenGL-capability to the widget.
          (gtk-widget-set-gl-capability drawing-area glconfig #f #t
                                        GDK_GL_RGBA_TYPE)
          (gtk-box-pack-start vbox drawing-area #t #t 0)
          (gtk-widget-set-events drawing-area
                                 (logior GDK_EXPOSURE_MASK
                                         GDK_VISIBILITY_NOTIFY_MASK))
          (g-signal-connect drawing-area "realize" init)
          (g-signal-connect drawing-area "configure_event" reshape)
          (g-signal-connect drawing-area "expose_event" draw)
          (glgd-graph-build-simple *graph*)
          (glgd-graph-connect *graph* drawing-area)
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
    (glgd-graph-fini *graph*)
    0))
