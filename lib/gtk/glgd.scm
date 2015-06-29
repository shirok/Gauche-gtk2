;;;
;;; gtk/glgd.scm - openGL Graph Display binding
;;;
;;;  Copyright(C) 2004 by Shawn Taras (shawn_t@cementedminds.com)
;;;  Copyright(C) 2004 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: glgd.scm,v 1.5 2007/01/13 01:36:31 maruska Exp $
;;;

(define-module gtk.glgd
  (use gl)
  (use gtk)
  (use gtk.gtkgl)
  (export-all))
(select-module gtk.glgd)

(dynamic-load "gauche-glgd" :export-symbols #t)

;; Higher-level utilities

;; Class <gtk-graph-area>
;;   Binds glgd-graph and gtk-drawing-area conveniently.

(define-class <gtk-graph-area> (<gtk-drawing-area>)
  ((glconfig :init-keyword :glconfig
             :init-form (or (gdk-gl-config-new-by-mode
                             (logior GDK_GL_MODE_RGB GDK_GL_MODE_DEPTH
                                     GDK_GL_MODE_DOUBLE))
                            (gdk-gl-config-new-by-mode
                             (logior GDK_GL_MODE_RGB GDK_GL_MODE_DEPTH))
                            (error "Required visual not supported")))
   (graph    :init-keyword :graph
             :init-form (glgd-graph-create))

   ;; temporary
   (connected :init-value #f)
   ))

(define-method initialize ((self <gtk-graph-area>) initargs)
  (next-method)
  ;; Set OpenGL-capability to the widget.
  (gtk-widget-set-gl-capability self (ref self 'glconfig) #f #t
                                GDK_GL_RGBA_TYPE)
  ;; Initial event mask.
  (gtk-widget-set-events self (logior GDK_EXPOSURE_MASK
                                      GDK_VISIBILITY_NOTIFY_MASK))
  ;; Default event handlings
  (g-signal-connect self "destroy"
                    (^[w . _]
                      (glgd-graph-fini (ref self 'graph))))
  (g-signal-connect self "realize"
                    (^[w . _]
                      (with-gtkgl-context self gtk-graph-area-initialize)))
  (g-signal-connect self "configure_event"
                    (^[w . _]
                      (with-gtkgl-context self gtk-graph-area-configure)))
  (g-signal-connect self "expose_event"
                    (^[w . _]
                      (with-gtkgl-context self gtk-graph-area-draw)))
  (g-signal-connect self "map_event"
                    (^[w . _]
                      (with-gtkgl-context self gtk-graph-area-mapped)))

  )

(define-method gtk-graph-area-initialize ((self <gtk-graph-area>)
                                          gldrawable glcontext)
  (gl-enable GL_DEPTH_TEST))

(define-method gtk-graph-area-configure ((self <gtk-graph-area>)
                                         gldrawable glcontext)
  (let1 wsize (ref self 'allocation)
    (gl-viewport 0 0 (ref wsize 'width) (ref wsize 'height))
    #t))

(define-method gtk-graph-area-draw ((self <gtk-graph-area>)
                                    gldrawable glcontext)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glgd-graph-draw (ref self 'graph))
  (if (gdk-gl-drawable-is-double-buffered gldrawable)
    (gdk-gl-drawable-swap-buffers gldrawable)
    (gl-flush)))

(define-method gtk-graph-area-mapped ((self <gtk-graph-area>)
                                      gldrawable glcontext)
  (unless (and (ref self 'graph) (ref self 'connected))
    (glgd-graph-connect (ref self 'graph) self)
    (set! (ref self 'connected) #t))
  (gtk-widget-queue-draw self)
  #t)
