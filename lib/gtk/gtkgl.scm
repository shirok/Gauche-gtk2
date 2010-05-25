;;;
;;; gtk/gtkgl.scm - GtkGLExt binding
;;;
;;;  Copyright(C) 2002,2004 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: gtkgl.scm,v 1.3 2007/01/13 01:36:31 maruska Exp $
;;;

(define-module gtk.gtkgl
  (use gtk)
  (export-all))
(select-module gtk.gtkgl)

(dynamic-load "gauche-gtkgl" :export-symbols #t)

;; Higher-level utilities

(define (with-gtkgl-context widget proc)
  (let ((gldrawable (gtk-widget-get-gl-drawable widget))
        (glcontext  (gtk-widget-get-gl-context widget)))
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (proc widget gldrawable glcontext)
      (gdk-gl-drawable-gl-end gldrawable))))

(provide "gtk/gtkgl")
