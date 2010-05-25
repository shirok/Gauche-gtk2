;;;
;;; gtk.scm - Gauche GTk binding
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: gtk.scm,v 1.9 2007/01/13 01:36:31 maruska Exp $
;;;

(define-module gtk
  (export-all)
  (use srfi-4)
  (use gauche.charconv)
  )
(select-module gtk)

(dynamic-load "gauche-gtk" :export-symbols #t)

(require "gtk/gtkaux")
(require "gtk/gdkkeysyms")

(autoload "gtk/listener" gtk-scheme-listener-add)

(define gpointer-mapping (make-hash-table 'string=?))
;; mapping  "signal-name"   ->  ( ( index . type) ....)
;; todo: C part depends on this, so it should be defined there! 

(provide "gtk")
