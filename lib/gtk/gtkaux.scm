;;;
;;; gtk/gtkaux.scm - Auxiliary defs
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
;;;  $Id: gtkaux.scm,v 1.4 2007/01/13 01:36:31 maruska Exp $
;;;

(select-module gtk)

;; GtkListStore --------------------------------------

(define (gtk-list-store-set list-store iter . args)
  (check-arg (cut is-a? <> <gtk-list-store>) list-store)
  (check-arg (cut is-a? <> <gtk-tree-iter>) iter)
  (unless (even? (length args))
    (error "even number of arguments required, but got" args))
  (let loop ((args args))
    (unless (null? args)
      (gtk-list-store-set-value list-store iter (car args) (cadr args))
      (loop (cddr args)))))

;; GtkTreeViewColumn ---------------------------------

(define (gtk-tree-view-column-new-with-attributes title renderer . args)
  (check-arg string? title)
  (check-arg (cut is-a? <> <gtk-cell-renderer>) renderer)
  (unless (even? (length args))
    (error "cell renderer option arguments must be even number"))
  (let1 column (gtk-tree-view-column-new)
    (gtk-tree-view-column-set-title column title)
    (gtk-tree-view-column-pack-start column renderer #t)
    (let loop ((args args))
      (unless (null? args)
        (gtk-tree-view-column-add-attribute column renderer (car args) (cadr args))
        (loop (cddr args))))
    column)
  )

;; GtkTreeSelection -----------------------------------

(define (gtk-tree-selection-get-selected-multi selection)
  (let ((sels '()))
    (gtk-tree-selection-selected-foreach
     selection
     (lambda (model path iter) (push! sels iter)))
    (reverse sels)))

(provide "gtk/gtkaux")
