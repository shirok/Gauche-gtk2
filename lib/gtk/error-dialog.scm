;;;
;;; gtk/error-dialog.scm - reports error via GUI dialog
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
;;;  $Id: error-dialog.scm,v 1.3 2007/01/13 01:36:31 maruska Exp $
;;;

;; this file is to be autoloaded.

;; makes an error to be reported using gtk dialog.

(define-module gtk.error-dialog
  (use gauche.mop.singleton)
  (use gauche.threads)
  (use gtk)
  (export gtk-scheme-enable-error-dialog <error-dialog>))
(select-module gtk.error-dialog)

(define-class <error-dialog> (<singleton-mixin>)
  ((widget)
   (label)
   (parent :init-keyword :parent :init-value #f)
   (flags  :init-keyword :flags  :init-value 0)
   ))

(define-method initialize ((self <error-dialog>) initargs)
  (next-method)
  (let* ((dialog (gtk-dialog-new-with-buttons "Error"
                                              (ref self 'parent)
                                              (ref self 'flags)
                                              GTK_STOCK_OK
                                              GTK_RESPONSE_ACCEPT))
         (vbox   (ref dialog 'vbox))
         (label  (gtk-label-new "")))
    (g-signal-connect dialog "response"
                      (lambda _ (gtk-widget-hide-all dialog)))
    (gtk-box-pack-start vbox label  #t #t 10)
    (slot-set! self 'widget dialog)
    (slot-set! self 'label label)
    ))

(define (report-error exc)
  (let ((self (instance-of <error-dialog>))
        (mesg (if (is-a? exc <error>)
                  #`"*** ERROR: ,(ref exc 'message)"
                  (x->string exc)))
        )
    (gtk-label-set-text (ref self 'label) mesg)
    (gtk-widget-show-all (ref self 'widget))))

(define (gtk-scheme-enable-error-dialog . maybe-parent)
  (make <error-dialog> :parent (get-optional maybe-parent #f))
  (vm-set-default-exception-handler (current-thread) report-error))

(provide "gtk/error-dialog")
