;;;
;;; gtk/listener.scm - Listener
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
;;;  $Id: listener.scm,v 1.5 2007/01/13 01:36:31 maruska Exp $
;;;

;; this file is to be autoloaded

(select-module gtk)
(use gauche.listener)

(define (gtk-scheme-listener-add . opts)
  (let* ((iport (get-keyword :input-port opts (current-input-port)))
         (prompter (get-keyword :prompter opts
                                (lambda () (display "gosh-gtk> "))))
         (id #f)
         (user-finalizer (get-keyword :finalizer opts values))
         (listener (apply make <listener>
                          :finalizer (lambda ()
                                       (gtk-input-remove id)
                                       (user-finalizer))
                          :prompter prompter
                          opts))
         (handler (listener-read-handler listener)))
    (set! (port-buffering iport) :none)
    (set! id (gtk-input-add iport GDK_INPUT_READ
                            (lambda (port flags) (handler))))
    (listener-show-prompt listener)))

(provide "gtk/listener")
