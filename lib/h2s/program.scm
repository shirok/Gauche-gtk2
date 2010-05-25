
;; just collect the USED modules

(define-module h2s.program
  (extend h2s.top


          h2s.objects
          h2s.parse
          h2s.emit
          h2s.gtk-types
          h2s.fixup
          h2s.utils

          h2s.persistence

                                        ;(use )
          ))
(select-module h2s.program)


(provide "h2s/program")
