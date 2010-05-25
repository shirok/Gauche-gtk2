#! /usr/bin/gosh

(use h2s.Persistence)




(define (main args)
  ;; lookup (recursively) the type, and show its ...
  (let ((db-file "/tmp/good.db"))

    (let1 open-database
        (lambda (filename)
          (let ((db (open-type-db filename))
                (sdb (open-struct-db filename)))

            (set! global-sdb sdb)
            (set! find-type-in-archive-function
                  (cut find-type-in-archive <> db))))
      ;; apply
      (if (slot-bound? recipe 'parsing-function)
          ((slot-ref recipe 'parsing-function)))
      ;;

  
      )))
