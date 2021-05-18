;;
;; test gtk
;;

(use gauche.test)
(use srfi-1)
(add-load-path "../lib")

;; NB: I don't have comprehensive framework for to automate
;; testing of interactive parts.  This test just calls as many
;; APIs as possible, to catch silly errors.

(test-start "Gtk")

(use gtk)

(test "gtk-init" '("test-gtk") (lambda () (gtk-init '("test-gtk"))))

(test-section "built-in classes")

;; test for each class - WRITEME

(test-section "inheritance")

(define-class <xlabel> (<gtk-label>) ((a)))

(test "subclass of <gtk-label>" #t
      (lambda ()
        (let1 xlabel (make <xlabel>)
          (and (is-a? xlabel <gtk-label>)
               (not (eq? (class-of xlabel) <gtk-label>))))))

(test "subclass of <gtk-label>" #t
      (lambda ()
        (let1 xlabel (make <xlabel>)
          (and (not (slot-bound? xlabel 'a))
               (begin (set! (ref xlabel 'a) 3)
                      (= (slot-ref xlabel 'a) 3))
               (begin (gtk-label-set-text xlabel "foo")
                      (equal? (gtk-label-get-text xlabel) "foo")
                      (= (slot-ref xlabel 'a) 3))))))

;; subclassing <gtk-entry> involves multiple inheritance (direct-supers
;; has to be working to succeed)
(define-class <xentry> (<gtk-entry>) ((a)))

(test "subclass of <gtk-entry>" #t
      (lambda ()
        (let1 xentry (make <xentry>)
          (and (is-a? xentry <gtk-entry>)
               (is-a? xentry <gtk-editable>)
               (not (eq? (class-of xentry) <gtk-entry>))))))

;; further multiple inheritance
(define-class <xmixin> () ((b)))

(define-class <xtreestore> (<xmixin> <gtk-tree-store>) ((a)))

(test "subclass of <gtk-tree-store>" #t
      (lambda ()
        (let1 xts (make <xtreestore>)
          (and (is-a? xts <gtk-tree-store>)
               (is-a? xts <xmixin>)
               (is-a? xts <gtk-tree-model>)
               (is-a? xts <gtk-tree-sortable>)
               (is-a? xts <gtk-tree-drag-source>)
               (is-a? xts <gtk-tree-drag-dest>)))))

(test-section "memory management")

;; check if ScmGObject which is not referenced except from GObject
;; is kept alive.
(test "scmobject reference" #t
      (let ((wv (make-weak-vector 1)))
        (lambda ()
          (let ((w (gtk-hbox-new #f 0)))
            (let1 c (gtk-label-new "")
              (gtk-box-pack-start w c #f #f 0)
              (weak-vector-set! wv 0 c))
            (dotimes (n 10) (dotimes (m 10) (make-list 10)) (gc))
            (eq? (weak-vector-ref wv 0)
                 (car (gtk-container-get-children w)))
            ))))

;; NB: The following tests checks GTk objects and associated Scheme objects
;; are GC-ed properly.  However, currently there's a debugging code in
;; gauche-gtk.c that tracks GTk objects in a hash table, causing the tests
;; to fail.  We temporarily disable those tests; make sure to enable them
;; after we get rid of the debugging table.

;; check for the callback closure is collected at the right moment
(define wv (make-weak-vector 1))
(define wb #f)

(define (callback-bind-test callback)
  (let ((w (gtk-button-new)))
    (set! wb w)
    (g-signal-connect w "clicked" callback)
    (weak-vector-set! wv 0 callback)
    ))

'(test "callback" '(#t #f)
      (lambda ()
        (callback-bind-test (lambda _ 'foo))
        (dotimes (n 10) (make-list 10) (gc))
        (let1 pre (procedure? (weak-vector-ref wv 0))
          ;(set! wb #f)
          (g-object-unref wb)
          (dotimes (n 10) (make-list 10) (gc))
          (list pre (procedure? (weak-vector-ref wv 0))))))

;; see if destruction properly causes unreferencing
(test "destroy" #t
      (lambda ()
        (let ((w (gtk-button-new)))
          (gtk-object-destroy w)
          (g-object-unreferenced? w))))

;; see if destruction causes reclamation
'(test "destroy -> GC" #f
      (lambda ()
        (let ((wv (make-weak-vector 1)))
          (let ((w (gtk-button-new)))
            (weak-vector-set! wv 0 w)
            (gtk-object-destroy w))
          (dotimes (n 10) (gc))
          (weak-vector-ref wv 0))))

;; see if explicit destruction breaks cyclic reference
'(test "destroy -> GC (cyclic)" '(#f #f)
      (lambda ()
        (let ((wv (make-weak-vector 2)))
          (let ((w (gtk-button-new)))
            (weak-vector-set! wv 0 w)
            (let ((cb (lambda _ w)))
              (g-signal-connect w "clicked" cb)
              (weak-vector-set! wv 1 cb))
            (gtk-object-destroy w))
          ;; NB: tricky --- remove stale reference from old environment
          ;; by those variables.  Won't work if compiler becomes
          ;; intelligent enough to remove unused variables.
          (let* ((a 0) (b 0) (c 0) (d 0) (e 0)
                 (a 0) (b 0) (c 0) (d 0) (e 0)
                 (a 0) (b 0) (c 0) (d 0) (e 0))
            (dotimes (n 10) (make-list 100) (gc))
            (list (weak-vector-ref wv 0)
                  (weak-vector-ref wv 1))))
        ))

;; see if destruction causes reclamation of entire tree
'(test "destroy widget tree" '(#f #f)
      (lambda ()
        (let ((wv (make-weak-vector 2)))
          (let* ((box (gtk-hbox-new #f 0))
                 (b0  (gtk-button-new))
                 (b1  (gtk-button-new)))
            (gtk-box-pack-start box b0 #f #f 0)
            (gtk-box-pack-start box b1 #f #f 0)
            (weak-vector-set! wv 0 b0)
            (weak-vector-set! wv 1 b1)
            (gtk-object-destroy box))
          ;; NB: tricky --- remove stale reference from old environment
          ;; by those variables.  Won't work if compiler becomes
          ;; intelligent enough to remove unused variables.
          (let* ((a 0) (b 0) (c 0) (d 0) (e 0)
                 (a 0) (b 0) (c 0) (d 0) (e 0)
                 (a 0) (b 0) (c 0) (d 0) (e 0))
            (dotimes (n 10) (make-list 100) (gc))
            (list (weak-vector-ref wv 0)
                  (weak-vector-ref wv 1))))
        ))

(test-end)
