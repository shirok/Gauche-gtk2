;;
;; test gdk
;;

(use gauche.test)
(use srfi-1)
(add-load-path "../lib")

(test-start "Gdk")

(use gtk)

(test "gdk-init" '("test-gdk")
      (lambda () (gdk-init '("test-gdk"))))

(test-section "gdk-color")

(test "gdk-color" '((0 1 2 3) (1 4 5 6))
      (lambda ()
        (let ((c0 (make <gdk-color> :pixel 0 :red 1 :green 2 :blue 3))
              (c1 (make <gdk-color> :pixel 1 :red 4 :green 5 :blue 6)))
          (map (lambda (c) (map (cut slot-ref c <>) '(pixel red green blue)))
               (list c0 c1)))))

(test-section "gdk-cursor")

(test "gdk-cursor" GDK_ARROW
      (lambda () (slot-ref (gdk-cursor-new GDK_ARROW) 'type)))

(test-section "gdk-gc")

(define (gcvalues->alist v)
  (list :function          (ref v 'function)
        :fill              (ref v 'fill)
        :subwindow-mode    (ref v 'subwindow-mode)
        :ts-x-origin       (ref v 'ts-x-origin)
        :ts-y-origin       (ref v 'ts-y-origin)
        :clip-x-origin     (ref v 'clip-x-origin)
        :clip-y-origin     (ref v 'clip-y-origin)
        :graphics-exposures (ref v 'graphics-exposures)
        :line-width        (ref v 'line-width)
        :line-style        (ref v 'line-style)
        :cap-style         (ref v 'cap-style)
        :join-style        (ref v 'join-style)
        ))

(define gcv (make <gdk-gc-values>
              :function GDK_NAND :fill GDK_STIPPLED
              :subwindow-mode GDK_CLIP_BY_CHILDREN
              :ts-x-origin 40 :ts-y-origin 20
              :clip-x-origin 11 :clip-y-origin 10
              :graphics-exposures 0 :line-width 2
              :line-style GDK_LINE_ON_OFF_DASH :cap-style GDK_CAP_BUTT
              :join-style GDK_JOIN_BEVEL))

'(test "gdk-gc" (gcvalues->alist gcv)
      (lambda ()
        (let1 gc (gdk-gc-new-with-values
                                         gcv
                                         (logior GDK_GC_FUNCTION
                                                 GDK_GC_FILL
                                                 GDK_GC_SUBWINDOW
                                                 GDK_GC_TS_X_ORIGIN
                                                 GDK_GC_TS_Y_ORIGIN
                                                 GDK_GC_CLIP_X_ORIGIN
                                                 GDK_GC_CLIP_Y_ORIGIN
                                                 GDK_GC_EXPOSURES
                                                 GDK_GC_LINE_WIDTH
                                                 GDK_GC_LINE_STYLE
                                                 GDK_GC_CAP_STYLE
                                                 GDK_GC_JOIN_STYLE))
          (gcvalues->alist (gdk-gc-get-values gc)))))

(test-section "gdk-visual")

(test "gdk-visual" (sort (gdk-query-depths))
      (lambda ()
        (delete-duplicates
         (sort (map (cut slot-ref <> 'depth) (gdk-list-visuals))))))

(test "gdk-visual" (sort (gdk-query-visual-types))
      (lambda ()
        (delete-duplicates
         (sort (map (cut slot-ref <> 'type) (gdk-list-visuals))))))

(test-end)
