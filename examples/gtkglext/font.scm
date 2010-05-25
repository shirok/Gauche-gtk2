;;
;; Simple bitmap font rendering example.
;;
;; Ported from examples/font.c,
;; written by Naofumi Yasufuku  <naofumi@users.sourceforge.net>
;;

(use gauche.collection)
(use gauche.uvector)
(use gtk)
(use gtk.gtkgl)
(use gl)

(define *font-string* "courier 12")
(define *font-list-base* 0)
(define *font-height* 0)

(define-syntax prval
  (syntax-rules ()
    ((_ expr) (format #f "~s = ~a" 'expr expr))))

(define-syntax prattr
  (syntax-rules ()
    ((_ glconfig name bool?)
     (receive (status value) (gdk-gl-config-get-attrib glconfig name)
       (if status
           (format #f "~s = ~s" 'name
                   (if bool?
                       (not (zero? value))
                       value))
           (format #f "~s : failed to get attribute value" 'name))))))

(define (examine-gl-config-attrib glconfig)
  (print "\nOpenGL visual configurations :\n")
  (print (prval (gdk-gl-config-is-rgba glconfig)))
  (print (prval (gdk-gl-config-is-double-buffered glconfig)))
  (print (prval (gdk-gl-config-is-stereo glconfig)))
  (print (prval (gdk-gl-config-has-alpha glconfig)))
  (print (prval (gdk-gl-config-has-depth-buffer glconfig)))
  (print (prval (gdk-gl-config-has-accum-buffer glconfig)))
  (print)
  (print (prattr glconfig GDK_GL_USE_GL #t))
  (print (prattr glconfig GDK_GL_BUFFER_SIZE #f))
  (print (prattr glconfig GDK_GL_LEVEL       #f))
  (print (prattr glconfig GDK_GL_RGBA        #t))
  (print (prattr glconfig GDK_GL_DOUBLEBUFFER #t))
  (print (prattr glconfig GDK_GL_STEREO      #t))
  (print (prattr glconfig GDK_GL_AUX_BUFFERS #f))
  (print (prattr glconfig GDK_GL_RED_SIZE    #f))
  (print (prattr glconfig GDK_GL_GREEN_SIZE  #f))
  (print (prattr glconfig GDK_GL_BLUE_SIZE   #f))
  (print (prattr glconfig GDK_GL_ALPHA_SIZE  #f))
  (print (prattr glconfig GDK_GL_DEPTH_SIZE  #f))
  (print (prattr glconfig GDK_GL_STENCIL_SIZE #f))
  (print (prattr glconfig GDK_GL_ACCUM_RED_SIZE #f))
  (print (prattr glconfig GDK_GL_ACCUM_GREEN_SIZE #f))
  (print (prattr glconfig GDK_GL_ACCUM_BLUE_SIZE #f))
  (print (prattr glconfig GDK_GL_ACCUM_ALPHA_SIZE #f))
  (print)
  )

(define (init widget)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget))
        (wsize (ref widget 'allocation)))
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (let* ((font-list-base (gl-gen-lists 128))
             (font-desc (pango-font-description-from-string *font-string*))
             (font (gdk-gl-font-use-pango-font font-desc 0 128 font-list-base)))
        (unless font
          (errorf "*** Can't load font '~s'" *font-string*))
        (set! *font-list-base* font-list-base)
        (let1 font-metrics (pango-font-get-metrics font #f)
          (set! *font-height*
                (pango-pixels
                 (+ (pango-font-metrics-get-ascent font-metrics)
                    (pango-font-metrics-get-descent font-metrics))))))
      (gl-clear-color 1.0 1.0 1.0 1.0)
      (gl-clear-depth 1.0)
      (gl-viewport 0 0 (ref wsize 'width) (ref wsize 'height))
      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (gl-ortho 0.0 (ref wsize 'width) 0.0 (ref wsize 'height) -1.0 1.0)
      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    ))

(define (reshape widget . _)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget))
        (wsize (ref widget 'allocation)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-viewport 0 0 (ref wsize 'width) (ref wsize 'height))
      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (gl-ortho 0.0 (ref wsize 'width) 0.0 (ref wsize 'height) -1.0 1.0)
      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    #t))

;; this should be in Gauche core ...
(define (string->u8vector string)
  (with-builder (<u8vector> put! get :size (string-size string))
    (with-input-from-string string
      (lambda () (port-for-each put! read-byte)))
    (get)))

(define (display widget . _)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (gl-color 0.0 0.0 0.0)
      (do ((i 2 (- i 1)))
          ((< i -2))
        (gl-raster-pos 10.0
                       (+ (* 0.5 (ref (ref widget 'allocation) 'height))
                          (* i *font-height*)))
        (do ((j (char->integer #\space) (+ j 1)))
            ((> j (char->integer #\Z)))
          (gl-call-list (+ *font-list-base* j))))

      (gl-color 1.0 0.0 0.0)
      (gl-raster-pos 10.0 10.0)
      (gl-list-base *font-list-base*)
      (let1 array (string->u8vector *font-string*) ;;ugh...
        (gl-call-lists (size-of array) GL_UNSIGNED_BYTE array))
      (if (gdk-gl-drawable-is-double-buffered gldrawable)
          (gdk-gl-drawable-swap-buffers gldrawable)
          (gl-flush))
      (gdk-gl-drawable-gl-end gldrawable)
      ;;*** OpenGL END ***
      )
    #t))

(define (main args)
  (gtk-init args)
  (unless (gdk-gl-query-extension)
    (error "*** OpenGL is not suppotred.***"))
  (call-with-values gdk-gl-query-version
    (cut format #t "OpenGL is supported - version ~*~a.~a\n" <> <> <>))

  (let1 glconfig (or (gdk-gl-config-new-by-mode (logior GDK_GL_MODE_RGB
                                                        GDK_GL_MODE_DOUBLE))
                     (begin
                       (warn "*** Cannot find the double-buffered visual.\n*** Trying single-buffered visual.\n")
                       (gdk-gl-config-new-by-mode GDK_GL_MODE_RGB))
                     (error "*** No appropriate OpenGL-capable visual found.\n")
                     )
    (examine-gl-config-attrib glconfig)

    (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
      (gtk-window-set-title window "font")
      (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)) #f)
      (let1 vbox (gtk-vbox-new #f 0)
        (gtk-container-add window vbox)
        (gtk-widget-show vbox)
        (let1 drawing-area (gtk-drawing-area-new)
          (gtk-widget-set-size-request drawing-area 640 240)
          (gtk-widget-set-gl-capability drawing-area glconfig #f #t
                                        GDK_GL_RGBA_TYPE)
          (gtk-box-pack-start vbox drawing-area #t #t 0)
          (gtk-widget-set-events drawing-area
                                 (logior GDK_EXPOSURE_MASK GDK_BUTTON_PRESS_MASK))
          (g-signal-connect drawing-area "realize" init)
          (g-signal-connect drawing-area "configure_event" reshape)
          (g-signal-connect drawing-area "expose_event" display)
          (gtk-widget-show drawing-area))
        (let1 button (gtk-button-new-with-label "Quit")
          (gtk-box-pack-start vbox button #f #f 0)
          (g-signal-connect button "clicked" (lambda _ (gtk-main-quit)) #f)
          (gtk-widget-show button))
        )
      (gtk-widget-show window)
      ))
  (gtk-main)
  0)
