;;
;; OpenGL Graph Display demo of a class hierarchy.
;; This program is in the public domain.
;;
;; Shawn Taras

;;
(use math.const)
(use gauche.charconv)
(use gtk)
(use gtk.gtkgl)
(use gtk.glgd)
(use gl)

(define *attr-link* 0)
(define *attr-link-field* 1)
(define *attr-src-node-field* 2)
(define *attr-dst-node-field* 3)
(define *graph* (glgd-graph-create))
(define *menu-bg* #f)
(define *dialog* #f)
(define *label* #f)

;; Menu callback routines
;; ----------------------
(define (menu-cb-frame-all event . _)
  (glgd-graph-frame *graph*)
  #t)

(define (menu-cb-expand-all event . _)
  (glgd-graph-attribute-set *graph* *attr-link-field*)
  (glgd-graph-attribute-set *graph* *attr-src-node-field*)
  (glgd-graph-attribute-set *graph* *attr-dst-node-field*)
  (glgd-graph-auto-organize *graph* 0.0 0.0)
  #t)

(define (menu-cb-collapse-all event . _)
  (glgd-graph-attribute-reset *graph* *attr-link-field*)
  (glgd-graph-attribute-reset *graph* *attr-src-node-field*)
  (glgd-graph-attribute-reset *graph* *attr-dst-node-field*)
  (glgd-graph-auto-organize *graph* 0.0 0.0)
  #t)

;; Dialog utility routines
;; -----------------------
(define (show-dialog dialog label txt)
  (gtk-label-set-text label txt)
  (gtk-window-set-position dialog GTK_WIN_POS_MOUSE)
  (gtk-widget-show dialog)
  #t)

;; GLGDGRAPH_FN_MOUSE_HOVER callback
;; ---------------------------------
(define (mouse-hover-callback graph node link event)
  (glgd-graph-node-list-flag graph GLGDNODE_FLAG_HILITE GLGD_FLAGOP_CLEAR)
  (if (= (glgd-graph-link-index graph link) -1)
    (if (>= (glgd-node-id-get node) -1)
      (glgd-node-flags-set node GLGDNODE_FLAG_HILITE GLGD_FLAGOP_SET)))
  #t)

;; GLGDGRAPH_FN_MOUSE_LEFT callback
;; --------------------------------
(define (mouse-left-callback graph node link event)
  (if (= (ref event 'type) GDK_BUTTON_PRESS)
    (case (glgd-node-id-get node)
      ((0) (glgd-graph-attribute-toggle graph *attr-link-field*)
       (glgd-graph-auto-organize graph 0.0 0.0))
      ((1) (show-dialog *dialog* *label* #`"Flags: ???"))
      ((2) (show-dialog *dialog* *label* #`"Attributes: ???"))
      ((3) (glgd-graph-attribute-toggle graph *attr-src-node-field*)
       (glgd-graph-auto-organize graph 0.0 0.0))
      ((4) (glgd-graph-attribute-toggle graph *attr-dst-node-field*)
       (glgd-graph-auto-organize graph 0.0 0.0))
      ((5) (show-dialog *dialog* *label* #`"Next Link: ???"))
      ((6) (show-dialog *dialog* *label* #`"Previous Link: ???"))
      ((7) (show-dialog *dialog* *label* #`"Flags: ???"))
      ((8) (show-dialog *dialog* *label* #`"Label: ???"))
      ((9) (show-dialog *dialog* *label* #`"Node ID: ,(glgd-node-id-get node)"))
      ((10) (show-dialog *dialog* *label* #`"Position: ???"))
      ((11) (show-dialog *dialog* *label* #`"Color: ???"))
      ((12) (show-dialog *dialog* *label* #`"Data: ???"))
      ((13) (show-dialog *dialog* *label* #`"Next Node: ???"))
      ((14) (show-dialog *dialog* *label* #`"Previous Node: ???"))
      ((15) (show-dialog *dialog* *label* #`"Flags: ???"))
      ((16) (show-dialog *dialog* *label* #`"Label: ???"))
      ((17) (show-dialog *dialog* *label* #`"Node ID: ,(glgd-node-id-get node)"))
      ((18) (show-dialog *dialog* *label* #`"Position: ???"))
      ((19) (show-dialog *dialog* *label* #`"Color: ???"))
      ((20) (show-dialog *dialog* *label* #`"Data: ???"))
      ((21) (show-dialog *dialog* *label* #`"Next Node: ???"))
      ((22) (show-dialog *dialog* *label* #`"Previous Node: ???"))))

  (if (= (ref event 'type) GDK_BUTTON_RELEASE)
    (gtk-widget-hide *dialog*))
  #t)

;; GLGDGRAPH_FN_MOUSE_RIGHT callback
;; --------------------------------
(define (mouse-right-callback graph node link event)
  (if (= (glgd-graph-link-index graph link) -1)
    (gtk-menu-popup *menu-bg* #f #f #f (ref event 'button) (ref event 'time)))
  #t)

;; GLGDGRAPH_FN_KEY callback
;; -------------------------
(define (key-callback graph node link event)
  (let1 kv (ref event 'keyval)
    (cond
     ((= kv GDK_KEY_Escape) (gtk-main-quit))))
  #t)

(define (draw widget . _)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (glgd-graph-draw *graph*)
      (if (gdk-gl-drawable-is-double-buffered gldrawable)
        (gdk-gl-drawable-swap-buffers gldrawable)
        (gl-flush))
      (gdk-gl-drawable-gl-end gldrawable))
    #t))

;; new window size or exposure
(define (reshape widget . _)
  (let* ((glcontext (gtk-widget-get-gl-context widget))
         (gldrawable (gtk-widget-get-gl-drawable widget))
         (wsize (ref widget 'allocation))
         (h (/ (ref wsize 'height) (ref wsize 'width))))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-viewport 0 0 (ref wsize 'width) (ref wsize 'height))
      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***
    #t)
  (glgd-graph-reshape *graph*))

(define (init widget)
  (let ((glcontext (gtk-widget-get-gl-context widget))
        (gldrawable (gtk-widget-get-gl-drawable widget)))
    ;;*** OpenGL BEGIN ***
    (when (gdk-gl-drawable-gl-begin gldrawable glcontext)
      (gl-light GL_LIGHT0 GL_POSITION '#f32(5.0 5.0 10.0 0.0))
      (gl-enable GL_CULL_FACE)
      (gl-enable GL_LIGHTING)
      (gl-enable GL_LIGHT0)
      (gl-enable GL_DEPTH_TEST)

      (gl-enable GL_NORMALIZE)

      (print)
      (print #`"GL_RENDERER   = ,(gl-get-string GL_RENDERER)")
      (print #`"GL_VERSION    = ,(gl-get-string GL_VERSION)")
      (print #`"GL_VENDOR     = ,(gl-get-string GL_VENDOR)")
      (print #`"GL_EXTENSIONS = ,(gl-get-string GL_EXTENSIONS)")
      (print)

      (gdk-gl-drawable-gl-end gldrawable))
    ;;*** OpenGL END ***

    ))

;; =========================================================================
;; create a graph representing the <glgdLink> and <glgdNode> class hierarchy
;; =========================================================================
(define (glgd-graph-build-class graph)
  (glgd-graph-init graph)
  (let* ((link (glgd-node-create))
         (flags (glgd-node-create))
         (attributes (glgd-node-create))
         (src-node (glgd-node-create))
         (dst-node (glgd-node-create))
         (next-link (glgd-node-create))
         (prev-link (glgd-node-create))
         (src-node-flags (glgd-node-create))
         (src-node-label (glgd-node-create))
         (src-node-id (glgd-node-create))
         (src-node-pos (glgd-node-create))
         (src-node-col (glgd-node-create))
         (src-node-data (glgd-node-create))
         (src-node-next (glgd-node-create))
         (src-node-prev (glgd-node-create))
         (dst-node-flags (glgd-node-create))
         (dst-node-label (glgd-node-create))
         (dst-node-id (glgd-node-create))
         (dst-node-pos (glgd-node-create))
         (dst-node-col (glgd-node-create))
         (dst-node-data (glgd-node-create))
         (dst-node-next (glgd-node-create))
         (dst-node-prev (glgd-node-create)))
    (glgd-node-info-set link
                        (ces-convert "Link (\u03a0\u03b1\u03bd\u8a9eFT2)"
                                     #f "utf8")
                        0)
    (glgd-node-attribute-set link *attr-link*)
    (glgd-node-info-set flags "flags" 1)
    (glgd-node-attribute-set flags *attr-link-field*)
    (glgd-node-info-set attributes "attributes" 2)
    (glgd-node-attribute-set attributes *attr-link-field*)
    (glgd-node-info-set src-node "src" 3)
    (glgd-node-attribute-set src-node *attr-link-field*)
    (glgd-node-info-set dst-node "dst" 4)
    (glgd-node-attribute-set dst-node *attr-link-field*)
    (glgd-node-info-set next-link "next" 5)
    (glgd-node-attribute-set next-link *attr-link-field*)
    (glgd-node-info-set prev-link "prev" 6)
    (glgd-node-attribute-set prev-link *attr-link-field*)
    (glgd-node-info-set src-node-flags "flags" 7)
    (glgd-node-attribute-set src-node-flags *attr-src-node-field*)
    (glgd-node-info-set src-node-label "label" 8)
    (glgd-node-attribute-set src-node-label *attr-src-node-field*)
    (glgd-node-info-set src-node-id "id" 9)
    (glgd-node-attribute-set src-node-id *attr-src-node-field*)
    (glgd-node-info-set src-node-pos "pos" 10)
    (glgd-node-attribute-set src-node-pos *attr-src-node-field*)
    (glgd-node-info-set src-node-col "col" 11)
    (glgd-node-attribute-set src-node-col *attr-src-node-field*)
    (glgd-node-info-set src-node-data "data" 12)
    (glgd-node-attribute-set src-node-data *attr-src-node-field*)
    (glgd-node-info-set src-node-next "next" 13)
    (glgd-node-attribute-set src-node-next *attr-src-node-field*)
    (glgd-node-info-set src-node-prev "prev" 14)
    (glgd-node-attribute-set src-node-prev *attr-src-node-field*)
    (glgd-node-info-set dst-node-flags "flags" 15)
    (glgd-node-attribute-set dst-node-flags *attr-dst-node-field*)
    (glgd-node-info-set dst-node-label "label" 16)
    (glgd-node-attribute-set dst-node-label *attr-dst-node-field*)
    (glgd-node-info-set dst-node-id "id" 17)
    (glgd-node-attribute-set dst-node-id *attr-dst-node-field*)
    (glgd-node-info-set dst-node-pos "pos" 18)
    (glgd-node-attribute-set dst-node-pos *attr-dst-node-field*)
    (glgd-node-info-set dst-node-col "col" 19)
    (glgd-node-attribute-set dst-node-col *attr-dst-node-field*)
    (glgd-node-info-set dst-node-data "data" 20)
    (glgd-node-attribute-set dst-node-data *attr-dst-node-field*)
    (glgd-node-info-set dst-node-next "next" 21)
    (glgd-node-attribute-set dst-node-next *attr-dst-node-field*)
    (glgd-node-info-set dst-node-prev "prev" 22)
    (glgd-node-attribute-set dst-node-prev *attr-dst-node-field*)
    (glgd-node-color-set flags 0 0 0.7 1)
    (glgd-node-color-set attributes 0 0 0.7 1)
    (glgd-node-color-set src-node 0 0 0.7 1)
    (glgd-node-color-set dst-node 0 0 0.7 1)
    (glgd-node-color-set next-link 0 0 0.7 1)
    (glgd-node-color-set prev-link 0 0 0.7 1)
    (glgd-node-color-set src-node-flags 0.7 0 0.2 1)
    (glgd-node-color-set src-node-label 0.7 0 0.2 1)
    (glgd-node-color-set src-node-id 0.7 0 0.2 1)
    (glgd-node-color-set src-node-pos 0.7 0 0.2 1)
    (glgd-node-color-set src-node-col 0.7 0 0.2 1)
    (glgd-node-color-set src-node-data 0.7 0 0.2 1)
    (glgd-node-color-set src-node-next 0.7 0 0.2 1)
    (glgd-node-color-set src-node-prev 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-flags 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-label 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-id 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-pos 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-col 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-data 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-next 0.7 0 0.2 1)
    (glgd-node-color-set dst-node-prev 0.7 0 0.2 1)
    (glgd-graph-node-add graph link)
    (glgd-graph-node-add graph flags)
    (glgd-graph-node-add graph attributes)
    (glgd-graph-node-add graph src-node)
    (glgd-graph-node-add graph dst-node)
    (glgd-graph-node-add graph next-link)
    (glgd-graph-node-add graph prev-link)
    (glgd-graph-node-add graph src-node-flags)
    (glgd-graph-node-add graph src-node-label)
    (glgd-graph-node-add graph src-node-id)
    (glgd-graph-node-add graph src-node-pos)
    (glgd-graph-node-add graph src-node-col)
    (glgd-graph-node-add graph src-node-data)
    (glgd-graph-node-add graph src-node-next)
    (glgd-graph-node-add graph src-node-prev)
    (glgd-graph-node-add graph dst-node-flags)
    (glgd-graph-node-add graph dst-node-label)
    (glgd-graph-node-add graph dst-node-id)
    (glgd-graph-node-add graph dst-node-pos)
    (glgd-graph-node-add graph dst-node-col)
    (glgd-graph-node-add graph dst-node-data)
    (glgd-graph-node-add graph dst-node-next)
    (glgd-graph-node-add graph dst-node-prev)
    (let* ((list (glgd-link-list-create))
           (l2f (glgd-link-create))
           (l2a (glgd-link-create))
           (l2sn (glgd-link-create))
           (l2dn (glgd-link-create))
           (l2nl(glgd-link-create))
           (l2pl (glgd-link-create))
           (sn2f (glgd-link-create))
           (sn2l (glgd-link-create))
           (sn2i (glgd-link-create))
           (sn2pos (glgd-link-create))
           (sn2c (glgd-link-create))
           (sn2d (glgd-link-create))
           (sn2n (glgd-link-create))
           (sn2p (glgd-link-create))
           (dn2f (glgd-link-create))
           (dn2l (glgd-link-create))
           (dn2i (glgd-link-create))
           (dn2pos (glgd-link-create))
           (dn2c (glgd-link-create))
           (dn2d (glgd-link-create))
           (dn2n (glgd-link-create))
           (dn2p (glgd-link-create)))

      (glgd-link-set l2f link flags)
      (glgd-link-set l2a link attributes)
      (glgd-link-set l2sn link src-node)
      (glgd-link-set l2dn link dst-node)
      (glgd-link-set l2nl link next-link)
      (glgd-link-set l2pl link prev-link)
      (glgd-link-set sn2f src-node src-node-flags)
      (glgd-link-set sn2l src-node src-node-label)
      (glgd-link-set sn2i src-node src-node-id)
      (glgd-link-set sn2pos src-node src-node-pos)
      (glgd-link-set sn2c src-node src-node-col)
      (glgd-link-set sn2d src-node src-node-data)
      (glgd-link-set sn2n src-node src-node-next)
      (glgd-link-set sn2p src-node src-node-prev)
      (glgd-link-set dn2f dst-node dst-node-flags)
      (glgd-link-set dn2l dst-node dst-node-label)
      (glgd-link-set dn2i dst-node dst-node-id)
      (glgd-link-set dn2pos dst-node dst-node-pos)
      (glgd-link-set dn2c dst-node dst-node-col)
      (glgd-link-set dn2d dst-node dst-node-data)
      (glgd-link-set dn2n dst-node dst-node-next)
      (glgd-link-set dn2p dst-node dst-node-prev)

      (glgd-graph-link-add graph list l2pl)
      (glgd-graph-link-add graph list l2nl)
      (glgd-graph-link-add graph list l2dn)
      (glgd-graph-link-add graph list l2sn)
      (glgd-graph-link-add graph list l2a)
      (glgd-graph-link-add graph list l2f)

      (glgd-graph-link-add graph list sn2p)
      (glgd-graph-link-add graph list sn2n)
      (glgd-graph-link-add graph list sn2d)
      (glgd-graph-link-add graph list sn2c)
      (glgd-graph-link-add graph list sn2pos)
      (glgd-graph-link-add graph list sn2i)
      (glgd-graph-link-add graph list sn2l)
      (glgd-graph-link-add graph list sn2f)

      (glgd-graph-link-add graph list dn2p)
      (glgd-graph-link-add graph list dn2n)
      (glgd-graph-link-add graph list dn2d)
      (glgd-graph-link-add graph list dn2c)
      (glgd-graph-link-add graph list dn2pos)
      (glgd-graph-link-add graph list dn2i)
      (glgd-graph-link-add graph list dn2l)
      (glgd-graph-link-add graph list dn2f)
      (glgd-graph-link-list-add graph list)))
  (glgd-graph-attribute-set graph *attr-link*)
  (glgd-graph-attribute-set graph *attr-link-field*)
  (glgd-graph-auto-organize graph 0.0 0.0)
  (glgd-graph-link-list-dump graph)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_KEY key-callback)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_MOUSE_HOVER mouse-hover-callback)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_MOUSE_LEFT mouse-left-callback)
  (glgd-graph-callback-set graph GLGDGRAPH_FN_MOUSE_RIGHT mouse-right-callback)
  #t)

(define (main args)
  (gtk-init args)
  (unless (gdk-gl-query-extension)
    (error "*** OpenGL is not supported."))

  ;;
  ;; Configure OpenGL-capable visual.
  ;;
  (let1 glconfig (or (gdk-gl-config-new-by-mode (logior GDK_GL_MODE_RGB
                                                        GDK_GL_MODE_DEPTH
                                                        GDK_GL_MODE_DOUBLE))
                     (begin
                       (warn "*** Cannot find the double-buffered visual.\n*** Trying single-buffered visual.\n")
                       (gdk-gl-config-new-by-mode (logior GDK_GL_MODE_RGB
                                                          GDK_GL_MODE_DEPTH)))
                     (error "*** No appropriate OpenGL-capable visual found.")
                     )
    ;;
    ;; Top-level window.
    ;;
    (let1 window (gtk-window-new GTK_WINDOW_TOPLEVEL)
      (gtk-window-set-title window "Class Graph Demo")
      (g-signal-connect window "delete_event" (lambda _ (gtk-main-quit)))
      (let1 vbox (gtk-vbox-new #f 0)
        (gtk-container-add window vbox)
        (gtk-widget-show vbox)
        ;;
        ;; Drawing area for drawing OpenGL scene.
        ;;
        (let1 drawing-area (gtk-drawing-area-new)
          (gtk-widget-set-size-request drawing-area 640 480)
          ;; Set OpenGL-capability to the widget.
          (gtk-widget-set-gl-capability drawing-area glconfig #f #t
                                        GDK_GL_RGBA_TYPE)
          (gtk-box-pack-start vbox drawing-area #t #t 0)
          (gtk-widget-set-events drawing-area
                                 (logior GDK_EXPOSURE_MASK
                                         GDK_VISIBILITY_NOTIFY_MASK))
          (g-signal-connect drawing-area "realize" init)
          (g-signal-connect drawing-area "configure_event" reshape)
          (g-signal-connect drawing-area "expose_event" draw)
          (glgd-graph-build-class *graph*)
          (glgd-graph-connect *graph* drawing-area)
          (gtk-widget-show drawing-area))
        ;;
        ;; Simple quit button.
        ;;
        (let1 button (gtk-button-new-with-label "Quit")
          (gtk-box-pack-start vbox button #f #f 0)
          (g-signal-connect button "clicked" (lambda _ (gtk-main-quit)))
          (gtk-widget-show button))
        );vbox

      ;; Initialize dialogs
      ;; ------------------
      (set! *dialog* (gtk-window-new GTK_WINDOW_POPUP))
      (gtk-window-set-decorated *dialog* #f)
      (gtk-widget-set-size-request *dialog* 112 32)
      (gtk-container-set-border-width *dialog* 2)
      (let1 frame (gtk-frame-new #f)
        (gtk-container-add *dialog* frame)
        (gtk-frame-set-shadow-type frame GTK_SHADOW_ETCHED_OUT)
        (gtk-widget-show frame)
        (let1 vbox (gtk-vbox-new #f 0)
          (gtk-container-add frame vbox)
          (gtk-widget-show vbox)
          (set! *label* (gtk-label-new "label"))
          (gtk-box-pack-start vbox *label* #t #t 0)
          (gtk-widget-show *label*)))

      ;; Initialize pop-up menus
      ;; -----------------------
      (set! *menu-bg* (gtk-menu-new))
      (let* ((str #`"Frame All")
             (menu-item (gtk-menu-item-new-with-label str)))
        (gtk-menu-shell-append *menu-bg* menu-item)
        (g-signal-connect menu-item "activate" menu-cb-frame-all)
        (gtk-widget-show menu-item))
      (let1 separator (gtk-separator-menu-item-new)
        (gtk-menu-shell-append *menu-bg* separator)
        (gtk-widget-show separator))
      (let* ((str #`"Expand All")
             (menu-item (gtk-menu-item-new-with-label str)))
        (gtk-menu-shell-append *menu-bg* menu-item)
        (g-signal-connect menu-item "activate" menu-cb-expand-all)
        (gtk-widget-show menu-item))
      (let* ((str #`"Collapse All")
             (menu-item (gtk-menu-item-new-with-label str)))
        (gtk-menu-shell-append *menu-bg* menu-item)
        (g-signal-connect menu-item "activate" menu-cb-collapse-all)
        (gtk-widget-show menu-item))

      ;; Show the window
      ;; ---------------
      (gtk-widget-show window)
      )
    (gtk-main)
    (glgd-graph-fini *graph*)
    0))
