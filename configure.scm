#!/usr/bin/env gosh
;;
;; NOTE: This will eventually replace autoconf-generated 'configure'.  At this
;; moment, running this file requires Gauche 0.9.16 (unreleased), so we keep
;; this on the side for the reference.
;;


;; Configuring Gauche-gtk2
;; Run ./configure (or gosh ./configure) to generate Makefiles.

(use gauche.configure)
(use file.util)

;; Here you can define handlers of configure arguments by cf-arg-enable
;; and cf-arg-with.  Note that --with-local is handled implicitly if you use
;; cf-init-gauche-extension.


;; Initialize configure.  This creates the global context, parses
;; command-line args and sets up default values.
(cf-init-gauche-extension)

(cf-config-headers "src/gtk-config.h")

(cf-path-xtra)

;; Here you can add feature tests and other cf-define's.
($ cf-arg-enable 'gtkgl
   (cf-help-string "--enable-gtkgl" "Enable gtkglext interface.")
   (^_ (cf-define 'HAVE_GTKGL 1)))

($ cf-arg-enable 'glgd
   (cf-help-string "--enable-glgd" "Enable glgd interface.")
   (^_ (cf-define 'HAVE_GTKGL 1)
       (cf-define 'HAVE_GLGD 1)))

($ cf-arg-enable 'glgd-pango
   (cf-help-string "--enable-glgd-pango" "Enable glgd interface with Pango.")
   (^_ (cf-define 'HAVE_GTKGL 1)
       (cf-define 'HAVE_GLGD 1))
       (cf-define 'HAVE_GLGD_PANGO 1))

(cf-subst 'GTKGL_SO  (if (cf-defined? 'HAVE_GTKGL) "gauche-gtkgl.so" ""))
(cf-subst 'GTKGL_LIB (if (cf-defined? 'HAVE_GTKGL) "-lgtkgl" ""))
(cf-subst 'GTKGL_SCM (if (cf-defined? 'HAVE_GTKGL) "gtk/gtkgl.scom" ""))
(cf-subst 'PANGOFT2_LIB (if (cf-defined? 'HAVE_GLGD_PANGO) "-lpangoft2-1.0" ""))
(cf-subst 'GLGD_SO  (if (cf-defined? 'HAVE_GLGD) "gauche-glgd.so" ""))
(cf-subst 'GLGD_LIB (if (cf-defined? 'HAVE_GLGD) "-lglgd" ""))
(cf-subst 'GLGD_SCM (if (cf-defined? 'HAVE_GLGD) "gtk/glgd.scm" ""))

;; TRANSIENT: Current Makefile uses subst GENSTUB.  It will be unnecessary
;; once we start using this configure script---if we can use this, we're
;; using Gauche 0.9.16 or later, so we don't need to support older Gauche.
(cf-subst 'GENSTUB "tools/genstub")

(let1 gen-files
    (append-map file->string-list
                '("src/GDKFILES" "src/GDKPIXBUFFILES" "src/PANGOFILES"))
  (define (map-join proc strs)
    (string-join (map proc strs) " "))
  (cf-subst 'GEN_OBJS
            (map-join (cut path-swap-extension <> (cf-ref 'OBJEXT)) gen-files))
  (cf-subst 'GEN_SRCS
            (map-join (cut path-swap-extension <> "c") gen-files))
  (cf-subst 'GEN_STUBS
            (map-join (cut path-swap-extension <> "stub") gen-files))
  )

;; Output
(cf-output-default)

;; Local variables:
;; mode: scheme
;; end:
