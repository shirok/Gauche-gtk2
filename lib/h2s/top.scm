
(define-module h2s.top
  (export
   <parse-2-stub>
   init-hardwired
   standard-parse-n-emit
   )
  
  
  (use h2s.mmc-utils)
  (use h2s.objects)
  (use h2s.gtk-types)
;  (use h2s.persistence)
  (use h2s.fixup)
  (use h2s.emit)

  ;; h2s.parse
  (use h2s.utils)
  )
(select-module h2s.top)

(define-class <parse-2-stub> ()
  (
   (hint-files :init-keyword :hint-files)

   (types-file :init-keyword :types-file)
   (inits-file :init-keyword :inits-file)
   (header-file :init-keyword :header-file)
   (include-file :init-keyword :include-file)
   (init-function :init-keyword :init-function)

   ;; parsing what?
   (parsing-function :init-keyword :parsing-function)
   ))

(define debug #f)
(define (init-hardwired)
  ;; define primitive types
  (for-each
      (lambda (entry)
        (if debug (logformat "init: ~a\n" (car entry)))
        (make <gtk-type> :c-name (car entry) :body (cadr entry)))
    '((gint       <int>)
      (gint8      <int8>)
      (gint16     <int16>)
      (gint32     <int32>)
      (glong      <long>)
      (gshort     <int16>)
      (guint      <uint>)
      (guint8     <uint8>)
      (guint16    <uint16>)
      (guint32    <uint32>)
      (gulong     <ulong>)
      (guchar     <uint8>)
      (gushort    <uint16>)
      (gboolean   <boolean>)
      (gfloat     <float>)
      (gdouble    <double>)
      (long       <long>)
      (int        <int>)
      (short      <int16>)
      (char       <int8>)
      (void       <void>)
      (float      <float>)
      (double     <double>)

      ;; mmc:
      (gunichar     <gunichar>)         ;uint32
                                        ;(PangoGlyphUnit  <int32>)
      ;; cp:  PangoGlyphItem PangoLayoutRun

      ;; C string business is tricky.  We can only treat the case that
      ;; passing const char * or const gchar * - in those cases, gtk copies
      ;; the passed string immediately, so we can safely pass the string
      ;; from ScmGetStringConst*.
      (const-char*  <const-char*>)
      (const-gchar* <const-gchar*>)
      ;; Generic GObject
      (GObject* <g-object>)
      ;; mmc: Will this solve it?
      (GObject <g-object>)
      ;; This is used to box the returned allocated gchar*
      (gchar*       <gchar*>)
      ;; Opaque types
      (PangoContext* <pango-context>)
      (PangoLanguage* <pango-language>)
      (PangoAttrList* <pango-attr-list>)
      (PangoLayoutIter* <pango-layout-iter>)
      (GdkAtom    <gdk-atom>)
      (GdkRegion* <gdk-region>)
      (GdkPixbufFormat* <gdk-pixbuf-format>)
      (GtkTreePath* <gtk-tree-path>)
      (GtkTreeRowReference* <gtk-tree-row-reference>)
      ;; GdkEvent is a union.
      (GdkEvent*  <gdk-event>)
      ;; GtkAllocation is simply an alias of GdkRectangle
      (GtkAllocation* <gdk-rectangle>)
      ;; Interfaces
      (GtkEditable* <gtk-editable>)
      (GtkTreeModel* <gtk-tree-model>)
      (GtkTreeSortable* <gtk-tree-sortable>))))


(define (standard-parse-n-emit recipe input-db output-db)
  (init-hardwired)
  (let1 open-database (lambda (filename)
                        ;; (logformat "using GTK definitsion in (BDB) file ~a\n" filename)
                        (let ((db (open-type-db filename))
                              (sdb (open-struct-db filename)))
                          (set! global-sdb sdb)
                          (set! find-type-in-archive-function
                                (cut find-type-in-archive <> db))))
    (if input-db (open-database input-db))
    ;; apply
    (if (slot-bound? recipe 'parsing-function)
        ((slot-ref recipe 'parsing-function)))
    ;;

    (report "Fixing up ...")
    (fixup (slot-ref recipe 'hint-files))
    (report "Generating ...")
      
    (emit-all
     (ref recipe 'types-file)
     (ref recipe 'header-file)
     (ref recipe 'include-file)
     (ref recipe 'init-function)
     (ref recipe 'inits-file))
    ;; 
    ;(if output-db (dump-all output-db))
    ))



(provide "h2s/top")
