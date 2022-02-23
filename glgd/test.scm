(add-load-path "../gtkgl")

(use gtk)

(use gauche.test)
(test-start "GtkGLGD")

(use gtk.glgd)
(test-module 'gtk.glgd)


(test-end)
