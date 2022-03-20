# Gauche-gtk2

This is a Gauche extension module to use GTK2.

Scheme binding is mostly generated automatically by parsing
GTK header files.   There are missing APIs and fields.  There
are also some APIs that are converted incorrectly.  In worst
case, you'll get core dump with some APIs of which stub code
treats pointers incorrectly.

If you find any deficiencies, please report them to the author
(If you're a member of gauche-devel mailing list, you can post
bug reports there.  If you're not, you can directly send them
to shiro at acm dot org).

The current version passes Scheme strings to Gtk as is; it
works OK if Gauche's native character encoding is UTF-8.
If you compiled Gauche with other native character encodings,
do not pass characters other than ASCII.  This restriction
will be removed in future versions, in which the strings
will be automatically converted.


## Requirements

- Gauche 0.9 or later
- Gtk 2.10 or later.
  These can be downloaded from http://www.gtk.org/ .
  Gtk 1.x is not supported.
- pkgconfig-0.12 or later.  If you have gtk2, you should
  already have it.

- Optionally, you can build GtkGLExt binding with Gauche-gtk.
  If you want it, you need gtkglext-0.6.0 or later.
  It can be downloaded from http://gtkglext.sourceforge.net/ .
  You'd want to have Gauche-gl as well to use gtkglext.

- Additionally, you can build a GLGD widget, which draws a graph
  using OpenGL using GtkGLExt.

- If you try to build from the git repository instead of tarball,
  you also need autoconf 2.54 or later.


## Building from tarball

```
  % ./configure
  % make
  % make install
```

`Configure` script finds the location of Gauche and Gtk2.

If you have gtkglext and want to build its binding, run
configure as this:

```
  % ./configure --enable-gtkgl
```

If you also want to build glgd (experimental), run
configure as this (it implies --enable-gtkgl).

```
  % ./configure --enable-glgd
```

GLGD uses Pango to display multilingual text on the OpenGL
screen.  If you want this feature, configure like this
instead:

```
  % ./configure --enable-glgd-pango
```

This uses PangoFT2.  I hope it works on recent Linux
distributions if you've set up font stuff correctly.
It'd be hassle to make it work unless you have XFree86 4.3
or later.


## Building from git repo

The source repository does not have machine-generated files.
Large number of *.stub files are autogenerated ones, and
you need to run 'make' for a separate target, 'make stubs',
to generate them.

```
  % autoconf
  % ./configure [configure options ... see above]
  % make stubs
  ... lots of messages ...
  % make
  % make install
```

## Usage

You can find some Scheme scripts ported from GTK examples
under 'examples/gtk-tutorial' directory.
There are also a few gtkglext examples under examples/gtkglext.

Most GTK/GDK/Pango classes and functions are mapped straightforward
to Scheme.  GTK class becomes Scheme class.  For example,

```
   C: struct GtkWidget  --> Scheme class: <gtk-widget>
   C: struct GtkVBox    --> Scheme class: <gtk-vbox>
```

Generally, fields of C structures are visible as slots in Scheme
class.   Some fields are read-only.  Some fields are invisible
from Scheme, usually because the proper accessor function hasn't
been written.

 NOTE: Some fields are visible from Scheme but shouldn't be.
 Don't rely too much on such fields; eventually the 'private'
 fields will be hidden from Scheme.  The rule of thumb is
 that if the GTk manual lists the field, then it'll always
 be available from Scheme.

 NOTE: Some Gtk structures have array fields.  Currently,
 you can view such fields as vectors from Scheme, but you
 can't modify them.  A special getter/setter for such
 fields will be provided.

Scheme procedures generally take the same arguments as C version,
with straightforward mapping.

```
   C:      GtkVBox *gtk_vbox_new(gboolean homogenous, gint spacing)
   Scheme: (gtk-vbox-new <boolean> <integer>) => <gtk-vbox>
```

If C version has 'out' arguments, i.e. the pointer arguments to
receive values from callee, the Scheme version returns such
values as an extra return values.

```
   C:      gboolean gtk_get_current_event_state(GdkModifierType *type)
   Scheme: (gtk-get-modifier-type) => <gboolean>, <integer>
```

```
   C:      void gtk-misc-get-padding(GtkMisc *misc, gint *xpad, gint *ypa)
   Scheme: (gtk-misc-get-padding <gtk-misc>) => <integer>, <integer>
```

Some GTK functions take a function pointer along a user data, to mimic
a closure.  In Scheme, such procedures just take a closure.

An important note for memory management: once Scheme obtains a pointer
to a GTk object, the GTk object won't be reclaimed automatically.
Instead, it should be destroyed explicitly, by `gtk-object-destroy`.
GTk objects tend to require heavyweight finalization, and it is not
a good idea to let Gauche's GC handle it.  (Alternatively, you can
call `g-object-unref` to tell GTk that you won't use that GTk object
from Scheme anymore.  GTk's reference counting mechanism then handles
object management properly).

Once a GTk object is destroyed, all Scheme pointers that have referred
the object becomes 'unreferenced' state.   Trying to use such Scheme
pointers signals an error.  You can check if the Scheme pointer is
in unreferenced state by `g-object-unreferenced?` procedure.

More specific tweaks:

* `g_signal_connect` : the function takes a closure (and no user data),
  so there's no 'swapped' variant.

* `g_object_get_data`, `g_object_set_data` : It is available in Scheme, but
  Scheme data is stored in the different location than `GObject` data.
  You can pass arbitrary Scheme object as a key, so there's no
  `g_object_{get|set}_qdata`.

If you're not sure about the specific API, take a look at the
corresponding stub file (e.g. `gtkentry.stub` for `GtkEntry`) in the
source directory.


## Interactive development

Usually you have to call `gtk-main` to make Gtk widgets work, which is
not very convenient for interactive development.  A new module
`gtk.listener` is added from Gauche-gtk 0.2.2 which supports
interative repl even while `gtk-main` is running.

To use listener, you simply need to call

```
  (gtk-scheme-listener-add)
```

before calling gtk-main.  You'll get Scheme prompt.