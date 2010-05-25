/*
 * simple gtk test
 */

#include "gauche-gtk.h"

extern void Scm_Init_gauche_gtk(void);

int main(int argc, char **argv)
{
    GtkWidget *window;
    
    gtk_init (&argc, &argv);
    Scm_Init();
    Scm_Init_gauche_gtk();

    window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_widget_show(window);

    Scm_Printf(SCM_CURERR, "%S\n", Scm_MakeGObject(SCM_CLASS_GOBJECT, G_OBJECT(window)));
    /*gtk_main ();*/
    return 0;
}
