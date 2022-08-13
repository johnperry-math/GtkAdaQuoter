with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Glib; use Glib;
with Glib.Object; use Glib.Object;
with Glib.Values;

with Gdk.Event;       use Gdk.Event;

with Gtk.Handlers; use Gtk.Handlers;
with Gtk.List_Store; use Gtk.List_Store;
with Gtk.Tree_Model; use Gtk.Tree_Model;
with Gtk.Widget;      use Gtk.Widget;


with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

package Gtk_Utility is

   function Delete_Main_Window_Cb
      (Self  : access Gtk_Widget_Record'Class;
       Event : Gdk.Event.Gdk_Event)
       return Boolean;

   procedure Editing_Done
      (Self     : access Glib.Object.GObject_Record'Class;
       Path     : UTF8_String;
       New_Text : UTF8_String
      );

   function Save_Quotes_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean;

end Gtk_Utility;
