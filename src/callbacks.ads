-- Callback functions for the UI elements.
-- GtkAda's architecture requires callbacks to appear in a separate module.

-- Glib packages

with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Values;

-- Gdk packages

with Gdk.Event;       use Gdk.Event;

-- Gtk packages

with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Widget;             use Gtk.Widget;

-- Ada packages
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;



package Callbacks is
   -- Callback functions for the UI elements.

   function Delete_Main_Window_Cb
      (Self  : access Gtk_Widget_Record'Class;
       Event : Gdk.Event.Gdk_Event)
       return Boolean;
   -- Callback for when the user attmpts to close the main window.
   -- This implementation ignores `Self` and `Event`.

   procedure Editing_Done
      (Self     : access Glib.Object.GObject_Record'Class;
       Path     : UTF8_String;
       New_Text : UTF8_String
      );
   -- Callback for completing the edit of a `Tree_View` cell.
   -- `Self` should be a `Gtk_Tree_View`,
   -- `Path` is a `UTF8_String` that points to the desired cell
   --     (we use this in `Get_Iter_From_String`), and
   -- `New_Text` is the new text for the cell.

   function Save_Quotes_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean;
   -- Callback for saving the quotes in Json format.
   -- `Self` should be a `Gtk_List_Store`;
   -- `Event` is ignored.
   -- Uses a GTK file chooser dialog to prompt for file name.

   function Save_Quotes_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean;

   function Add_Quote_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean;
   -- Callback for adding a quote to the list.
   -- `Self` should be a `Gtk_Tree_View`;
   -- `Event` is ignored.
   -- A new row will appear after the currently highlighted one.

   function Add_Quote_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean;

   function Del_Quote_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean;
   -- Callback for removing a quote from the list.
   -- `Self` should be a `Gtk_Tree_View`;
   -- `Event` is ignored.
   -- The currently higlighted row will be removed.

   function Del_Quote_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean;

end Callbacks;
