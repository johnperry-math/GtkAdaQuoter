-- Callback functions for the UI elements.
-- GtkAda's architecture requires callbacks to appear in a separate module.

-- Ada packages
with Ada.Strings.Unbounded;

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
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;

-- Ada packages
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;      use ADa.Strings.Unbounded;

-- my packages
with Quote_Structure; use Quote_Structure;

package Callbacks is
-- Callbacks and other utility functions for the UI elements.

   Configuration_File: constant String := ".gtkadaquoter_cfg";
   -- filename for the configuration file

   Field_Names: array (Fields) of Ada.Strings.Unbounded.Unbounded_String
      := (To_Unbounded_String("Author"),
          To_Unbounded_String("Speaker"),
          To_Unbounded_String("Source"),
          To_Unbounded_String("Quotation")
         );
   -- for easily mapping the fields to strings in the UI

   type Column_Renderer_Array is array(Fields) of Gtk_Cell_Renderer_Text;

   type Shutdown_GObject_Record is new GObject_Record with record
      Window: GTK_Window;
      Tree_View: Gtk_Tree_View;
      Column_Renderers: Column_Renderer_Array;
   end record;

   type Shutdown_GObject is access all Shutdown_GObject_Record;
   -- used to track main window and tree view

   function Initialize
      (Window   : Gtk_Window;
       Tree_View: Gtk_Tree_View;
       Renderers: Column_Renderer_Array
      ) return Shutdown_GObject;

   function Delete_Main_Window_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event)
       return Boolean;
   -- Callback for when the user attmpts to close the main window.
   -- This implementation ignores `Self` and `Event`.

   function Get_Source_File(Path: String) return String;

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

   function Quit_Button_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean;
   -- Callback for quitting.
   -- `Self` should be a `Gtk_Window`;
   -- `Event` is ignored.

   function Quit_Button_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean;

   function Window_Key_Release_Cb
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key
     ) return Boolean;

end Callbacks;
