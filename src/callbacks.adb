-- Ada packages
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

-- Glib packages
with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;

-- Gtk packages
with Gdk.Types;
with Gdk.Types.Keysyms;

-- Gtk packages
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Dialog;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.List_Store;          use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;

-- GnatColl packages
with GnatColl.Json;
with GnatColl.VFS;

-- GNAT-specific packages
with GNAT.OS_Lib;

-- my packages
with Quote_Structure; use all type Quote_Structure.Fields;

package body Callbacks is

   function Initialize
      (Window   : Gtk_Window;
       Tree_View: Gtk_Tree_View;
       Renderers: Column_Renderer_Array
      ) return Shutdown_GObject
   is
      Object: Shutdown_GObject := new Shutdown_GObject_Record;
   begin
      Initialize(GObject(Object));
      Object.Window := Window;
      Object.Tree_View := Tree_View;
      Object.Column_Renderers := Renderers;
      return Object;
   end Initialize;

   function Delete_Main_Window_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event)
       return Boolean
   -- closes the main window
   is
      -- let's make our lives a little easier
      subtype JSON_Array is GnatColl.Json.Json_Array;
      subtype JSON_Value is GnatColl.Json.JSON_Value;
      package Tio renames Ada.Text_IO;

      Dir_Sep         : Character := GNAT.OS_Lib.Directory_Separator;
      Config_Filename : String := GnatColl.VFS.
         Get_Home_Directory.
            Display_Full_Name(Normalize => True) & Dir_Sep
         & Configuration_File;
      Config_File     : Tio.File_Type;

      Window_And_View: Shutdown_GObject := Shutdown_GObject(Self);

      Tree_View: Gtk_Tree_View := Window_And_View.Tree_View;
      Author_Column: Gtk_Tree_View_Column
         := Tree_View.Get_Column(Fields'Pos(Author));
      Speaker_Column: Gtk_Tree_View_Column
         := Tree_View.Get_Column(Fields'Pos(Speaker));
      Source_Column: Gtk_Tree_View_Column
         := Tree_View.Get_Column(Fields'Pos(Source));
      Quotation_Column: Gtk_Tree_View_Column
         := Tree_View.Get_Column(Fields'Pos(Quotation));

      Output: JSON_Value;

      procedure Set_Field_Integer
         (Val        : JSON_Value;
          Field_Name : UTF8_String;
          Field      : Integer) renames GnatColl.JSON.Set_Field;

   begin

      Output := GnatColl.JSON.Create_Object;

      Output.Set_Field
         (To_String(Field_Names(Author)), Integer(Author_Column.Get_Width));
      Output.Set_Field
         (To_String(Field_Names(Source)), Integer(Source_Column.Get_Width));
      Output.Set_Field
         (To_String(Field_Names(Speaker)), Integer(Speaker_Column.Get_Width));
      Output.Set_Field
         (To_String(Field_Names(Quotation)),Integer(Quotation_Column.Get_Width));

      declare
         Corner_X, Corner_Y, Width, Height: Glib.Gint;
         Window: Gtk_Window := Window_And_View.Window;
      begin
         Window.Get_Size(Width, Height);
         Window.Get_Position(Corner_X, Corner_Y);
         Output.Set_Field("width", Integer(Width));
         Output.Set_Field("height", Integer(Height));
         Output.Set_Field("corner_x", Integer(Corner_X));
         Output.Set_Field("corner_y", Integer(Corner_Y));
      end;

      Tio.Create(Config_File, Name => Config_Filename);
      Tio.Put(Config_File, GnatColl.JSON.Write(Output, False));
      Tio.Close(Config_File);

      Gtk.Main.Main_Quit;
      return False;
   end Delete_Main_Window_Cb;

   procedure Editing_Done
      ( Self     : access Glib.Object.GObject_Record'Class;
        Path     : UTF8_String;
        New_Text : UTF8_String
       )
         -- handles the response to editing a text cell in a tree view
   is

      -- we pass in the tree view as the object
      Tree_View  : Gtk_Tree_View := Shutdown_GObject(Self).Tree_View;

      -- get the tree model, which is a list store;
      -- To_Object converts from the Interface type to the List_Store type
      Model      : Gtk_Tree_Model := Get_Model( Tree_View );
      Store      : Gtk_List_Store := Gtk_List_Store( To_Object(Model) );

      -- find the row
      Iter       : Gtk_Tree_Iter := Get_Iter_From_String(Model, Path);

      -- to fill in with statements
      Cell_Value : Glib.Values.GValue;
      Tree_Path  : Gtk.Tree_Model.Gtk_Tree_Path;
      Tree_Column: Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Column_Id  : Glib.Gint;
      Row_Id     : Glib.Gint;

      subtype Fields is Quote_Structure.Fields;

   begin

      -- initialize cell to hold a string, then put the desired string into it
      Glib.Values.Init(Cell_Value, Glib.GType_String);
      Glib.Values.Set_String(Cell_Value, New_Text);

      -- find the active cell in the tree view
      -- (which should be the one we want to update)
      Get_Cursor(Tree_View, Tree_Path, Tree_Column);
      Column_Id := Tree_Column.Get_Sort_Column_Id;
      Row_Id := Tree_Path.Get_Indices(0);

      -- finally set the tree view's value to keep the value we wanted
      Set_Value(Store, Iter, Column_Id, Cell_Value);
      Tree_View.Set_Cursor(Tree_Path, Tree_View.Get_Column(Column_Id), True);

      -- move to next cell and make it editable
      Column_Id := ( Column_Id + 1 ) mod ( Fields'Pos(Fields'Last) + 1);
      if Column_Id = 0 then Tree_Path.Next; end if;

      Tree_View.Set_Cursor
         (Tree_Path, Tree_View.Get_Column(Column_Id), True);
      Tree_View.Grab_Focus;

      --  Get_Cursor(Tree_View, Tree_Path, Tree_Column);
      --  declare
      --     Renderer: Gtk_Cell_Renderer_Text := Renderers(Fields'Val(Column_Id));
      --     Color   : Gdk_RGBA;
      --     Parsed  : Boolean;
      --  begin
      --     Parse(Color, "rgba(255,0,0,0.5)", Parsed);
      --     Set_Property(Renderer, Cell_Background_Rgba_Property, Color);
      --  end;

      Gtk.Tree_Model.Path_Free(Tree_Path);

   end Editing_Done;

   function Get_Source_File(Path: String) return String
   is
      -- let's make our lives a little easier
      package Dialog renames Gtk.Dialog;
      use all type Dialog.Gtk_Response_Type;
      package File_Chooser renames Gtk.File_Chooser;
      package FCD renames Gtk.File_Chooser_Dialog;

      -- the dialog
      Filename_Dialog: FCD.Gtk_File_Chooser_Dialog;
      -- needed only to discard return value
      Discard        : Gtk.Widget.Gtk_Widget;
      Success        : Boolean;
   begin

      -- create chooser dialog
      FCD.Gtk_New
         (
          Dialog    => Filename_Dialog,
          Title     => "Json input path",
          Parent    => null,
          Action    => File_Chooser.Action_Open
         );

      -- add save, cancel buttons
      Discard := Dialog.Add_Button
         (
          Dialog      => Dialog.Gtk_Dialog(Filename_Dialog),
          Text        => "_Cancel",
          Response_Id => Dialog.Gtk_Response_Cancel
         );
      Discard := Dialog.Add_Button
         (
          Dialog      => Dialog.Gtk_Dialog(Filename_Dialog),
          Text        => "_Open",
          Response_Id => Dialog.Gtk_Response_Accept
         );

      Success := FCD.Set_Current_Folder(Filename_Dialog, Path);

      -- run the dialog and react accordingly
      -- (if the user cancels we don't do anything)
      if FCD.Run(Filename_Dialog) = Dialog.Gtk_Response_Accept then
         declare Result: String := FCD.Get_Filename(Filename_Dialog);
         begin
            Gtk.Widget.Destroy(Gtk.Widget.Gtk_Widget(Filename_Dialog));
            return Result;
         end;
      end if;

      -- don't forget to destroy the dialog or it will stay there
      Gtk.Widget.Destroy(Gtk.Widget.Gtk_Widget(Filename_Dialog));
      return "";

   end Get_Source_File;

   function Save_Quotes_Cb
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   -- handles the response to pressing the save button"
   -- this calls the mnemonic version, below
   is ( Save_Quotes_Mnemonic_Cb(Self, True) );

   function Save_Quotes_Mnemonic_Cb
     (Self: access Glib.Object.GObject_Record'Class;
      Arg : Boolean)
      return Boolean
   -- opens a file dialog that requests a filename,
   -- then (if the user accepts and confirms any overwrite) saves the file
   is

      -- let's make our lives a little easier
      subtype JSON_Array is GnatColl.Json.Json_Array;
      subtype JSON_Value is GnatColl.Json.JSON_Value;

      -- relevant UI elements
      Store : Gtk_List_Store := Gtk_List_Store( Self );
      Model : Gtk_Tree_Model := To_Interface(Store);

      -- start from first quote
      Iter  : Gtk_Tree_Iter := Get_Iter_First(Model);

      -- quote-related
      use all type Quote_Structure.Quote;
      use all type Quote_Structure.Quote_Vector;
      Data  : Quote_Structure.Quote_Vector;

   begin

      -- loop through quotes, adding them to Data
      while Iter /= Null_Iter loop

         declare
            subtype Fields is Quote_Structure.Fields;
            Author_String : UTF8_String
               := Store.Get_String(Iter, Fields'Pos(Author));
            Speaker_String: UTF8_String
               := Store.Get_String(Iter, Fields'Pos(Speaker));
            Text_String   : UTF8_String
               := Store.Get_String(Iter, Fields'Pos(Source));
            Quote_String  : UTF8_String
               := Store.Get_String(Iter, Fields'Pos(Quotation));
            This_Quote    : Quote_Structure.Quote
               := New_Quote
                  .With_Field(Author, Author_String)
                  .With_Field(Speaker, Speaker_String)
                  .With_Field(Source, Text_String)
                  .With_Field(Quotation, Quote_String);

         begin
            Add_Quote(This_Quote, Data);
         end;

         Next(Model, Iter);
      end loop;

      -- pop up a filename chooser dialog to get the filename
      declare

         -- let's make our lives a little easier
         package Dialog renames Gtk.Dialog;
         use all type Dialog.Gtk_Response_Type;
         package File_Chooser renames Gtk.File_Chooser;
         package FCD renames Gtk.File_Chooser_Dialog;

         -- the dialog
         Filename_Dialog: FCD.Gtk_File_Chooser_Dialog;
         -- needed only to discard return value
         Discard: Gtk.Widget.Gtk_Widget;

      begin

         -- create chooser dialog
         FCD.Gtk_New
            (
             Dialog    => Filename_Dialog,
             Title     => "Json output path",
             Parent    => null,
             Action    => File_Chooser.Action_Save
            );

         -- add save, cancel buttons
         Discard := Dialog.Add_Button
            (
             Dialog      => Dialog.Gtk_Dialog(Filename_Dialog),
             Text        => "_Cancel",
             Response_Id => Dialog.Gtk_Response_Cancel
            );
         Discard := Dialog.Add_Button
            (
             Dialog      => Dialog.Gtk_Dialog(Filename_Dialog),
             Text        => "_Save",
             Response_Id => Dialog.Gtk_Response_Accept
            );

         -- make sure user is OK with overwriting old file
         FCD.Set_Do_Overwrite_Confirmation(Filename_Dialog, True);
         FCD.Set_Current_Name(Filename_Dialog, "new_signatures.json");

         -- run the dialog and react accordingly
         -- (if the user cancels we don't do anything)
         if FCD.Run(Filename_Dialog) = Dialog.Gtk_Response_Accept then
            declare Filename: UTF8_String := FCD.Get_Filename(Filename_Dialog);
            begin
               Write_Quotes(Data, Filename);
            end;
         end if;

         -- don't forget to destroy the dialog or it will stay there
         Gtk.Widget.Destroy(Gtk.Widget.Gtk_Widget(Filename_Dialog));

      end;

      return False;

   end Save_Quotes_Mnemonic_Cb;

   procedure Get_Store_And_Iter
      (View  : Gtk_Tree_View;
       Store : out Gtk_List_Store;
       Iter  : out Gtk_Tree_Iter
      )
         -- convenience function to prevents repetition of code
         -- (and Therefore Bugs);
         -- gets `View`'s `Gtk_List_Store` and
         -- currently highlighted `Gtk_Tree_Iter` in `View`;
         -- stores them in `Store` and `Iter`
   is
      Model   : Gtk_Tree_Model := View.Get_Model;
      Path    : Gtk_Tree_Path;
      Column  : Gtk_Tree_View_Column;

   begin
      Store := Gtk_List_Store(To_Object(Model));
      View.Get_Cursor(Path, Column);
      Iter := Get_Iter(Model, Path);
   end Get_Store_And_Iter;

   function Add_Quote_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean
   -- callback to respond to the "Add Quote" button:
   -- this calls the mnemonic version below
   is ( Add_Quote_Mnemonic_Cb(Self, False) );

   function Add_Quote_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean
   -- callback to respond to the "Add Quote" mnemonic:
   -- adds a new row beneath the one currently highlighted
   is
      Store         : Gtk_List_Store;
      Iter, Sibling : Gtk_Tree_Iter;
   begin
      Get_Store_And_Iter(Gtk_Tree_View(Self), Store, Sibling);
      Store.Insert_After(Iter, Sibling);
      return False;
   end Add_Quote_Mnemonic_Cb;

   function Del_Quote_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean
   -- callback to respond to the "Delete Quote" button:
   -- this calls the mnemonic version below
   is ( Del_Quote_Mnemonic_Cb(Self, False) );

   function Del_Quote_Mnemonic_Cb
      (Self : access Glib.Object.GObject_Record'Class;
       Arg  : Boolean
      ) return Boolean
   -- callback to respond to the "Delete Quote" mnemonic:
   -- deletes the currently highlighted row
   is
      Store : Gtk_List_Store;
      Iter  : Gtk_Tree_Iter;
      Window_And_View: Shutdown_GObject := Shutdown_GObject(Self);
   begin
      Get_Store_And_Iter(Window_And_View.Tree_View, Store, Iter);
      Store.Remove(Iter);
      return False;
   end Del_Quote_Mnemonic_Cb;

   function Quit_Button_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean
   is ( Delete_Main_Window_Cb(Self, null) );

   function Quit_Button_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean
   is ( Delete_Main_Window_Cb(Self, null) );

   function Window_Key_Release_Cb
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Key
     ) return Boolean
   is
      package Key_Types renames Gdk.Types;
      use all type Key_Types.Gdk_Key_Type;
      use all type Key_Types.Gdk_Modifier_Type;
      package Key_Syms renames Gdk.Types.Keysyms;
      Modifier: Key_Types.Gdk_Modifier_Type := Event.State;
      Keyval: Key_Types.Gdk_Key_Type := Event.Keyval;
      Window: Gtk_Window := Shutdown_GObject(Self).Window;
   begin
      if Modifier = Key_Types.Control_Mask and
         ( Keyval = Key_Syms.GDK_LC_Q or Keyval = Key_Syms.GDK_LC_W) then
         return Delete_Main_Window_Cb(Self, null);
      else
         return False;
      end if;
   end Window_Key_Release_Cb;

end Callbacks;
