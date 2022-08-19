with Ada.Text_IO;

with Glib.Properties; use Glib.Properties;
with Glib.Values;     use Glib.Values;

with Gtk.Dialog;
with Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;
with Gtk.List_Store;          use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Tree_View_Column;    use Gtk.Tree_View_Column;
with Gtk.Widget;

with GnatColl.Json;

with Quote_Structure;

package body Callbacks is

   package Tio renames Ada.Text_IO;

   function Delete_Main_Window_Cb
      (Self  : access Gtk_Widget_Record'Class;
       Event : Gdk.Event.Gdk_Event)
      return Boolean
   is
      pragma Unreferenced (Self, Event);
   begin
      Gtk.Main.Main_Quit;
      return True;
   end Delete_Main_Window_Cb;

   procedure Editing_Done
      ( Self     : access Glib.Object.GObject_Record'Class;
        Path     : UTF8_String;
        New_Text : UTF8_String
       )
         -- handles the response to editing a text cell in a tree view
   is
      -- we pass in the tree view as the object
      Tree_View  : Gtk_Tree_View := Gtk_Tree_View(Self);
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
   begin
      -- initialize cell to hold a string, then put the desired string into it
      Glib.Values.Init(Cell_Value, Glib.GType_String);
      Glib.Values.Set_String(Cell_Value, New_Text);
      -- find the active cell in the tree view
      -- (which should be the one we want to update)
      Get_Cursor(Tree_View, Tree_Path, Tree_Column);
      Column_Id := Tree_Column.Get_Sort_Column_Id;
      -- finally set the tree view's value to keep the value we wanted
      Set_Value(Store, Iter, Column_Id, Cell_Value);
   end Editing_Done;

   function Save_Quotes_Cb
     (Self  : access Glib.Object.GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button)
      return Boolean
   -- handles the response to pressing the save button
   is
   begin
      return Save_Quotes_Mnemonic_Cb(Self, True);
   end Save_Quotes_Cb;

   function Save_Quotes_Mnemonic_Cb
     (Self: access Glib.Object.GObject_Record'Class;
      Arg : Boolean)
      return Boolean
   -- handles the response to pressing the save button
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
            -- need to work out how to change this to use Fields,
            -- currently in main.adb
            Author_String : UTF8_String := Store.Get_String(Iter, 0);
            Speaker_String: UTF8_String := Store.Get_String(Iter, 1);
            Text_String   : UTF8_String := Store.Get_String(Iter, 2);
            Quote_String  : UTF8_String := Store.Get_String(Iter, 3);
            This_Quote    : Quote_Structure.Quote
               := New_Quote(Author_String, Speaker_String, Quote_String, Text_String);

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
               Tio.Put_Line("Writing to " & Filename);
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
   is
   begin
      return Add_Quote_Mnemonic_Cb(Self, False);
   end Add_Quote_Cb;

   function Add_Quote_Mnemonic_Cb
      (Self: access Glib.Object.GObject_Record'Class;
       Arg : Boolean
      ) return Boolean
   is
      Store         : Gtk_List_Store;
      Iter, Sibling : Gtk_Tree_Iter;
   begin
      Ada.Text_IO.Put_Line(Arg'Image);
      Get_Store_And_Iter(Gtk_Tree_View(Self), Store, Sibling);
      Store.Insert_After(Iter, Sibling);
      return False;
   end Add_Quote_Mnemonic_Cb;

   function Del_Quote_Cb
      (Self  : access Glib.Object.GObject_Record'Class;
       Event : Gdk.Event.Gdk_Event_Button
      ) return Boolean
   is
   begin
      return Del_Quote_Mnemonic_Cb(Self, False);
   end Del_Quote_Cb;

   function Del_Quote_Mnemonic_Cb
      (Self : access Glib.Object.GObject_Record'Class;
       Arg  : Boolean
      ) return Boolean
   is
      Store : Gtk_List_Store;
      Iter  : Gtk_Tree_Iter;
   begin
      Get_Store_And_Iter(Gtk_Tree_View(Self), Store, Iter);
      Store.Remove(Iter);
      return False;
   end Del_Quote_Mnemonic_Cb;

end Callbacks;