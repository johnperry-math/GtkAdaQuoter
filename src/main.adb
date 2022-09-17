-- A Gtk frontend to a "fortune"-like program,
-- written to introduce myself to GtkAda.
-- Reads quotes from a file whose format is described in JsonQuoter;
-- see also the Fields type.
-- It then opens a window that displays the quotes
-- and allows the user to edit, add, delete, and save quotes.
-- Uses Gtk's UI elements FileChooser, TreeView, and Button,
-- along with mnemonics.

-- Ada packages
with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

-- Gdk packages (gdk is a low-level API for gtk)
with Gdk.Rectangle;          use Gdk.Rectangle;
with Gdk.Screen;             use Gdk.Screen;

-- Gtk packages
with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Editable;           use Gtk.Editable;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Fixed;              use Gtk.Fixed;
with Gtk.Image;              use Gtk.Image;
with Gtk.Label;              use Gtk.Label;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Main;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Table;              use Gtk.Table;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;
with Gtk.Window;             use Gtk.Window;
with Pango.Enums;            use Pango.Enums;

-- Glib packages
with Glib;            use all type Glib.Gchar_Array;
with Glib.Values;
with Glib.Properties; use Glib.Properties;
with Glib.Object;

-- Gtkada packages
with Gtkada;
with Gtkada.Types;

-- Gnat-specific packages
with Gnat.OS_Lib;

-- GnatColl packages
with GnatColl.JSON;
with GnatColl.VFS;

-- my packages
with Quote_Structure; use all type Quote_Structure.Fields;
with Callbacks;       use Callbacks;

procedure Main is

   -- containers

   Screen    : Gdk.Screen.Gdk_Screen;
   -- screen containing the main window
   Monitor_Number: Glib.Gint;
   -- active monitor
   Geometry: Gdk.Rectangle.Gdk_Rectangle;
   -- monitor's geometry

   Win        : Gtk_Window;
   -- main window
   Scroll_Box : Gtk_Scrolled_Window;
   -- a scrollbox to contain the tree view
   VBox       : Gtk_Table;
   -- vertical box containing the tree view and the button bar

   -- the tree view and its storage

   Tree_View  : Gtk_Tree_View;
   -- the tree view that presents the data
   List_Store : Gtk_List_Store;
   -- the data the tree view presents

   -- buttons

   Button_Bar : Gtk_Table;
   -- horizontal box containing the buttons
   Add_Button, Del_Button, Save_Button, Quit_Button: Gtk_Button;
   -- button whose name implies its meaning

   Window_And_View: Shutdown_GObject;
   -- for tracking the window and the view

   -- the quotes

   subtype Fields is Quote_Structure.Fields;
   use all type Quote_Structure.Quote;
   use all type Quote_Structure.Quote_Vector;

   Column_Widths: array(Fields) of Integer := (others => 0);
   Renderers: Column_Renderer_Array;

   All_Quotes: Quote_Structure.Quote_Vector;
   -- all quotes read from the resource file

   Dir_Sep   : Character := GNAT.OS_Lib.Directory_Separator;
   -- used to open the main directory

   function Setup_Columns return Column_Renderer_Array is
   -- title the columns and make them editable

      use all type Glib.Gint;

      Column_No: Glib.Gint;
      Column   : Gtk_Tree_View_Column;
      Result   : Column_Renderer_Array;

   begin

      for Field in Fields loop
         declare Text: Gtk_Cell_Renderer_Text;
         begin
            -- column text
            Gtk_New(Text);
            Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);

            -- column: title, pack text into it, attribute, resizable, sortable
            Gtk_New(Column);
            Column.Set_Title(To_String(Field_Names(Field)));
            Column.Pack_Start(Text, True);
            Column.Add_Attribute(Text, "text", Fields'Pos(Field));
            Column.Set_Resizable(True);
            Column.Set_Sort_Column_Id(Fields'Pos(Field));
            if Column_Widths(Field) /= 0 then
               Column.Set_Fixed_Width(Glib.Gint(Column_Widths(Field)));
            end if;

            -- finally, add it to the tree view
            -- interestingly, column number might differ from sort id above
            Column_No := Tree_View.Append_Column(Column);
            Set_Property(
                         Column,
                         Gtk.Tree_View_Column.Max_Width_Property,
                         Geometry.Width / 3
                        );
            --  Set_Property(
            --               Column,
            --               Gtk.Tree_View_Column.Max_Width_Property,
            --               Geometry.Width / 10
            --              );
            if Field = Quotation then
               Set_Property(Text,
                            Gtk.Cell_Renderer_Text.Wrap_Width_Property,
                            Geometry.Width / 3
                           );
               Set_Property(Text,
                            Gtk.Cell_Renderer_Text.Wrap_Mode_Property,
                            Pango.Enums.Pango_Wrap_Word
                           );
            end if;
            --  Column.Set_Sizing
            --        (Gtk.Tree_View_Column.Tree_View_Column_Autosize);
            Result(Field) := Text;
         end;
      end loop;

      return Result;

   end Setup_Columns;

   Home_Path : String := GnatColl.VFS.Get_Home_Directory.
      Display_Full_Name(Normalize => True) & Dir_Sep;
   Read_Result: GnatColl.JSON.Read_Result;

begin

   --  Initialize GtkAda.
   Gtk.Main.Init;

   -- read signatures
   declare
      Path: String := Home_Path & "signatures" & Dir_Sep;
      File_Path: String := Get_Source_File(Path);
   begin
      Read_Quotes(File_Path, All_Quotes);
   end;

   -- read configuration, if it exists
   declare
      package Tio renames Ada.Text_IO;
      Config_Path: String := Home_Path & Configuration_File;
      Config_File: Tio.File_Type;
      Data: GnatColl.JSON.JSON_Value;
      All_Lines: Unbounded_String;
   begin
      Tio.Open(Config_File, Tio.In_File, Config_Path);
      while not Tio.End_Of_File(Config_File) loop
         declare Line: String := Tio.Get_Line(Config_File);
         begin
            Append(All_Lines, Line);
         end;
      end loop;
      Tio.Close(Config_File);
      Read_Result := GnatColl.JSON.Read(All_Lines);
      if Read_Result.Success then
         for Field in Fields loop
            if Read_Result.Value.Has_Field(To_String(Field_Names(Field))) then
               Column_Widths(Field)
                  := Read_Result.Value.Get(To_String(Field_Names(Field)));
            end if;
         end loop;
      end if;
   exception
      when E: Tio.Status_Error =>
         Tio.Put_Line("** gtkadaquoter ** could not find configuration file " & Configuration_File);
      when others =>
         Tio.Put_Line("** gtkadaquoter ** could not read configuration file " & Configuration_File);
   end;

   --  Create a full-size window
   Gtk_New (Win);
   Screen := Win.Get_Screen;
   Monitor_Number := Screen.Get_Monitor_At_Window(Screen.Get_Active_Window);
   Screen.Get_Monitor_Geometry(Monitor_Number, Geometry);
   declare
      subtype Gint is Glib.Gint;
      Width: Integer := Integer(Geometry.Width);
      Height: Integer := Integer(Geometry.Height);
   begin
      if Read_Result.Value.Has_Field("width") then
         Width := Read_Result.Value.Get("width");
      end if;
      if Read_Result.Value.Has_Field("height") then
         Height := Read_Result.Value.Get("height");
      end if;
      Win.Set_Default_Size(Gint(Width), Gint(Height));
   end;

   -- Create a box for vertical organization of the window contents:
   -- 10 rows, 1 column, different-sized cells
   VBox := Gtk_Table_New(10, 1, False);
   Win.Add(VBox);
   Gtk_New(Scroll_Box);
   VBox.Attach(Scroll_Box, 0, 1, 0, 8, Expand + Fill, Expand + Fill, 0, 0);

   -- A horizontally-flowing box in position (0,9) - (1,10)
   Button_Bar := Gtk_Table_New(1, 10, True);
   VBox.Attach(Button_Bar, 0, 1, 9, 10, Expand + Fill, Shrink, 0, 0);

   -- Buttons for the Button_Bar: Add Quote, Delete Quote, Save Quotes
   Add_Button := Gtk_Button_New_With_Mnemonic("_Add Quote");
   Del_Button := Gtk_Button_New_With_Mnemonic("_Delete Quote");
   Save_Button := Gtk_Button_New_With_Mnemonic("_Save Quotes");
   Quit_Button := Gtk_Button_New_With_Mnemonic("_Quit");
   -- gtk devs think you shouldn't combine labels and icons in buttons
   -- so we opt for labels with mnemonics
   --  Add_Button := Gtk_Button_New_From_Icon_Name("list-add-symbolic", Icon_Size_Button);
   --  Del_Button := Gtk_Button_New_From_Icon_Name("list-remove-symbolic", Icon_Size_Button);
   --  Save_Button := Gtk_Button_New_From_Icon_Name("document-save-symbolic", Icon_Size_Button);
   --  Quit_Button := Gtk_Button_New_From_Icon_Name("process-stop-symbolic", Icon_Size_Button);

   --  Save_Button.Set_Halign(Align_Start); -- this will place buttons at extremes
   --  Quit_Button.Set_Halign(Align_End);   -- but it is rather unsightly
   Button_Bar.Attach(Save_Button, 0, 1, 0, 1, Fill, Shrink, 0, 0);
   Button_Bar.Attach(Add_Button, 4, 5, 0, 1, Expand, Shrink, 0, 0);
   Button_Bar.Attach(Del_Button, 5, 6, 0, 1, Expand, Shrink, 0, 0);
   Button_Bar.Attach(Quit_Button, 9, 10, 0, 1, Fill, Shrink, 0, 0);
   Button_Bar.Set_VExpand(False);
   Button_Bar.Set_Hexpand(True);
   Button_Bar.Set_Size_Request(40, 20);

   -- Iniitalize a tree view with its backing storage of 3 columns (0..2)
   Gtk_New(Tree_View);
   Gtk_New(List_Store, ( 0..3 => Glib.GType_String ));
   Scroll_Box.Add(Tree_View);

   -- fill `List_Store` with the quote data

   declare

      Num_Quotes : Natural := Number_Of_Quotes(All_Quotes);
      Cell_Value : Glib.Values.GValue;
      -- used to store strings

   begin

      -- initialize Cell_Value to hold the correct type
      Glib.Values.Init(Cell_Value, Glib.GType_String);

      -- loop through the quotes
      for Item in 1 .. Num_Quotes loop

         declare

            Current_Quote : Quote_Structure.Quote
               := Quote_Item(All_Quotes, Item - 1);
            Iter          : Gtk_Tree_Iter;
            -- used to iterate through the storage's rows
            -- columns are specified directly

         begin

            -- add a row, set its values

            List_Store.Append(Iter);

            Glib.Values.Set_String(Cell_Value, Author_Of(Current_Quote) );
            List_Store.Set_Value(Iter, Fields'Pos(Author), Cell_Value );
            Glib.Values.Set_String(Cell_Value, Speaker_Of(Current_Quote) );
            List_Store.Set_Value(Iter, Fields'Pos(Speaker), Cell_Value );
            Glib.Values.Set_String(Cell_Value, Source_Of(Current_Quote) );
            List_Store.Set_Value(Iter, Fields'Pos(Source), Cell_Value );
            Glib.Values.Set_String(Cell_Value, Body_Of(Current_Quote) );
            List_Store.Set_Value(Iter, Fields'Pos(Quotation), Cell_Value );

         end;

      end loop;

   end;

   -- set the tree's model to be `List_Store`;
   -- notice the need to cast the tagged type
   Tree_View.Set_Model( To_Interface(List_Store) );
   Tree_View.Set_Reorderable(True);

   -- title the columns and make them editable

   Renderers := Setup_Columns;
   Window_And_View := Initialize(Win, Tree_View, Renderers);
   for Renderer of Renderers loop
      Renderer.On_Edited(Editing_Done'Access, Slot => Window_And_View);
   end loop;

   -- CALLBACKS

   -- connect the buttons with their callbacks
   -- the `Slot` is the item passed to the `Call` as `Self`
   Save_Button.On_Button_Release_Event
      (Call  => Save_Quotes_Cb'Access,
       Slot  => List_Store,
       After => False);
   Save_Button.On_Mnemonic_Activate
      (Call  => Save_Quotes_Mnemonic_Cb'Access,
       Slot  => List_Store,
       After => False);
   Add_Button.On_Button_Release_Event
      (Call  => Add_Quote_Cb'Access,
       Slot  => Tree_View,
       After => False);
   Add_Button.On_Mnemonic_Activate
      (Call  => Add_Quote_Mnemonic_Cb'Access,
       Slot  => Tree_View,
       After => False);
   Del_Button.On_Button_Release_Event
      (Call  => Del_Quote_Cb'Access,
       Slot  => Tree_View,
       After => False);
   Del_Button.On_Mnemonic_Activate
      (Call  => Del_Quote_Mnemonic_Cb'Access,
       Slot  => Tree_View,
       After => False);
   Quit_Button.On_Button_Release_Event
      (Call  => Quit_Button_Cb'Access,
       Slot  => Window_And_View,
       After => False);
   Quit_Button.On_Mnemonic_Activate
      (Call  => Quit_Button_Mnemonic_Cb'Access,
       Slot  => Window_And_View,
       After => False);
   Win.On_Key_Release_Event
      (Call  => Window_Key_Release_Cb'Access,
       Slot  => Window_And_View,
       After => False);

   -- Stop the Gtk process when closing the window
   Win.On_Delete_Event (Delete_Main_Window_Cb'Unrestricted_Access, Window_And_View);

   --  Show the window and present it
   Win.Show_All;
   Win.Present;
   declare
      subtype Gint is Glib.Gint;
      Corner_X: Integer := 0;
      Corner_Y: Integer := 0;
   begin
      if Read_Result.Value.Has_Field("corner_x") then
         Corner_X := Read_Result.Value.Get("corner_x");
      end if;
      if Read_Result.Value.Has_Field("corner_y") then
         Corner_Y := Read_Result.Value.Get("corner_y");
      end if;
      Win.Move(Gint(Corner_X), Gint(Corner_Y));
   end;
   Tree_View.Grab_Focus;

   --  Start the Gtk+ main loop
   Gtk.Main.Main;

end Main;
