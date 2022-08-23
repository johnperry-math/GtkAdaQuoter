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

-- Glib packages
with Glib;            use all type Glib.Gchar_Array;
with Glib.Values;
with Glib.Properties; use Glib.Properties;
with Glib.Object;

-- Gtkada packages
with Gtkada;
with Gtkada.Types;

-- Gnat packages
with Gnat.OS_Lib;

-- GnatColl packages
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
   Add_Button, Del_Button, Save_Button : Gtk_Button;
   -- button whose name implies its meaning

   -- the quotes

   subtype Fields is Quote_Structure.Fields;
   use all type Quote_Structure.Quote;
   use all type Quote_Structure.Quote_Vector;

   All_Quotes: Quote_Structure.Quote_Vector;
   -- all quotes read from the resource file

   Field_Names: array (Fields) of Ada.Strings.Unbounded.Unbounded_String
      := (To_Unbounded_String("Author"),
          To_Unbounded_String("Speaker"),
          To_Unbounded_String("Source"),
          To_Unbounded_String("Quotation")
         );
   -- for easily mapping the fields to strings in the UI

   Dir_Sep   : Character := GNAT.OS_Lib.Directory_Separator;
   -- used to open the main directory

   procedure Setup_Column(Field: Fields) is
   -- title the columns and make them editable

      use all type Glib.Gint;

      Column_No: Glib.Gint;
      Column   : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;

   begin

      -- column text
      Gtk_New(Text);
      Text.On_Edited(Editing_Done'Access, Slot => Tree_View);
      Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);

      -- column: title, pack text into it, attribute, resizable, sortable
      Gtk_New(Column);
      Column.Set_Title(To_String(Field_Names(Field)));
      Column.Pack_Start(Text, True);
      Column.Add_Attribute(Text, "text", Fields'Pos(Field));
      Column.Set_Resizable(True);
      Column.Set_Sort_Column_Id(Fields'Pos(Field));

      -- finally, add it to the tree view
      -- interestingly, column number might differ from sort id above
      Column_No := Tree_View.Append_Column(Column);

   end Setup_Column;

begin

   Read_Quotes(GnatColl.VFS.
                  Get_Home_Directory.
                     Display_Full_Name(Normalize => True) & Dir_Sep
               & "signatures" & Dir_Sep
               & "all_signatures.json", All_Quotes);

   --  Initialize GtkAda.
   Gtk.Main.Init;

   --  Create a window with a size of 400x400
   Gtk_New (Win);
   Screen := Win.Get_Screen;
   Monitor_Number := Screen.Get_Monitor_At_Window(Screen.Get_Active_Window);
   Screen.Get_Monitor_Geometry(Monitor_Number, Geometry);
   Win.Set_Default_Size (Geometry.Width, Geometry.Height);

   -- Create a box for vertical organization of the window contents:
   -- 10 rows, 1 column, different-sized cells
   VBox := Gtk_Table_New(10, 1, False);
   Win.Add(VBox);
   Gtk_New(Scroll_Box);
   VBox.Attach(Scroll_Box, 0, 1, 0, 8, Expand + Fill, Expand + Fill, 0, 0);

   -- A horizontally-flowing box in position (0,9) - (1,10)
   Button_Bar := Gtk_Table_New(1, 5, True);
   VBox.Attach (Button_Bar, 0, 1, 9, 10, Expand, Shrink, 0, 0);

   -- Buttons for the Button_Bar: Add Quote, Delete Quote, Save Quotes
   Add_Button := Gtk_Button_New_With_Mnemonic("_Add Quote");
   Del_Button := Gtk_Button_New_With_Mnemonic("_Delete Quote");
   Save_Button := Gtk_Button_New_With_Mnemonic("_Save Quotes");

   Button_Bar.Attach(Add_Button, 1, 2, 0, 1, Shrink, Shrink, 0, 0);
   Button_Bar.Attach(Del_Button, 2, 3, 0, 1, Shrink, Shrink, 0, 0);
   Button_Bar.Attach(Save_Button, 4, 5, 0, 1, Shrink, Shrink, 0, 0);
   Set_VExpand(Button_Bar, False);
   Set_Size_Request(Button_Bar, 40, 20);

   -- Iniitalize a tree view with its backing storage of 3 columns (0..2)
   Gtk_New(Tree_View);
   Gtk_New(List_Store, ( 0..3 => Glib.GType_String ));
   Scroll_Box.Add(Tree_View);

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

   for Field in Fields loop
      Setup_Column(Field);
   end loop;

   -- Stop the Gtk process when closing the window
   Win.On_Delete_Event (Delete_Main_Window_Cb'Unrestricted_Access);

   --  Show the window and present it
   Win.Show_All;
   Win.Present;

   --  Start the Gtk+ main loop
   Gtk.Main.Main;

end Main;
