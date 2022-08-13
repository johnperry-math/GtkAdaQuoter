-- Ada packages
with Ada.Text_IO;
with Ada.Containers.Vectors;

-- Gdk packages (gdk is a low-level API for gtk)
with Gdk.Event;            use Gdk.Event;

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

-- my packages
with Quote_Structure;
with Gtk_Utility;     use Gtk_Utility;

procedure Main is

   Win        : Gtk_Window;
   Scroll_Box : Gtk_Scrolled_Window;
   HBox, VBox : Gtk_Table;

   Add_Button, Del_Button, Save_Button
                 : Gtk_Button;
   Add_Label, Del_Label, Save_Label
                 : Gtk_Label;

   Tree_View  : Gtk_Tree_View;
   List_Store : Gtk_List_Store;

   All_Quotes: Quote_Structure.Quote_Vector;
   use all type Quote_Structure.Quote_Vector;
   use all type Quote_Structure.Quote;

   Dummy: Glib.Gint;

begin

   Read_Quotes("/home/cantanima/signatures/all_signatures.json", All_Quotes);

   --  Initialize GtkAda.
   Gtk.Main.Init;

   --  Create a window with a size of 400x400
   Gtk_New (Win);
   Win.Set_Default_Size (400, 400);

   -- Create a box to organize vertically the contents of the window
   -- 10 rows, 1 column, differently-sized cells
   VBox := Gtk_Table_New(10, 1, False);
   Win.Add(VBox);
   Gtk_New(Scroll_Box);
   VBox.Attach(Scroll_Box, 0, 1, 0, 8, Expand + Fill, Expand + Fill, 0, 0);

   -- A horizontally-flowing box in position (0,9) - (1,10)
   --  Gtk_New_Hbox(HBox);
   HBox := Gtk_Table_New(1, 5, True);
   VBox.Attach (HBox, 0, 1, 9, 10, Expand, Shrink, 0, 0);

   -- Buttons for the HBox: Add Quote, Delete Quote, Save Quotes
   Gtk_New(Add_Button);
   Gtk_New(Add_Label); Set_Label(Add_Label, "Add Quote"); Add_Button.Add(Add_Label);
   Gtk_New(Del_Button);
   Gtk_New(Del_Label); Set_Label(Del_Label, "Delete Quote"); Del_Button.Add(Del_Label);
   Gtk_New(Save_Button);
   Gtk_New(Save_Label); Set_Label(Save_Label, "Save Quotes"); Save_Button.Add(Save_Label);

   HBox.Attach(Add_Button, 1, 2, 0, 1, Shrink, Shrink, 0, 0);
   HBox.Attach(Del_Button, 2, 3, 0, 1, Shrink, Shrink, 0, 0);
   HBox.Attach(Save_Button, 4, 5, 0, 1, Shrink, Shrink, 0, 0);
   Set_VExpand(HBox, False);
   Set_Size_Request(HBox, 40, 20);

   -- Iniitalize a tree view with its backing storage of 3 columns (0..2)
   Gtk_New(Tree_View);
   Gtk_New(List_Store, ( 0..3 => Glib.GType_String ));
   Scroll_Box.Add(Tree_View);
   Save_Button.On_Button_Release_Event(Call  => Save_Quotes_Cb'Access,
                                       Slot  => List_Store,
                                       After => False);

   declare

      Num_Quotes : Natural := Number_Of_Quotes(All_Quotes);
      Cell_Value : Glib.Values.GValue;

   begin

      -- initialize Cell_Value to hold the correct type
      Glib.Values.Init(Cell_Value, Glib.GType_String);

      -- fill the store
      for Item in 1 .. Num_Quotes loop

         declare

            Current_Quote              : Quote_Structure.Quote
               := Quote_Item(All_Quotes, Item - 1);
            Iter                       : Gtk_Tree_Iter;
            -- used to iterate thorugh the storage's rows
            -- columns are specified directly

         begin

            List_Store.Append(Iter);
            Glib.Values.Set_String(Cell_Value, Author_Of(Current_Quote) );
            List_Store.Set_Value(Iter, 0, Cell_Value );
            Glib.Values.Set_String(Cell_Value, Speaker_Of(Current_Quote) );
            List_Store.Set_Value(Iter, 1, Cell_Value );
            Glib.Values.Set_String(Cell_Value, Source_Of(Current_Quote) );
            List_Store.Set_Value(Iter, 2, Cell_Value );
            Glib.Values.Set_String(Cell_Value, Body_Of(Current_Quote) );
            List_Store.Set_Value(Iter, 3, Cell_Value );

         end;
      end loop;
   end;

   Tree_View.Set_Model( To_Interface(List_Store) );

   declare

      Column_No: Glib.Gint;
      Column   : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;

   begin

      Gtk_New(Column);
      Column.Set_Title("Author");
      Gtk_New(Text);
      Text.On_Edited(Editing_Done'Access, Slot => Tree_View);
      Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);
      Column.Pack_Start(Text, True);
      Column.Add_Attribute(Text, "text", 0);
      Column_No := Tree_View.Append_Column(Column);
      Column.Set_Resizable(True);
      Column.Set_Sort_Column_Id(0); -- store need not align w/view?!?

      Gtk_New(Column);
      Column.Set_Title("Speaker");
      Gtk_New(Text);
      Text.On_Edited(Editing_Done'Access, Slot => Tree_View);
      Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);
      Column.Pack_Start(Text, True);
      Column.Add_Attribute(Text, "text", 1);
      Column_No := Tree_View.Append_Column(Column);
      Column.Set_Resizable(True);
      Column.Set_Sort_Column_Id(1);

      Gtk_New(Column);
      Column.Set_Title("Source");
      Gtk_New(Text);
      Text.On_Edited(Editing_Done'Access, Slot => Tree_View);
      Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);
      Column.Pack_Start(Text, True);
      Column.Add_Attribute(Text, "text", 2);
      Column_No := Tree_View.Append_Column(Column);
      Column.Set_Resizable(True);
      Column.Set_Sort_Column_Id(2);

      Gtk_New(Column);
      Column.Set_Title("Quotation");
      Gtk_New(Text);
      Text.On_Edited(Editing_Done'Access, Slot => Tree_View);
      Set_Property(Text, Gtk.Cell_Renderer_Text.Editable_Property, True);
      Column.Pack_Start(Text, True);
      Column.Add_Attribute(Text, "text", 3);
      Column_No := Tree_View.Append_Column(Column);
      Column.Set_Resizable(True);
      Column.Set_Sort_Column_Id(3);

  end;

   -- Stop the Gtk process when closing the window
   Win.On_Delete_Event (Delete_Main_Window_Cb'Unrestricted_Access);

   --  Show the window and present it
   Win.Show_All;
   Win.Present;

   --  Start the Gtk+ main loop
   Gtk.Main.Main;

end Main;
