--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Containers;          use Ada.Containers;
with Ada.Directories;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;                    use Glib;

with Gtk.Adjustment;
with Gtk.Check_Button;
with Gtk.Editable;            use Gtk.Editable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Spin_Button;
with Gtk.Toggle_Button;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;   use Gtkada.File_Selection;

with Session;                 use Session;
with Track;                   use Track;

package body GUI.Tracks is

   procedure New_Track_File_Btn_CB (Self : access Gtk_Button_Record'Class) is
      New_File  : constant String := File_Selection_Dialog (Title => App_Title & " - Track File",
                                                            --  Default_Dir => Dir,
                                                            Must_Exist => True);
   begin
      if New_File /= "" then
         Ada.Text_IO.Put_Line ("DEBUG: File selected: " & New_File);
         Set_Dirty;
      end if;
      Self.Set_Tooltip_Text (New_File);
   end New_Track_File_Btn_CB;

   procedure New_Track_File_Del_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      New_Track_File_Btn.Set_Tooltip_Text ("");
      Set_Dirty;
   end New_Track_File_Del_Btn_CB;

   procedure Comment_Changed_CB (Self : Gtk_Editable) is
      Ent       : constant Gtk_Entry := -Self;
      Name      : constant String  := Ent.Get_Name;
      Track_Num : constant Integer := Integer'Value (Name (9 .. Name'Last));
   begin
      if Sess.Tracks (Track_Num).Comment /= Gtk_Entry (Tracks_Grid.Get_Child_At (Comment_Col, Gint (Track_Num))).Get_Text then
         Sess.Tracks (Track_Num).Comment := To_Unbounded_String (Gtk_Entry (Tracks_Grid.Get_Child_At (Comment_Col, Gint (Track_Num))).Get_Text);
         Set_Dirty;
      end if;
   end Comment_Changed_CB;

   procedure Title_Changed_CB (Self : Gtk_Editable) is
      Ent       : constant Gtk_Entry := -Self;
      Name      : constant String  := Ent.Get_Name;
      Track_Num : constant Integer := Integer'Value (Name (7 .. Name'Last));
   begin
      if Sess.Tracks.Length > 0 and then
         Sess.Tracks (Track_Num).Title /= Gtk_Entry (Tracks_Grid.Get_Child_At (Title_Col, Gint (Track_Num))).Get_Text then
         Sess.Tracks (Track_Num).Title := To_Unbounded_String (Gtk_Entry (Tracks_Grid.Get_Child_At (Title_Col, Gint (Track_Num))).Get_Text);
         Set_Dirty;
      end if;
   end Title_Changed_CB;   

   procedure Track_Insert_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
      New_Path : constant String := New_Track_File_Btn.Get_Tooltip_Text;
   begin
      New_Track.Title   := To_Unbounded_String (
                              Gtk_Entry (
                                 Tracks_Grid.Get_Child_At (Title_Col, New_Track_Entry_Row)).Get_Text);
      New_Track.Comment := To_Unbounded_String (
                              Gtk_Entry (
                                 Tracks_Grid.Get_Child_At (Comment_Col, New_Track_Entry_Row)).Get_Text);
      New_Track.Skip    := Gtk.Toggle_Button.Gtk_Toggle_Button (
                              Tracks_Grid.Get_Child_At (Skip_Col, New_Track_Entry_Row)).Get_Active;
      New_Track.Volume  := Integer (Gtk.Spin_Button.Gtk_Spin_Button (
                              Tracks_Grid.Get_Child_At (Vol_Col, New_Track_Entry_Row)).Get_Value_As_Int);
      if New_Path'Length < 4 then
         if Message_Dialog ("You have not selected an audio or MIDI file, do you want to save this track?",
                         Confirmation, Button_No or Button_Yes) = Button_No
         then
            return; --  Nothing is done
         end if;
      end if;
      New_Track.Path := To_Unbounded_String (New_Path);
      New_Track.File_Type := Guess_Media_Type (New_Path);
      Sess.Tracks.Append (New_Item => New_Track);
       Display_Tracks;
      Tracks_Grid.Show_All;
      Set_Dirty;
   end Track_Insert_Btn_CB;

   procedure Track_Delete_Btn_CB (Self : access Gtk_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
      Buttons   : Message_Dialog_Buttons;
   begin
      Buttons := Message_Dialog (Msg => "Are you sure you want to remove this track?",
                                 Dialog_Type => Confirmation,
                                 Buttons => Button_No or Button_Yes,
                                 Title => App_Title & " - Delete Track");
      if Buttons = Button_Yes then
         Sess.Tracks.Delete (Track_Num);
         Display_Tracks;  --  N.B. Can't simply delete the row as numbering needs updating.
         Set_Dirty;
      end if;
   end Track_Delete_Btn_CB;

   procedure Track_Down_Btn_CB (Self : access Gtk_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      Sess.Tracks.Swap (Track_Num, Track_Num + 1);
      Display_Tracks;
      Set_Dirty;
   end Track_Down_Btn_CB;

   procedure Track_Up_Btn_CB (Self : access Gtk_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      Sess.Tracks.Swap (Track_Num, Track_Num - 1);
      Display_Tracks;
      Set_Dirty;
   end Track_Up_Btn_CB;

   procedure Track_File_Btn_CB (Self : access Gtk_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
      Filename  : constant String      := To_String (Sess.Tracks (Track_Num).Path);
      Dir,
      New_File  : Unbounded_String := Null_Unbounded_String;
   begin
      if Filename /= "" then
         Dir := To_Unbounded_String (Ada.Directories.Containing_Directory (Filename));
      end if;
      New_File := To_Unbounded_String (File_Selection_Dialog (Title => App_Title & " - Track File",
                                                              Default_Dir => To_String (Dir),
                                                              Must_Exist => True));
      if New_File /= "" then
         Sess.Tracks (Track_Num).Path := New_File;
         Sess.Tracks (Track_Num).File_Type := Guess_Media_Type (To_String (New_File));
         Display_Tracks;
         Set_Dirty;
      end if;
   end Track_File_Btn_CB;

   procedure Track_File_Del_Btn_CB (Self : access Gtk_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      Sess.Tracks (Track_Num).Path := Null_Unbounded_String;
      Sess.Tracks (Track_Num).File_Type := NONE;
      Display_Tracks;
      Set_Dirty;
   end Track_File_Del_Btn_CB;

   procedure Track_Select_Btn_CB (Self : access Gtk_Button_Record'Class) is
      use Session.Track_Vectors;
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      if Currently_Selected_Track > 0 and then 
         Currently_Selected_Track <= Integer (Sess.Tracks.Length) then
         Tracks_Grid.Get_Child_At (Row_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
         Tracks_Grid.Get_Child_At (Title_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
         Tracks_Grid.Get_Child_At (Comment_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
         Tracks_Grid.Get_Child_At (Vol_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
      end if;
      Currently_Selected_Track := Track_Num;
      Tracks_Grid.Get_Child_At (Row_Col, Gint (Track_Num)).Set_Name ("highlit");
      Tracks_Grid.Get_Child_At (Title_Col, Gint (Track_Num)).Set_Name ("highlit");
      Tracks_Grid.Get_Child_At (Comment_Col, Gint (Track_Num)).Set_Name ("highlit");
      Tracks_Grid.Get_Child_At (Vol_Col, Gint (Track_Num)).Set_Name ("highlit");
   end Track_Select_Btn_CB;

   procedure Track_Skip_Check_CB (Self : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      Sess.Tracks (Track_Num).Skip := Self.Get_Active;
      Set_Dirty;
   end Track_Skip_Check_CB;

   procedure Track_Vol_Changed_CB (Self : access Gtk.Spin_Button.Gtk_Spin_Button_Record'Class) is
      Name      : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer     := Integer'Value (Name (8 .. Name'Last));
   begin
      Sess.Tracks (Track_Num).Volume := Integer (Self.Get_Value_As_Int);
      Set_Dirty;
   end Track_Vol_Changed_CB;

   procedure Clear_Tracks_Display is
   begin
      --  clear out any existing items, ASSUMES rows are contiguous
      while Tracks_Grid.Get_Child_At (Title_Col, 1) /= null loop
         Tracks_Grid.Remove_Row (1);
      end loop;
   end Clear_Tracks_Display;

   function Create_Tracks_Grid return Gtk_Grid is
   begin
      Gtk_New (Tracks_Grid);
      Tracks_Grid.Attach (Gtk_Label_New ("Title"), Title_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Skip"), Skip_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Comment"), Comment_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Volume (%)"), Vol_Col, 0);
      return Tracks_Grid;

   end Create_Tracks_Grid;

   procedure Display_Empty_Track (Track_Row : Glib.Gint) is
   --  Display an empty row for entering a new track
      Dummy_Label : Gtk_Label;
      Track_File_Del_Btn,
      Track_Insert_Btn : Gtk_Button;
      Skip_Check : Gtk.Check_Button.Gtk_Check_Button;
      Title_Entry,
      Comment_Entry : Gtk_Entry;
      Vol_Adj    : Gtk.Adjustment.Gtk_Adjustment;
      Vol_Spin   : Gtk.Spin_Button.Gtk_Spin_Button;
   begin
      New_Track_Entry_Row := Track_Row;
      --  New_Track := new Track_T;

      --  Placeholders for the case of an empty Sess...
      for c in 0 .. Title_Col - 1 loop
         Gtk_New (Dummy_Label, "*");
         Tracks_Grid.Attach (Dummy_Label, c, Track_Row);
      end loop;

      Gtk_New (Title_Entry);
      Title_Entry.Set_Width_Chars (25);
      Tracks_Grid.Attach (Title_Entry, Title_Col, Track_Row);

      Gtk.Check_Button.Gtk_New (Skip_Check, "");
      Skip_Check.Set_Halign (Align_Center);
      Tracks_Grid.Attach (Skip_Check, Skip_Col, Track_Row);

      Gtk_New (Comment_Entry);
      Comment_Entry.Set_Width_Chars (40);
      Tracks_Grid.Attach (Comment_Entry, Comment_Col, Track_Row);

      Gtk.Adjustment.Gtk_New (
            Adjustment     => Vol_Adj,
            Value          => Gdouble (100),
            Lower          => 0.0,
            Upper          => 100.0,
            Step_Increment => 1.0,
            Page_Increment => 5.0,
            Page_Size      => 0.0);
      Gtk.Spin_Button.Gtk_New (Spin_Button => Vol_Spin, Adjustment => Vol_Adj, Climb_Rate => 0.1, The_Digits => 0);
      Tracks_Grid.Attach (Vol_Spin, Vol_Col, Track_Row);

      Gtk_New_From_Icon_Name (New_Track_File_Btn, "folder-symbolic", Icon_Size_Button);
      New_Track_File_Btn.On_Clicked (New_Track_File_Btn_CB'Access);
      Tracks_Grid.Attach (New_Track_File_Btn, File_Col, Track_Row);

      Gtk_New_From_Icon_Name (Track_File_Del_Btn, "edit-clear-symbolic", Icon_Size_Button);
      Track_File_Del_Btn.On_Clicked (New_Track_File_Del_Btn_CB'Access);
      Tracks_Grid.Attach (Track_File_Del_Btn, File_Del_Col, Track_Row);

      Track_Insert_Btn := Gtk_Button_New_With_Label ("Insert");
      Track_Insert_Btn.On_Clicked (Track_Insert_Btn_CB'Access);
      Tracks_Grid.Attach (Track_Insert_Btn, Down_Col + 1, Track_Row, 3);
   end Display_Empty_Track;

   procedure Display_Tracks is
      Track_Row  : Gint := 1;
      Row        : Integer;
      Track_Down_Btn,
      Track_Del_Btn,
      Track_Up_Btn,
      Track_File_Btn,
      Track_File_Del_Btn : Gtk_Button;
      Skip_Check : Gtk.Check_Button.Gtk_Check_Button;
      Row_Label  : Gtk_Label;
      Title_Entry,
      Comment_Entry : Gtk_Entry;
      MIDI_Label : Gtk_Label;
      Vol_Adj    : Gtk.Adjustment.Gtk_Adjustment;
      Vol_Spin   : Gtk.Spin_Button.Gtk_Spin_Button;
   begin
      Clear_Tracks_Display;

      for Track of Sess.Tracks loop
         Gtk_New (Row_Label, Track_Row'Img);
         Row_Label.Set_Halign (Align_Center);
         Tracks_Grid.Attach (Row_Label, Row_Col, Track_Row);

         Row := Integer (Track_Row);
         Gtk_New_From_Icon_Name (Select_Btn_Arr (Row), "go-next-symbolic", Icon_Size_Button);
         Select_Btn_Arr (Row).Set_Name ("Select" & Row'Image);
         Select_Btn_Arr (Row).On_Clicked (Track_Select_Btn_CB'Access);
         Tracks_Grid.Attach (Select_Btn_Arr (Row), Select_Col, Track_Row);

         Gtk_New (Title_Entry);
         Title_Entry.Set_Width_Chars (25);
         Title_Entry.Set_Text (To_String (Track.Title));
         Title_Entry.Set_Name ("Title" & Row'Image);
         Tracks_Grid.Attach (Title_Entry, Title_Col, Track_Row);
         On_Changed (+Title_Entry, Title_Changed_CB'Access);

         Gtk.Check_Button.Gtk_New (Skip_Check, "");
         Skip_Check.Set_Active (Track.Skip);
         Skip_Check.Set_Halign (Align_Center);
         Skip_Check.Set_Name ("SkipIt" & Row'Image);
         Skip_Check.On_Toggled (Track_Skip_Check_CB'Access);
         Tracks_Grid.Attach (Skip_Check, Skip_Col, Track_Row);

         Gtk_New (Comment_Entry);
         Comment_Entry.Set_Width_Chars (40);
         Comment_Entry.Set_Text (To_String (Track.Comment));
         Comment_Entry.Set_Name ("Comment" & Row'Image);
         Tracks_Grid.Attach (Comment_Entry, Comment_Col, Track_Row);
         On_Changed (+Comment_Entry, Comment_Changed_CB'Access);

         case Track.File_Type is
            when NONE | UNKNOWN =>
               MIDI_Label := Gtk_Label_New ("No Track");
               Tracks_Grid.Attach (MIDI_Label, Vol_Col, Track_Row);
            when MIDI =>
               MIDI_Label := Gtk_Label_New ("MIDI");
               Tracks_Grid.Attach (MIDI_Label, Vol_Col, Track_Row);
            when others =>
               Gtk.Adjustment.Gtk_New (
                  Adjustment     => Vol_Adj,
                  Value          => Gdouble (Track.Volume),
                  Lower          => 0.0,
                  Upper          => 100.0,
                  Step_Increment => 1.0,
                  Page_Increment => 5.0,
                  Page_Size      => 0.0);
               Gtk.Spin_Button.Gtk_New (Spin_Button => Vol_Spin, Adjustment => Vol_Adj, Climb_Rate => 0.1, The_Digits => 0);
               Vol_Spin.Set_Name ("VolAdj" & Row'Image);
               Vol_Spin.On_Value_Changed (Track_Vol_Changed_CB'Access);
               Tracks_Grid.Attach (Vol_Spin, Vol_Col, Track_Row);
         end case;

         if Show_Track_Modifiers then

            Gtk_New_From_Icon_Name (Track_File_Btn, "folder-symbolic", Icon_Size_Button);
            Track_File_Btn.Set_Tooltip_Text (To_String (Track.Path));
            Track_File_Btn.Set_Name ("FilSel" & Row'Image);
            Track_File_Btn.On_Clicked (Track_File_Btn_CB'Access);
            Tracks_Grid.Attach (Track_File_Btn, File_Col, Track_Row);

            Gtk_New_From_Icon_Name (Track_File_Del_Btn, "edit-clear-symbolic", Icon_Size_Button);
            Track_File_Del_Btn.Set_Tooltip_Text ("Remove file association");
            Track_File_Del_Btn.Set_Name ("FilDel" & Row'Image);
            Track_File_Del_Btn.On_Clicked (Track_File_Del_Btn_CB'Access);
            Tracks_Grid.Attach (Track_File_Del_Btn, File_Del_Col, Track_Row);

            if Integer (Track_Row) < Integer (Sess.Tracks.Length) then
               Gtk_New_From_Icon_Name (Track_Down_Btn, "go-down-symbolic", Icon_Size_Button);
               Track_Down_Btn.Set_Name ("MoveDn" & Row'Image);
               Track_Down_Btn.On_Clicked (Track_Down_Btn_CB'Access);
               Tracks_Grid.Attach (Track_Down_Btn, Down_Col, Track_Row);
            end if;

            if Track_Row > 1 then
               Gtk_New_From_Icon_Name (Track_Up_Btn, "go-up-symbolic", Icon_Size_Button);
               Track_Up_Btn.Set_Name ("MoveUp" & Row'Image);
               Track_Up_Btn.On_Clicked (Track_Up_Btn_CB'Access);
               Tracks_Grid.Attach (Track_Up_Btn, Up_Col, Track_Row);
            end if;

            Gtk_New_From_Icon_Name (Track_Del_Btn, "edit-delete", Icon_Size_Button);
            Track_Del_Btn.Set_Name ("Delete" & Row'Image);
            Track_Del_Btn.On_Clicked (Track_Delete_Btn_CB'Access);
            Tracks_Grid.Attach (Track_Del_Btn, Del_Col, Track_Row);

         end if;

         Track_Row := Track_Row + 1;
      end loop;

      --  Empty track for adding to Sess...
      if Show_Track_Modifiers then
         Display_Empty_Track (Track_Row);
      end if;

      Apply_Css (Tracks_Grid, +CSS_Provider);
      Tracks_Grid.Show_All;
   end Display_Tracks;

end GUI.Tracks;