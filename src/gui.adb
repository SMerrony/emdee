--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Containers;          use Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Sequential_IO;
--  with Ada.Streams;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

--  with Gdk.Screen;
with Gdk.Threads;

with Glib;                    use Glib;
with Glib.Application;        use Glib.Application;
with Glib.Error;              use Glib.Error;

with Gtk.About_Dialog;        use Gtk.About_Dialog;
with Gtk.Adjustment;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Container;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
--  with Gtk.Radio_Button;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Spin_Button;
with Gtk.Style_Provider;
with Gtk.Style_Context;       use Gtk.Style_Context;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;

with Interfaces;

with Embedded;                use Embedded;
with Players;                 use Players;
with Session;                 use Session;
with Track;                   use Track;

package body GUI is

   --  Internal Helpers

   package FA is new Gtk.Container.Forall_User_Data (Gtk.Style_Provider.Gtk_Style_Provider);

   procedure Apply_Css (Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
                        Provider : Gtk.Style_Provider.Gtk_Style_Provider) is
   --  Apply the given CSS to the widget (which may be a container)
   begin
      Gtk.Style_Context.Get_Style_Context (Widget).Add_Provider (Provider, Glib.Guint'Last);
      if Widget.all in Gtk.Container.Gtk_Container_Record'Class then
         declare
            Container : constant Gtk.Container.Gtk_Container := Gtk.Container.Gtk_Container (Widget);
         begin
            FA.Forall (Container, Apply_Css'Unrestricted_Access, Provider);
         end;
      end if;
   end Apply_Css;

   function Create_Icon_Pixbuf return Gdk.Pixbuf.Gdk_Pixbuf is
      IP :  Gdk.Pixbuf.Gdk_Pixbuf;
      Icon_Emb : constant Embedded.Content_Type := Embedded.Get_Content (App_Icon);
      package IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);
      Tmp_Filename : constant String := "emdee_Icon.tmp";
      Tmp_File : IO.File_Type;
      Error : aliased Glib.Error.GError;
   begin
      if Ada.Directories.Exists (Tmp_Filename) then
         Ada.Directories.Delete_File (Tmp_Filename);
      end if;
      IO.Create (File => Tmp_File, Name => Tmp_Filename);
      for Val of Icon_Emb.Content.all loop
         IO.Write (Tmp_File, Interfaces.Unsigned_8 (Val));
      end loop;
      IO.Close (Tmp_File);
      Gdk.Pixbuf.Gdk_New_From_File (Pixbuf => IP, Filename => Tmp_Filename, Error => Error);
      if Error /= null then
         Ada.Text_IO.Put_Line ("WARNING: Could not find/load icon file: emdee_Icon.tmp");
      end if;
      Ada.Directories.Delete_File (Tmp_Filename);
      return IP;
   end Create_Icon_Pixbuf;

   --  CALLBACKS

   procedure About_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_About_Dialog;
      Dummy_Response : Gtk_Response_Type;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Logo (Icon_PB);
      Dialog.Set_Authors ((1 => new String'(App_Author)));
      Dialog.Set_Copyright (App_Copyright);
      Dialog.Set_Comments (App_Comment);
      Dialog.Set_Program_Name (App_Title);
      Dialog.Set_Version (App_SemVer);
      Dialog.Set_Website (App_Website);
      Dummy_Response := Dialog.Run;
      Dialog.Destroy;
   end About_CB;

   procedure Play_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Player_Active then
         Unused_Buttons := Message_Dialog (Msg => "Already playing a track",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Currently_Selected_Track = -1 then
         Unused_Buttons := Message_Dialog (Msg => "No track is selected",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Active_Session.Tracks (Currently_Selected_Track).Path = Null_Unbounded_String then
         Unused_Buttons := Message_Dialog (Msg => "Track has no media file to play",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Active_Session.Tracks (Currently_Selected_Track).File_Type = MIDI and then Active_Session.MIDI_Port = Null_Unbounded_String then
         Unused_Buttons := Message_Dialog (Msg => "No MIDI Port has been set in this session",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      else
         Currently_Playing_Track := Currently_Selected_Track;
         Play_Track;
      end if;
   end Play_Btn_CB;

   --  procedure Louder_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --     Vol : constant Natural := Get_System_Volume;
   --  begin
   --     if Vol < 91 then
   --        Adjust_System_Volume (Vol + 10);
   --     end if;
   --  end Louder_Btn_CB;

   --  procedure Vol_Reset_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --  begin
   --     Adjust_System_Volume (100);
   --  end Vol_Reset_Btn_CB;

   --  procedure Quieter_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --     Vol : constant Natural := Get_System_Volume;
   --  begin
   --     if Vol > 9 then
   --        Adjust_System_Volume (Vol - 10);
   --     end if;
   --  end Quieter_Btn_CB;

   procedure Stop_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
   end Stop_Btn_CB;

   procedure Next_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
      Select_Next_Track;
   end Next_Btn_CB;

   procedure Previous_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
      Select_Previous_Track;
   end Previous_Btn_CB;

   procedure Track_Select_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      Name : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer := Integer'Value (Name (8 .. Name'Last));
   begin
      if Currently_Selected_Track /= -1 then
         --  Tracks_Grid.Get_Child_At (1, Gint (Currently_Selected_Track)).Set_Opacity (0.5);
         Tracks_Grid.Get_Child_At (Title_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
         Tracks_Grid.Get_Child_At (Comment_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
         Tracks_Grid.Get_Child_At (Vol_Col, Gint (Currently_Selected_Track)).Set_Name ("not-highlit");
      end if;
      Currently_Selected_Track := Track_Num;
      --  Tracks_Grid.Get_Child_At (1, Gint (Track_Num)).Set_Opacity (1.0);
      Tracks_Grid.Get_Child_At (Title_Col, Gint (Track_Num)).Set_Name ("highlit");
      Tracks_Grid.Get_Child_At (Comment_Col, Gint (Track_Num)).Set_Name ("highlit");
      Tracks_Grid.Get_Child_At (Vol_Col, Gint (Track_Num)).Set_Name ("highlit");
   end Track_Select_Btn_CB;

   procedure Track_Modifiers_CB (Self : access Gtk_Check_Menu_Item_Record'Class) is

   begin
      Show_Track_Modifiers := Self.Get_Active;
      Display_Tracks;
   end Track_Modifiers_CB;

   procedure Display_Track_Headers is

   begin
      --  Tracks_Grid.Attach (Gtk_Label_New ("#"), Row_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Title"), Title_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Skip"), Skip_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Comment"), Comment_Col, 0);
      Tracks_Grid.Attach (Gtk_Label_New ("Volume (%)"), Vol_Col, 0);
   end Display_Track_Headers;

   procedure Display_Tracks is
      Track_Row  : Gint := 1;
      Row        : Integer;
      Track_Down_Btn, Track_Del_Btn, Track_Up_Btn, Track_File_Btn : Gtk.Button.Gtk_Button;
      Skip_Check : Gtk.Check_Button.Gtk_Check_Button;
      Row_Label  : Gtk.Label.Gtk_Label;
      Title_Entry, Comment_Entry : Gtk.GEntry.Gtk_Entry;
      Vol_Adj    : Gtk.Adjustment.Gtk_Adjustment;
      Vol_Spin   : Gtk.Spin_Button.Gtk_Spin_Button;
   begin
      --  clear out any existing items
      while Tracks_Grid.Get_Child_At (0, 1) /= null loop
         Tracks_Grid.Remove_Row (1);
      end loop;

      for Track of Active_Session.Tracks loop
         Gtk.Label.Gtk_New (Row_Label, Track_Row'Img);
         Row_Label.Set_Halign (Align_Center);
         Tracks_Grid.Attach (Row_Label, Row_Col, Track_Row);

         Row := Integer (Track_Row);
         Gtk.Button.Gtk_New_From_Icon_Name (Select_Btn_Arr (Row), "go-next-symbolic", Icon_Size_Button);
         Select_Btn_Arr (Row).Set_Name ("Select" & Row'Image);
         Select_Btn_Arr (Row).On_Clicked (Track_Select_Btn_CB'Access);
         Tracks_Grid.Attach (Select_Btn_Arr (Row), Select_Col, Track_Row);

         Gtk.GEntry.Gtk_New (Title_Entry);
         Title_Entry.Set_Width_Chars (25);
         Title_Entry.Set_Text (To_String (Track.Title));
         Tracks_Grid.Attach (Title_Entry, Title_Col, Track_Row);

         Gtk.Check_Button.Gtk_New (Skip_Check, "");
         Skip_Check.Set_Active (Track.Skip);
         Skip_Check.Set_Halign (Align_Center);
         Tracks_Grid.Attach (Skip_Check, Skip_Col, Track_Row);

         Gtk.GEntry.Gtk_New (Comment_Entry);
         Comment_Entry.Set_Width_Chars (40);
         Comment_Entry.Set_Text (To_String (Track.Comment));
         Tracks_Grid.Attach (Comment_Entry, Comment_Col, Track_Row);

         Gtk.Adjustment.Gtk_New (
            Adjustment     => Vol_Adj,
            Value          => Gdouble (Track.Volume),
            Lower          => 0.0,
            Upper          => 100.0,
            Step_Increment => 1.0,
            Page_Increment => 5.0,
            Page_Size      => 0.0);
         Gtk.Spin_Button.Gtk_New (
            Spin_Button => Vol_Spin,
            Adjustment  => Vol_Adj,
            Climb_Rate  => 0.1,
            The_Digits  => 0);
         Tracks_Grid.Attach (Vol_Spin, Vol_Col, Track_Row);

         if Show_Track_Modifiers then

            Gtk.Button.Gtk_New_From_Icon_Name (Track_File_Btn, "folder-symbolic", Icon_Size_Button);
            Track_File_Btn.Set_Tooltip_Text (To_String (Track.Path));
            Tracks_Grid.Attach (Track_File_Btn, File_Col, Track_Row);

            if Integer (Track_Row) < Integer (Active_Session.Tracks.Length) then
               Gtk.Button.Gtk_New_From_Icon_Name (Track_Down_Btn, "go-down-symbolic", Icon_Size_Button);
               Tracks_Grid.Attach (Track_Down_Btn, Down_Col, Track_Row);
            end if;

            if Track_Row > 1 then
               Gtk.Button.Gtk_New_From_Icon_Name (Track_Up_Btn, "go-up-symbolic", Icon_Size_Button);
               Tracks_Grid.Attach (Track_Up_Btn, Up_Col, Track_Row);
            end if;

            Gtk.Button.Gtk_New_From_Icon_Name (Track_Del_Btn, "edit-delete", Icon_Size_Button);
            Tracks_Grid.Attach (Track_Del_Btn, Del_Col, Track_Row);

         end if;

         Track_Row := Track_Row + 1;
      end loop;

      Apply_Css (Tracks_Grid, +CSS_Provider);
      Tracks_Grid.Show_All;
   end Display_Tracks;

   procedure Session_Open_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => App_Title & " Open Session",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 1 then
         Session.Load_Session (Filename);
         Active_Session.Filename := To_Unbounded_String (Filename);
         Session_Desc_Entry.Set_Text (To_String (Active_Session.Desc));
         Session_Comment_Entry.Set_Text (To_String (Active_Session.Comment));
         if Active_Session.MIDI_Port /= Null_Unbounded_String then
            Create_Notes_Off_MIDI;
         end if;
         if Active_Session.Tracks.Length > 0 then
            Display_Tracks;
         end if;
      end if;
   exception
      when E : others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not open Session TOML file.  " & Exception_Message (E),
                                           Dialog_Type => Warning,
                                           Title => App_Title & " - Error");
         Active_Session.Filename := Null_Unbounded_String;
   end Session_Open_CB;

   procedure Session_Save_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Active_Session.Filename = Null_Unbounded_String then
         Unused_Buttons := Message_Dialog (Msg => "No Session has been loaded, cannot save.",
                                           Dialog_Type => Warning,
                                           Title => App_Title & " - Error");
      else
         Session.Save_Session (To_String (Active_Session.Filename));
      end if;
   end Session_Save_CB;

   procedure Session_Save_As_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => App_Title & " - Save Session As...",
                                                      Dir_Only => False,
                                                      Must_Exist => False);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 0 then
         if Ada.Directories.Exists (Filename) then
            Unused_Buttons := Message_Dialog (Msg => "Session TOML file already exists, use 'Save' to overwrite.",
                                              Dialog_Type => Warning,
                                              Title => App_Title & " - Oops");
         else
            Session.Save_Session (Filename);
         end if;
      end if;
   end Session_Save_As_CB;

   procedure Session_MIDI_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog : Gtk_Dialog;
      Dlg_Box   : Gtk.Box.Gtk_Box;
      Dlg_Port_Label : Gtk.Label.Gtk_Label;
      Port_Entry : Gtk.GEntry.Gtk_Entry;
      Cancel_Unused, Save_Unused : Gtk.Widget.Gtk_Widget;
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - Session MIDI Port");
      Dlg_Box := Dialog.Get_Content_Area;
      Gtk.Label.Gtk_New (Dlg_Port_Label, "MIDI Out Port:");
      Dlg_Box.Pack_Start (Child => Dlg_Port_Label, Expand => True, Fill => True, Padding => 5);
      Gtk.GEntry.Gtk_New (The_Entry => Port_Entry);
      if Active_Session.MIDI_Port /= Null_Unbounded_String then
         Port_Entry.Set_Text (To_String (Active_Session.MIDI_Port));
      end if;
      Port_Entry.Set_Tooltip_Text ("MIDI port suitable for aplaymidi to use in form nn:n.  Use aplaymidi -l to see list");
      Dlg_Box.Pack_Start (Child => Port_Entry, Expand => True, Fill => True, Padding => 5);
      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Save_Unused := Dialog.Add_Button ("Save", Gtk_Response_Accept);
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then
         null; --  TODO write MIDI port to TOML
      end if;
      Dialog.Destroy;
   end Session_MIDI_CB;

   procedure Select_Next_Track is
      Candidate_Track : Integer := Currently_Selected_Track;
   begin
      if Currently_Selected_Track /= -1 and then Currently_Selected_Track < (Integer (Active_Session.Tracks.Length)) then --  Not already at end
         loop
            Candidate_Track := Candidate_Track + 1;
            exit when Candidate_Track = Integer (Active_Session.Tracks.Length);
            exit when not Active_Session.Tracks (Candidate_Track).Skip;
         end loop;
         Select_Btn_Arr (Candidate_Track).Clicked;
      end if;
   end Select_Next_Track;

   procedure Select_Previous_Track is
      Candidate_Track : Integer := Currently_Selected_Track;
   begin
      if Currently_Selected_Track /= -1 and then Currently_Selected_Track > 1 then --  Not already at start
         loop
            Candidate_Track := Candidate_Track - 1;
            exit when Candidate_Track = Integer (Active_Session.Tracks.Length);
            exit when not Active_Session.Tracks (Candidate_Track).Skip;
         end loop;
         Select_Btn_Arr (Candidate_Track).Clicked;
      end if;
   end Select_Previous_Track;

   function Update_Status_Box_CB (SB : Gtk.Box.Gtk_Box) return Boolean is
   begin
      if Shutting_Down then
         return False;
      end if;

      Gdk.Threads.Enter;
         if Player_Active then
            Active_Label.Set_Text ("Playing: " & To_String (Active_Session.Tracks (Currently_Playing_Track).Title));
            Currently_Active := True;
         else
            if Currently_Active then  --  we have transitioned from playing to not playing
               Select_Next_Track;
               Currently_Active := False;
            end if;
            Active_Label.Set_Text ("Not Playing");
         end if;
         SB.Queue_Draw;
      Gdk.Threads.Leave;
      return True;
   end Update_Status_Box_CB;

   procedure Quit_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Shutting_Down := True;
      delay 0.5;
      --  Removing the main window closes the application...
      App.Remove_Window (Main_Window);
   end Quit_CB;

   procedure Window_Closed_CB (Self : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Shutting_Down := True;  --  Time for Update_Status_Box_CB to finish up
      delay 0.5;
   end Window_Closed_CB;

   --  --------  --
   --  BUILDERS  --

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Sep_Item : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      File_Menu, Session_Menu, Help_Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Session_Open_Item, Session_Save_Item, Session_Save_As_Item, Session_Create_Item, Session_MIDI_Item,
      Quit_Item,
      About_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Track_Modifiers_Check_Item : Gtk.Check_Menu_Item.Gtk_Check_Menu_Item;
   begin
      --  Log (DEBUG, "Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);

      --  File

      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);

      --  Session Create
      Gtk_New (Session_Create_Item, "New Session");
      File_Menu.Append (Session_Create_Item);
      --  Session_Create_Item.On_Activate (Session_Create_CB'Access);

      --  Session Open
      Gtk_New (Session_Open_Item, "Open Session");
      File_Menu.Append (Session_Open_Item);
      Session_Open_Item.On_Activate (Session_Open_CB'Access);

      --  Session Save
      Gtk_New (Session_Save_Item, "Save Session");
      File_Menu.Append (Session_Save_Item);
      Session_Save_Item.On_Activate (Session_Save_CB'Access);

      --  Session Save As
      Gtk_New (Session_Save_As_Item, "Save Session As...");
      File_Menu.Append (Session_Save_As_Item);
      Session_Save_As_Item.On_Activate (Session_Save_As_CB'Access);

      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Quit_Item, "Quit");
      File_Menu.Append (Quit_Item);
      Quit_Item.On_Activate (Quit_CB'Access);

      --  Session

      Gtk_New (Menu_Item, "Session");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Session_Menu);
      Menu_Item.Set_Submenu (Session_Menu);

      --  Session MIDI Settings
      Gtk_New (Session_MIDI_Item, "MIDI Settings");
      Session_Menu.Append (Session_MIDI_Item);
      Session_MIDI_Item.On_Activate (Session_MIDI_CB'Access);

      Gtk_New (Sep_Item);
      Session_Menu.Append (Sep_Item);

      Gtk_New (Sep_Item);
      Session_Menu.Append (Sep_Item);

      --  Modifiers visible
      Gtk_New (Track_Modifiers_Check_Item, "Show Track Modifiers");
      Track_Modifiers_Check_Item.Set_Active (Show_Track_Modifiers);
      Session_Menu.Append (Track_Modifiers_Check_Item);
      Track_Modifiers_Check_Item.On_Toggled (Track_Modifiers_CB'Access);

      --  Help

      Gtk_New (Menu_Item, "Help");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Help_Menu);
      Menu_Item.Set_Submenu (Help_Menu);
      --  About
      Gtk_New (About_Item, "About");
      Help_Menu.Append (About_Item);
      About_Item.On_Activate (About_CB'Access);

      return Menu_Bar;
   end Create_Menu_Bar;

   function Create_Controls_Grid return Gtk_Grid is
      Controls_Grid : Gtk_Grid;
      Spacer_Lab    : Gtk_Label;
      Play_Btn, Stop_Btn, Previous_Btn, Next_Btn : Gtk.Button.Gtk_Button;
      --  Quieter_Btn, Vol_Reset_Btn, Louder_Btn : Gtk.Button.Gtk_Button;
   begin
      Gtk_New (Controls_Grid);
      Controls_Grid.Set_Column_Spacing (10);
      Controls_Grid.Set_Column_Homogeneous (True);
      Gtk.Label.Gtk_New (Spacer_Lab, "");

      Gtk.Button.Gtk_New_From_Icon_Name (Play_Btn, "media-playback-start", Icon_Size_Dialog);
      Play_Btn.Set_Image_Position (Pos_Top);
      Play_Btn.Set_Label ("Play");
      Play_Btn.On_Clicked (Play_Btn_CB'Access);
      Controls_Grid.Attach (Play_Btn, 0, 0);

      --  Gtk.Button.Gtk_New_From_Icon_Name (Pause_Btn, "media-playback-pause", Icon_Size_Dialog);
      --  Pause_Btn.Set_Image_Position (Pos_Top);
      --  Pause_Btn.Set_Label ("Pause");
      --  --  Pause_Btn.On_Clicked (Pause_Btn_CB'Access);
      --  Controls_Grid.Attach (Pause_Btn, 1, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Stop_Btn, "media-playback-stop", Icon_Size_Dialog);
      Stop_Btn.Set_Image_Position (Pos_Top);
      Stop_Btn.Set_Label ("Stop");
      Stop_Btn.On_Clicked (Stop_Btn_CB'Access);
      Controls_Grid.Attach (Stop_Btn, 2, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Previous_Btn, "media-skip-backward", Icon_Size_Dialog);
      Previous_Btn.Set_Image_Position (Pos_Top);
      Previous_Btn.Set_Label ("Previous");
      Previous_Btn.On_Clicked (Previous_Btn_CB'Access);
      Controls_Grid.Attach (Previous_Btn, 3, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Next_Btn, "media-skip-forward", Icon_Size_Dialog);
      Next_Btn.Set_Image_Position (Pos_Top);
      Next_Btn.Set_Label ("Next");
      Next_Btn.On_Clicked (Next_Btn_CB'Access);
      Controls_Grid.Attach (Next_Btn, 4, 0);

      --  Controls_Grid.Attach (Spacer_Lab, 5, 0);

      --  Gtk.Button.Gtk_New_From_Icon_Name (Quieter_Btn, "audio-volume-low", Icon_Size_Dialog);
      --  Quieter_Btn.Set_Image_Position (Pos_Top);
      --  Quieter_Btn.Set_Label ("Vol -");
      --  Quieter_Btn.On_Clicked (Quieter_Btn_CB'Access);
      --  Controls_Grid.Attach (Quieter_Btn, 6, 0);

      --  Gtk.Button.Gtk_New_From_Icon_Name (Vol_Reset_Btn, "audio-volume-medium", Icon_Size_Dialog);
      --  Vol_Reset_Btn.Set_Image_Position (Pos_Top);
      --  Vol_Reset_Btn.Set_Label ("Reset ");
      --  Vol_Reset_Btn.On_Clicked (Vol_Reset_Btn_CB'Access);
      --  Controls_Grid.Attach (Vol_Reset_Btn, 7, 0);

      --  Gtk.Button.Gtk_New_From_Icon_Name (Louder_Btn, "audio-volume-high", Icon_Size_Dialog);
      --  Louder_Btn.Set_Image_Position (Pos_Top);
      --  Louder_Btn.Set_Label ("Vol +");
      --  Louder_Btn.On_Clicked (Louder_Btn_CB'Access);
      --  Controls_Grid.Attach (Louder_Btn, 8, 0);

      return Controls_Grid;
   end Create_Controls_Grid;

   function Create_Status_Box return Gtk_Box is
      Status_Box : Gtk_Box;
      Active_Frame : Gtk.Frame.Gtk_Frame;
   begin
      Gtk_New (Status_Box, Gtk.Enums.Orientation_Horizontal, 2);

      Gtk.Frame.Gtk_New (Active_Frame);
      Gtk.Label.Gtk_New (Active_Label, " ");
      Active_Frame.Add (Active_Label);
      Status_Box.Pack_Start (Active_Frame);

      --  Gtk.Frame.Gtk_New (Players_Frame);
      --  Gtk.Label.Gtk_New (Players_Label, "(Players Configuration not Loaded)");
      --  Players_Frame.Add (Players_Label);
      --  Status_Box.Pack_Start (Players_Frame);
      --  Status_Box.Set_Hexpand (True);

      SB_Timeout := SB_Timeout_P.Timeout_Add (SB_Update_MS, Update_Status_Box_CB'Access, Status_Box);

      return Status_Box;
   end Create_Status_Box;

   -- ----------- --
   -- Application --

   procedure App_Activate (Self : access Gapplication_Record'Class) is
      pragma Unreferenced (Self);
      Session_Label, Comment_Label : Gtk.Label.Gtk_Label;
      Error : aliased Glib.Error.GError;
      CSS_Emb : constant Embedded.Content_Type := Embedded.Get_Content (App_CSS);
      CSS_US  : Unbounded_String;
   begin
      for C of CSS_Emb.Content.all loop
         Append (CSS_US, Character'Val (C));
      end loop;
      if not CSS_Provider.Load_From_Data (To_String (CSS_US), Error'Access) then
         Ada.Text_IO.Put_Line ("ERROR: Could not load CSS internal resource");
      end if;

      Main_Window := Gtk_Application_Window_New (App);
      Main_Window.Set_Title (App_Title);

      --  Everything is in a Box...
      Gtk.Box.Gtk_New (Main_Box, Gtk.Enums.Orientation_Vertical, 2);

      --  Menu
      Main_Box.Pack_Start (Child => Create_Menu_Bar, Expand => False);

      --  Session Info Header
      Gtk_New (Session_Header_Grid);
      Gtk.GEntry.Gtk_New (Session_Desc_Entry);
      Gtk_New (Session_Label, " Session: ");
      Session_Header_Grid.Attach (Child => Session_Label, Left => 0, Top => 0);
      Session_Desc_Entry.Set_Width_Chars (60);
      Session_Header_Grid.Attach (Child => Session_Desc_Entry, Left => 1, Top => 0);
      Gtk_New (Comment_Label, " Notes: ");
      Session_Header_Grid.Attach (Child => Comment_Label, Left => 0, Top => 1);
      Gtk.GEntry.Gtk_New (Session_Comment_Entry);
      Session_Comment_Entry.Set_Width_Chars (80);
      Session_Header_Grid.Attach (Child => Session_Comment_Entry, Left => 1, Top => 1);
      Main_Box.Pack_Start (Child => Session_Header_Grid, Expand => False);

      --  Tracks Grid
      Gtk_New (Tracks_Grid);
      Main_Box.Pack_Start (Child => Tracks_Grid);
      Display_Track_Headers;

      --  Status Bar
      Main_Box.Pack_End (Child => Create_Status_Box, Expand => False);

      --  Controls Grid
      Main_Box.Pack_End (Child => Create_Controls_Grid, Expand => False);

      Main_Window.Add (Main_Box);
      Main_Window.Resize (500, 400); --  Gotta start somewhere

      --  Icon
      Icon_PB := Create_Icon_Pixbuf;
      Main_Window.Set_Icon (Icon_PB);

      --  Styling - CSS
      Apply_Css (Main_Window, +CSS_Provider);

      Main_Window.Show_All;

      Main_Window.On_Destroy (Window_Closed_CB'Unrestricted_Access);
   end App_Activate;

   procedure Launch is
      Unused_Status : Gint;
   begin
      App := Gtk_Application_New (App_ID, G_Application_Flags_None);
      App.On_Activate (App_Activate'Unrestricted_Access);
      Unused_Status := App.Run;
      App.Unref;
   end Launch;

end GUI;