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
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Container;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Bar;            use Gtk.Menu_Bar;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
--  with Gtk.Radio_Button;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Style_Provider;
with Gtk.Style_Context;       use Gtk.Style_Context;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;

with Interfaces;

with Embedded;                use Embedded;
with Players;                 use Players;
with Session;                 use Session;

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
      else
         Currently_Playing_Track := Currently_Selected_Track;
         Play_Track;
      end if;
   end Play_Btn_CB;

   procedure Stop_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
   end Stop_Btn_CB;

   procedure Track_Select_Btn_CB (Self : access Gtk.Button.Gtk_Button_Record'Class) is
      Name : constant UTF8_String := Self.Get_Name;
      Track_Num : constant Integer := Integer'Value (Name (8 .. Name'Last));
   begin
      if Currently_Selected_Track /= -1 then
         Tracks_Grid.Get_Child_At (1, Gint (Currently_Selected_Track)).Set_Opacity (1.0);
      end if;
      Currently_Selected_Track := Track_Num;
      Tracks_Grid.Get_Child_At (1, Gint (Track_Num)).Set_Opacity (0.5);
   end Track_Select_Btn_CB;

   procedure Display_Tracks is
      Track_Row : Gint := 1;
      Row       : Integer;
      Col       : Gint;
      Track_Down_Btn, Track_Del_Btn, Track_Up_Btn : Gtk.Button.Gtk_Button;
      Row_Label : Gtk.Label.Gtk_Label;
      Title_Entry, Comment_Entry : Gtk.GEntry.Gtk_Entry;
   begin
      --  clear out any existing items
      while Tracks_Grid.Get_Child_At (0, 1) /= null loop
         Tracks_Grid.Remove_Row (1);
      end loop;
      
      for Track of Active_Session.Tracks loop
         Col := 0;

         Gtk.Label.Gtk_New (Row_Label, Track_Row 'Img);
         Tracks_Grid.Attach (Row_Label, Col, Track_Row);
         Col := Col + 1;

         if Track.Path /= Null_Unbounded_String then
            Row := Integer (Track_Row);
            Gtk.Button.Gtk_New_From_Icon_Name (Select_Btn_Arr (Row), "go-next-symbolic", Icon_Size_Button);
            Select_Btn_Arr (Row).Set_Name ("Select" & Row'Image);
            Select_Btn_Arr (Row).On_Clicked (Track_Select_Btn_CB'Access);
            Tracks_Grid.Attach (Select_Btn_Arr (Row), Col, Track_Row);
         end if;
         Col := Col + 1;

         Gtk.GEntry.Gtk_New (Title_Entry);
         Title_Entry.Set_Width_Chars (25);
         Title_Entry.Set_Text (To_String (Track.Title));
         Tracks_Grid.Attach (Title_Entry, Col, Track_Row);
         Col := Col + 1;

         if Integer (Track_Row) < Integer (Active_Session.Tracks.Length) then
            Gtk.Button.Gtk_New_From_Icon_Name (Track_Down_Btn, "go-down-symbolic", Icon_Size_Button);
            Tracks_Grid.Attach (Track_Down_Btn, Col, Track_Row);
         end if;
         Col := Col + 1;

         Gtk.Button.Gtk_New_From_Icon_Name (Track_Del_Btn, "edit-delete", Icon_Size_Button);
         Tracks_Grid.Attach (Track_Del_Btn, Col, Track_Row);
         Col := Col + 1;

         if Track_Row > 1 then
            Gtk.Button.Gtk_New_From_Icon_Name (Track_Up_Btn, "go-up-symbolic", Icon_Size_Button);
            Tracks_Grid.Attach (Track_Up_Btn, Col, Track_Row);
         end if;
         Col := Col + 1;

         Gtk.GEntry.Gtk_New (Comment_Entry);
         Comment_Entry.Set_Width_Chars (40);
         Comment_Entry.Set_Text (To_String (Track.Comment));
         Tracks_Grid.Attach (Comment_Entry, Col, Track_Row);
         Col := Col + 1;

         Track_Row := Track_Row + 1;
      end loop;

      Apply_Css (Tracks_Grid, +CSS_Provider);
      Tracks_Grid.Show_All;
   end Display_Tracks;

   procedure Session_Load_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         Gtkada.File_Selection.File_Selection_Dialog (Title => App_Title & " Load Session",
                                                      Dir_Only => False,
                                                      Must_Exist => True);
      Unused_Buttons : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if Filename'Length > 1 then
         Session.Load_Session (Filename);
         Session_Desc_Entry.Set_Text (To_String (Active_Session.Desc));
         Session_Comment_Entry.Set_Text (To_String (Active_Session.Comment));
         if Active_Session.Tracks.Length > 0 then
            Display_Tracks;
         end if;
      end if;
   exception
      when E : others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not load Session TOML file.  " & Exception_Message (E),
                                           Dialog_Type => Warning,
                                           Title => App_Title & " - Error");
   end Session_Load_CB;

   procedure Advance_Selected_Track is
   begin
      --  Only try to advance if we are not already at the last track
      if Currently_Selected_Track > 0 and then Currently_Selected_Track < Integer (Active_Session.Tracks.Length) then
         Select_Btn_Arr (Currently_Selected_Track + 1).Clicked;
      end if;
   end Advance_Selected_Track;

   function Update_Status_Box_CB (SB : Gtk.Box.Gtk_Box) return Boolean is
   begin
      Gdk.Threads.Enter;

      if Player_Active then
         Active_Label.Set_Text ("Playing: " & To_String (Active_Session.Tracks (Currently_Playing_Track).Title));
         Currently_Active := True;
      else
         if Currently_Active then  --  we have transitioned from playing to not playing
            Advance_Selected_Track;
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
      --  Removing the main window closes the application...
      App.Remove_Window (Main_Window);
   end Quit_CB;

   --  --------  --
   --  BUILDERS  --

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Sep_Item : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      File_Menu, Edit_Menu, Session_Menu, Help_Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Logging_Item,
      Session_Load_Item,
      Quit_Item,
      Paste_Item,
      About_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      --  Log (DEBUG, "Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);

      --  File

      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);

      Gtk_New (Logging_Item, "Logging");
      File_Menu.Append (Logging_Item);
      --  Logging_Item.On_Activate (Logging_CB'Access);

      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

      Gtk_New (Quit_Item, "Quit");
      File_Menu.Append (Quit_Item);
      Quit_Item.On_Activate (Quit_CB'Access);

      --  Edit

      Gtk_New (Menu_Item, "Edit");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Edit_Menu);
      Menu_Item.Set_Submenu (Edit_Menu);
      Gtk_New (Paste_Item, "Paste");
      Edit_Menu.Append (Paste_Item);
      --  Paste_Item.On_Activate (Paste_CB'Access);

      --  --  View
      --  Gtk_New (Menu_Item, "View");
      --  Menu_Bar.Append (Menu_Item);
      --  Gtk_New (View_Menu);
      --  Menu_Item.Set_Submenu (View_Menu);

      --  Gtk_New (History_Item, "History");
      --  View_Menu.Append (History_Item);
      --  History_Item.On_Activate (View_History_CB'Access);

      --  Gtk_New (Sep_Item);
      --  View_Menu.Append (Sep_Item);
      --  Gtk_New (Load_Template_Item, "Load Func. Key Template");
      --  View_Menu.Append (Load_Template_Item);
      --  --  Load_Template_Item.On_Activate (Load_Template_CB'Access);
      --  Gtk_New (Hide_Template_Item, "Hide Func. Key Template");
      --  View_Menu.Append (Hide_Template_Item);
      --  Hide_Template_Item.Set_Sensitive (False);
      --  --  Hide_Template_Item.On_Activate (Hide_Template_CB'Access);

      --  Session

      Gtk_New (Menu_Item, "Session");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Session_Menu);
      Menu_Item.Set_Submenu (Session_Menu);

      --  Session Load
      Gtk_New (Session_Load_Item, "Load Session");
      Session_Menu.Append (Session_Load_Item);
      Session_Load_Item.On_Activate (Session_Load_CB'Access);

      --  Help

      Gtk_New (Menu_Item, "Help");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (Help_Menu);
      Menu_Item.Set_Submenu (Help_Menu);

      Gtk_New (About_Item, "About");
      Help_Menu.Append (About_Item);
      About_Item.On_Activate (About_CB'Access);

      return Menu_Bar;
   end Create_Menu_Bar;

   function Create_Controls_Grid return Gtk_Grid is
      Controls_Grid : Gtk_Grid;
      Play_Btn, Stop_Btn, Restart_Btn, Next_Btn : Gtk.Button.Gtk_Button;
   begin
      Gtk_New (Controls_Grid);
      Controls_Grid.Set_Column_Spacing (10);
      Gtk.Button.Gtk_New_From_Icon_Name (Play_Btn, "media-playback-start", Icon_Size_Dialog);
      Play_Btn.Set_Image_Position (Pos_Top);
      Play_Btn.Set_Label ("Play");
      Play_Btn.On_Clicked (Play_Btn_CB'Access);
      Controls_Grid.Attach (Play_Btn, 0, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Stop_Btn, "media-playback-stop", Icon_Size_Dialog);
      Stop_Btn.Set_Image_Position (Pos_Top);
      Stop_Btn.Set_Label ("Stop");
      Stop_Btn.On_Clicked (Stop_Btn_CB'Access);
      Controls_Grid.Attach (Stop_Btn, 1, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Restart_Btn, "media-playlist-repeat", Icon_Size_Dialog);
      Restart_Btn.Set_Image_Position (Pos_Top);
      Restart_Btn.Set_Label ("Restart!");
      Controls_Grid.Attach (Restart_Btn, 2, 0);

      Gtk.Button.Gtk_New_From_Icon_Name (Next_Btn, "media-skip-forward", Icon_Size_Dialog);
      Next_Btn.Set_Image_Position (Pos_Top);
      Next_Btn.Set_Label ("Next");
      Controls_Grid.Attach (Next_Btn, 3, 0);

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
      Session_Desc_Entry.Set_Width_Chars (80);
      Session_Header_Grid.Attach (Child => Session_Desc_Entry, Left => 1, Top => 0);
      Gtk_New (Comment_Label, " Notes: ");
      Session_Header_Grid.Attach (Child => Comment_Label, Left => 0, Top => 1);
      Gtk.GEntry.Gtk_New (Session_Comment_Entry);
      Session_Comment_Entry.Set_Width_Chars (132);
      Session_Header_Grid.Attach (Child => Session_Comment_Entry, Left => 1, Top => 1);
      Main_Box.Pack_Start (Child => Session_Header_Grid, Expand => False);

      --  Tracks Grid
      Gtk_New (Tracks_Grid);
      Main_Box.Pack_Start (Child => Tracks_Grid);

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