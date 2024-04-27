--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Gdk.Threads;

with Glib;                    use Glib;
with Glib.Application;        use Glib.Application;
with Glib.Error;              use Glib.Error;
--  with Glib.Values;

with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Frame;
with Gtk.Style_Context;

with Interfaces;

with Embedded;                use Embedded;
with Players;                 use Players;
with Session;                 use Session;

with GUI.Live_Controls;       use GUI.Live_Controls;
with GUI.Menu;                use GUI.Menu;
with GUI.Tracks;              use GUI.Tracks;

package body GUI is

   --  Internal Helpers

   package FA is new Gtk.Container.Forall_User_Data (Gtk.Style_Provider.Gtk_Style_Provider);

   procedure Apply_Css (Widget   : not null access Gtk_Widget_Record'Class;
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

   procedure Resize_Font (New_Size : Font_Size) is
      Error : aliased Glib.Error.GError;
   begin
      if not CSS_Provider.Load_From_Data (Build_CSS (New_Size), Error'Access) then
         Ada.Text_IO.Put_Line ("ERROR: Could not load CSS internal data");
      end if;
      Apply_Css (Main_Window, +CSS_Provider);
      Current_Font_Size := New_Size;
      Main_Window.Resize (1, 1);
      Main_Window.Set_Icon (Icon_PB);
   end Resize_Font;

   function Create_Icon_Pixbuf return Gdk.Pixbuf.Gdk_Pixbuf is
      IP :  Gdk.Pixbuf.Gdk_Pixbuf;
      Icon_Emb : constant Embedded.Content_Type := Embedded.Get_Content (App_Icon);
      package IO is new Ada.Sequential_IO (Interfaces.Unsigned_8);
      Tmp_Filename : constant String := "emdee_Icon.tmp";
      Tmp_File : IO.File_Type;
      Error : aliased GError;
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

   procedure Update_Text_Fields is
   --  This proc should be called before saving a sesssion or moving Tracks
   --  to ensure the Session is in sync with the screen contents.
   --  N.B. It would be more robust to do these updates whenever the text fields
   --       are changed by the user, but I couldn't find the right event(s)...
      Track_Row  : Gint := 1;
   begin
      Sess.Desc := To_Unbounded_String (Session_Desc_Entry.Get_Text);
      Sess.Comment := To_Unbounded_String (Session_Comment_Entry.Get_Text);
      for Track of Sess.Tracks loop
         Track.Title := To_Unbounded_String (
                           Gtk_Entry (
                              Tracks_Grid.Get_Child_At (Title_Col, Track_Row)).Get_Text);
         Track.Comment := To_Unbounded_String (
                           Gtk_Entry (
                              Tracks_Grid.Get_Child_At (Comment_Col, Track_Row)).Get_Text);
         Track_Row := Track_Row + 1;
      end loop;
   end Update_Text_Fields;

   procedure Select_Next_Track is
      Candidate_Track : Integer := Currently_Selected_Track;
   begin
      if Currently_Selected_Track /= -1 and then Currently_Selected_Track < (Integer (Sess.Tracks.Length)) then --  Not already at end
         loop
            Candidate_Track := Candidate_Track + 1;
            exit when Candidate_Track = Integer (Sess.Tracks.Length);
            exit when not Sess.Tracks (Candidate_Track).Skip;
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
            exit when Candidate_Track = Integer (Sess.Tracks.Length);
            exit when not Sess.Tracks (Candidate_Track).Skip;
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
            Active_Label.Set_Text ("Playing: " & To_String (Sess.Tracks (Currently_Playing_Track).Title));
            Currently_Active := True;
            GUI.Live_Controls.Play_Btn.Set_Sensitive (False);
            GUI.Live_Controls.Previous_Btn.Set_Sensitive (False);
            GUI.Live_Controls.Next_Btn.Set_Sensitive (False);
            GUI.Live_Controls.Stop_Btn.Set_Sensitive (True);
         else
            if Currently_Active then  --  we have transitioned from playing to not playing
               Select_Next_Track;
               Currently_Active := False;
            end if;
            Active_Label.Set_Text ("Not Playing");
            GUI.Live_Controls.Play_Btn.Set_Sensitive (True);
            GUI.Live_Controls.Previous_Btn.Set_Sensitive (True);
            GUI.Live_Controls.Next_Btn.Set_Sensitive (True);
            GUI.Live_Controls.Stop_Btn.Set_Sensitive (False);
         end if;
         SB.Queue_Draw;
      Gdk.Threads.Leave;
      return True;
   end Update_Status_Box_CB;

   procedure Window_Closed_CB (Self : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Shutting_Down := True;  --  Time for Update_Status_Box_CB to finish up
      delay 0.5;
   end Window_Closed_CB;

   function Create_Status_Box return Gtk_Box is
      Status_Box : Gtk_Box;
      Active_Frame : Gtk.Frame.Gtk_Frame;
   begin
      Gtk_New (Status_Box, Gtk.Enums.Orientation_Horizontal, 2);

      Gtk.Frame.Gtk_New (Active_Frame);
      Gtk_New (Active_Label, " ");
      Active_Frame.Add (Active_Label);
      Status_Box.Pack_Start (Active_Frame);

      SB_Timeout := SB_Timeout_P.Timeout_Add (SB_Update_MS, Update_Status_Box_CB'Access, Status_Box);

      return Status_Box;
   end Create_Status_Box;

   function Build_CSS (Size : Font_Size) return String is
   begin
      --  ("grid {font-size: " & Font_Point_Size'Image & "pt; }" & ASCII.LF & Fixed_CSS_Str);
      case Size is
         when S   => return "window {font-size: small; }" & ASCII.LF & Fixed_CSS_Str;
         when M   => return "window {font-size: medium; }" & ASCII.LF & Fixed_CSS_Str;
         when L   => return "window {font-size: large; }" & ASCII.LF & Fixed_CSS_Str;
         when XL  => return "window {font-size: x-large; }" & ASCII.LF & Fixed_CSS_Str;
         when XXL => return "window {font-size: xx-large; }" & ASCII.LF & Fixed_CSS_Str;
      end case;
   end Build_CSS;

   procedure App_Activate (Self : access Gapplication_Record'Class) is
      pragma Unreferenced (Self);
      Session_Label, Comment_Label : Gtk_Label;
      Error : aliased GError;
   begin
      Main_Window := Gtk_Application_Window_New (App);
      Main_Window.Set_Title (App_Title);

      if not CSS_Provider.Load_From_Data (Build_CSS (M), Error'Access) then
         Ada.Text_IO.Put_Line ("ERROR: Could not load CSS internal data");
      end if;

      --  Everything is in a Box...
      Gtk.Box.Gtk_New (Main_Box, Gtk.Enums.Orientation_Vertical, 2);

      --  Menu
      Main_Box.Pack_Start (Child => Create_Menu_Bar, Expand => False);

      --  Session Info Header
      Gtk_New (Session_Header_Grid);
      Session_Header_Grid.Set_Column_Spacing (8);

      Gtk_New (Session_Label, " Session: ");

      Session_Header_Grid.Attach (Child => Session_Label, Left => 0, Top => 0);
      Gtk_New (Session_Desc_Entry);
      Session_Desc_Entry.Set_Width_Chars (60);
      --  Session_Desc_Entry.On_Focus_Out_Event ... TODO probably better than Update_Text_Fields
      Session_Header_Grid.Attach (Child => Session_Desc_Entry, Left => 1, Top => 0);

      Gtk_New (Comment_Label, " Notes: ");
      Session_Header_Grid.Attach (Child => Comment_Label, Left => 0, Top => 1);
      Gtk_New (Session_Comment_Entry);
      Session_Comment_Entry.Set_Width_Chars (80);
      Session_Header_Grid.Attach (Child => Session_Comment_Entry, Left => 1, Top => 1);
      Main_Box.Pack_Start (Child => Session_Header_Grid, Expand => False);

      --  Tracks Grid
      Main_Box.Pack_Start (Child => Create_Tracks_Grid);

      --  Status Bar
      Main_Box.Pack_End (Child => Create_Status_Box, Expand => False);

      --  Live Controls Grid
      Main_Box.Pack_End (Child => Create_Live_Controls_Grid, Expand => False);

      Main_Window.Add (Main_Box);
      Main_Window.Resize (1, 1); --  Gotta start somewhere

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