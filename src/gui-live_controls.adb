--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Gtk.Button;              use Gtk.Button;
with Gtk.Enums;               use Gtk.Enums;

with Gtkada.Dialogs;          use Gtkada.Dialogs;

with Players;                 use Players;
with Session;                 use Session;
with Track;                   use Track;

package body GUI.Live_Controls is

   procedure Play_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Player_Active then
         Unused_Buttons := Message_Dialog (Msg => "Already playing a track",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Currently_Selected_Track = -1 then
         Unused_Buttons := Message_Dialog (Msg => "No track is selected",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Sess.Tracks (Currently_Selected_Track).Path = Null_Unbounded_String then
         Unused_Buttons := Message_Dialog (Msg => "Track has no media file to play",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      elsif Sess.Tracks (Currently_Selected_Track).File_Type = MIDI and then Sess.MIDI_Port = Null_Unbounded_String then
         Unused_Buttons := Message_Dialog (Msg => "No MIDI Port has been set in this session",
                                           Title => App_Title & " Cannot Play",
                                           Buttons => Button_OK);
      else
         Currently_Playing_Track := Currently_Selected_Track;
         Play_Track;
      end if;
   end Play_Btn_CB;

   procedure Stop_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
   end Stop_Btn_CB;

   procedure Next_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
      Select_Next_Track;
   end Next_Btn_CB;

   procedure Previous_Btn_CB (Self : access Gtk_Button_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Currently_Active := False;
      Stop_Playing;
      Select_Previous_Track;
   end Previous_Btn_CB;

   --  procedure Louder_Btn_CB (Self : access Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --     Vol : constant Natural := Get_System_Volume;
   --  begin
   --     if Vol < 91 then
   --        Adjust_System_Volume (Vol + 10);
   --     end if;
   --  end Louder_Btn_CB;

   --  procedure Vol_Reset_Btn_CB (Self : access Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --  begin
   --     Adjust_System_Volume (100);
   --  end Vol_Reset_Btn_CB;

   --  procedure Quieter_Btn_CB (Self : access Gtk_Button_Record'Class) is
   --     pragma Unreferenced (Self);
   --     Vol : constant Natural := Get_System_Volume;
   --  begin
   --     if Vol > 9 then
   --        Adjust_System_Volume (Vol - 10);
   --     end if;
   --  end Quieter_Btn_CB;

   function Create_Live_Controls_Grid return Gtk_Grid is
      Controls_Grid : Gtk_Grid;
      Spacer_Lab    : Gtk_Label;
   begin
      Gtk_New (Controls_Grid);
      Controls_Grid.Set_Column_Spacing (10);
      Controls_Grid.Set_Column_Homogeneous (True);
      Gtk_New (Spacer_Lab, "");

      Gtk_New_From_Icon_Name (Play_Btn, "media-playback-start", Icon_Size_Dialog);
      Play_Btn.Set_Image_Position (Pos_Top);
      Play_Btn.Set_Label ("Play");
      Play_Btn.On_Clicked (Play_Btn_CB'Access);
      Controls_Grid.Attach (Play_Btn, 0, 0);

      --  Gtk_New_From_Icon_Name (Pause_Btn, "media-playback-pause", Icon_Size_Dialog);
      --  Pause_Btn.Set_Image_Position (Pos_Top);
      --  Pause_Btn.Set_Label ("Pause");
      --  --  Pause_Btn.On_Clicked (Pause_Btn_CB'Access);
      --  Controls_Grid.Attach (Pause_Btn, 1, 0);

      Gtk_New_From_Icon_Name (Stop_Btn, "media-playback-stop", Icon_Size_Dialog);
      Stop_Btn.Set_Image_Position (Pos_Top);
      Stop_Btn.Set_Label ("Stop");
      Stop_Btn.On_Clicked (Stop_Btn_CB'Access);
      Controls_Grid.Attach (Stop_Btn, 2, 0);

      Gtk_New_From_Icon_Name (Previous_Btn, "media-skip-backward", Icon_Size_Dialog);
      Previous_Btn.Set_Image_Position (Pos_Top);
      Previous_Btn.Set_Label ("Previous");
      Previous_Btn.On_Clicked (Previous_Btn_CB'Access);
      Controls_Grid.Attach (Previous_Btn, 3, 0);

      Gtk_New_From_Icon_Name (Next_Btn, "media-skip-forward", Icon_Size_Dialog);
      Next_Btn.Set_Image_Position (Pos_Top);
      Next_Btn.Set_Label ("Next");
      Next_Btn.On_Clicked (Next_Btn_CB'Access);
      Controls_Grid.Attach (Next_Btn, 4, 0);

      --  Controls_Grid.Attach (Spacer_Lab, 5, 0);

      --  Gtk_New_From_Icon_Name (Quieter_Btn, "audio-volume-low", Icon_Size_Dialog);
      --  Quieter_Btn.Set_Image_Position (Pos_Top);
      --  Quieter_Btn.Set_Label ("Vol -");
      --  Quieter_Btn.On_Clicked (Quieter_Btn_CB'Access);
      --  Controls_Grid.Attach (Quieter_Btn, 6, 0);

      --  Gtk_New_From_Icon_Name (Vol_Reset_Btn, "audio-volume-medium", Icon_Size_Dialog);
      --  Vol_Reset_Btn.Set_Image_Position (Pos_Top);
      --  Vol_Reset_Btn.Set_Label ("Reset ");
      --  Vol_Reset_Btn.On_Clicked (Vol_Reset_Btn_CB'Access);
      --  Controls_Grid.Attach (Vol_Reset_Btn, 7, 0);

      --  Gtk_New_From_Icon_Name (Louder_Btn, "audio-volume-high", Icon_Size_Dialog);
      --  Louder_Btn.Set_Image_Position (Pos_Top);
      --  Louder_Btn.Set_Label ("Vol +");
      --  Louder_Btn.On_Clicked (Louder_Btn_CB'Access);
      --  Controls_Grid.Attach (Louder_Btn, 8, 0);

      return Controls_Grid;

   end Create_Live_Controls_Grid;

end GUI.Live_Controls;