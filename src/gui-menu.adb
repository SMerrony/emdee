--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Containers;          use Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Glib;                    use Glib;

with Gtk.About_Dialog;        use Gtk.About_Dialog;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Gtkada.File_Selection;   use Gtkada.File_Selection;

with Players;                 use Players;
with Session;                 use Session;

with GUI.Tracks;              use GUI.Tracks;

package body GUI.Menu is

   function Check_Session_Desc return Boolean is
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Session_Desc_Entry.Get_Text /= "" then
         return True;
      else
         Unused_Buttons := Message_Dialog (Msg => "You must enter the Session description, cannot save yet.",
                                           Dialog_Type => Warning,
                                           Title => App_Title & " - Warning");
         return False;
      end if;
   end Check_Session_Desc;

   procedure Session_New_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Prev_MIDI_Port : constant Unbounded_String := Sess.MIDI_Port;
   begin
      Clear_Session;
      Session_Desc_Entry.Set_Text ("");
      Session_Comment_Entry.Set_Text ("");
      Sess.MIDI_Port := Prev_MIDI_Port;
      Track_Modifiers_Check_Item.Set_Active (True);
      Clear_Tracks_Display;
      Display_Empty_Track (1);
      Tracks_Grid.Show_All;
      Session_Desc_Entry.Grab_Focus;
   end Session_New_CB;

   procedure Session_Open_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         File_Selection_Dialog (Title => App_Title & " Open Session",
                                Dir_Only => False,
                                Must_Exist => True);
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Filename'Length > 1 then
         Load_Session (Filename);
         Sess.Filename := To_Unbounded_String (Filename);
         Session_Desc_Entry.Set_Text (To_String (Sess.Desc));
         Session_Comment_Entry.Set_Text (To_String (Sess.Comment));
         if Sess.MIDI_Port /= Null_Unbounded_String then
            Create_Notes_Off_MIDI;
         end if;
         if Sess.Tracks.Length > 0 then
            Display_Tracks;
         end if;
      end if;
   exception
      when E : others =>
         Unused_Buttons := Message_Dialog (Msg => "Could not open Session TOML file.  " & Exception_Message (E),
                                           Dialog_Type => Warning,
                                           Title => App_Title & " - Error");
         Sess.Filename := Null_Unbounded_String;
   end Session_Open_CB;

   procedure Session_Save_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Check_Session_Desc then
         if Sess.Filename = Null_Unbounded_String then
            Unused_Buttons := Message_Dialog (Msg => "No Session has been loaded, cannot save.",
                                              Dialog_Type => Warning,
                                              Title => App_Title & " - Error");
         else
            Update_Text_Fields;
            Save_Session (To_String (Sess.Filename));
         end if;
      end if;
   end Session_Save_CB;

   procedure Session_Save_As_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Filename : constant String :=
         File_Selection_Dialog (Title => App_Title & " - Save Session As...",
                                Dir_Only => False,
                                Must_Exist => False);
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      if Check_Session_Desc then
         if Filename'Length > 0 then
            if Ada.Directories.Exists (Filename) then
               Unused_Buttons := Message_Dialog (Msg => "Session TOML file already exists, " & CR & LF
                                                        & "Use 'Save' to overwrite.",
                                                 Dialog_Type => Warning,
                                                 Title => App_Title & " - Oops");
            else
               Update_Text_Fields;
               Save_Session (Filename);
            end if;
         end if;
      end if;
   end Session_Save_As_CB;

   procedure Session_MIDI_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
      Dialog         : Gtk_Dialog;
      Dlg_Box        : Gtk_Box;
      Dlg_Port_Label : Gtk_Label;
      Port_Entry     : Gtk_Entry;
      Cancel_Unused,
      Save_Unused    : Gtk_Widget;
      Unused_Buttons : Message_Dialog_Buttons;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Destroy_With_Parent (True);
      Dialog.Set_Modal (True);
      Dialog.Set_Title (App_Title & " - Session MIDI Port");
      Dlg_Box := Dialog.Get_Content_Area;
      Dlg_Port_Label := Gtk_Label_New ("MIDI Out Port:");
      Dlg_Box.Pack_Start (Child => Dlg_Port_Label, Expand => True, Fill => True, Padding => 5);
      Port_Entry := Gtk_Entry_New;
      if Sess.MIDI_Port /= Null_Unbounded_String then
         Port_Entry.Set_Text (To_String (Sess.MIDI_Port));
      end if;
      Port_Entry.Set_Tooltip_Text ("MIDI port suitable for aplaymidi to use in form nn:n. " & CR & LF
                                   & "Use aplaymidi -l to see list");
      Dlg_Box.Pack_Start (Child => Port_Entry, Expand => True, Fill => True, Padding => 5);
      Cancel_Unused := Dialog.Add_Button ("Cancel", Gtk_Response_Cancel);
      Save_Unused   := Dialog.Add_Button ("Save", Gtk_Response_Accept);
      Dialog.Set_Default_Response (Gtk_Response_Accept);
      Dialog.Show_All;
      if Dialog.Run = Gtk_Response_Accept then
         if Port_Entry.Get_Text_Length > 0 then
            Sess.MIDI_Port := To_Unbounded_String (Port_Entry.Get_Text);
         end if;
      end if;
      Dialog.Destroy;
   end Session_MIDI_CB;

   procedure Track_Modifiers_CB (Self : access Gtk_Check_Menu_Item_Record'Class) is
   begin
      Show_Track_Modifiers := Self.Get_Active;
      Update_Text_Fields;
      Display_Tracks;
   end Track_Modifiers_CB;

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

   procedure Quit_CB (Self : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Shutting_Down := True;
      delay 0.5;
      --  Removing the main window closes the application...
      App.Remove_Window (Main_Window);
   end Quit_CB;

   function Create_Menu_Bar return Gtk.Menu_Bar.Gtk_Menu_Bar is
      Menu_Bar : Gtk.Menu_Bar.Gtk_Menu_Bar;
      Sep_Item : Gtk.Separator_Menu_Item.Gtk_Separator_Menu_Item;
      File_Menu, Session_Menu, Help_Menu : Gtk.Menu.Gtk_Menu;
      Menu_Item : Gtk.Menu_Item.Gtk_Menu_Item;
      Session_Open_Item, Session_Save_Item, Session_Save_As_Item, Session_New_Item, Session_MIDI_Item,
      Quit_Item,
      About_Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      --  Log (DEBUG, "Starting to Create_Menu_Bar");
      Gtk_New (Menu_Bar);

      --  File

      Gtk_New (Menu_Item, "File");
      Menu_Bar.Append (Menu_Item);
      Gtk_New (File_Menu);
      Menu_Item.Set_Submenu (File_Menu);

      --  Session New
      Gtk_New (Session_New_Item, "New Session");
      File_Menu.Append (Session_New_Item);
      Session_New_Item.On_Activate (Session_New_CB'Access);

      --  Session Open
      Gtk_New (Session_Open_Item, "Open Session");
      File_Menu.Append (Session_Open_Item);
      Session_Open_Item.On_Activate (Session_Open_CB'Access);

      Gtk_New (Sep_Item);
      File_Menu.Append (Sep_Item);

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
      Gtk_New (Track_Modifiers_Check_Item, "Allow Track Editing");
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

end GUI.Menu;