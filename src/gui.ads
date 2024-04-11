--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Gdk.Pixbuf;

with Glib.Main;

with Gtk.Application;         use Gtk.Application;
with Gtk.Application_Window;  use Gtk.Application_Window;

with Gtk.Box;
with Gtk.Button;
with Gtk.Css_Provider;        use Gtk.Css_Provider;
with Gtk.GEntry;
with Gtk.Grid;                use Gtk.Grid;
with Gtk.Label;               use Gtk.Label;

package GUI is

   package SB_Timeout_P is new Glib.Main.Generic_Sources (Gtk.Box.Gtk_Box);

   App_SemVer    : constant String := "0.1.0";  --  TODO Update Version each release!
   App_Title     : constant String := "eMDee";
   App_ID        : constant String := "fr.merrony." & App_Title;
   App_Comment   : constant String := "Musical Director's Assistant";
   App_Author    : constant String := "Stephen Merrony";
   App_Copyright : constant String := "Copyright (C)2024 S.Merrony";
   App_Icon      : constant String := "emdee.ico";
   App_CSS       : constant String := "main.css";
   App_Website   : constant String := "https://github.com/SMerrony/emdee";

   Max_Tracks  : constant Integer := 99;

   --  Track display column ordering
   Row_Col     : constant Glib.Gint := 0;
   Select_Col  : constant Glib.Gint := 1;
   Title_Col   : constant Glib.Gint := 2;
   Skip_Col    : constant Glib.Gint := 3;
   Comment_Col : constant Glib.Gint := 4;
   Vol_Col     : constant Glib.Gint := 5;
   File_Col    : constant Glib.Gint := 6;
   Down_Col    : constant Glib.Gint := 7;
   Up_Col      : constant Glib.Gint := 8;
   Del_Col     : constant Glib.Gint := 9;

   type Select_Btn_Arr_T is array (1 .. Max_Tracks) of Gtk.Button.Gtk_Button;

   App         : Gtk_Application;
   Main_Window : Gtk_Application_Window;
   Icon_PB     : Gdk.Pixbuf.Gdk_Pixbuf;
   Main_Box    : Gtk.Box.Gtk_Box;
   Session_Header_Grid,
   Tracks_Grid : Gtk.Grid.Gtk_Grid;
   Select_Btn_Arr : Select_Btn_Arr_T;
   Show_Track_Modifiers : Boolean := True;

   Session_Desc_Entry,
   Session_Comment_Entry : Gtk.GEntry.Gtk_Entry;

   --  Status Bar items...
   Active_Label,
   Players_Label : Gtk_Label;
   SB_Update_MS  : Glib.Guint := 250;
   SB_Timeout    : Glib.Main.G_Source_Id := 0;

   Currently_Selected_Track : Integer := -1;
   Currently_Active : Boolean := False;

   CSS_Provider : constant Gtk.Css_Provider.Gtk_Css_Provider := Gtk.Css_Provider.Gtk_Css_Provider_New;

   procedure Launch;

private

   procedure Display_Tracks;

end GUI;