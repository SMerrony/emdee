--  SPDX-License-Identifier: GPL-3.0-or-later
--  SPDX-FileCopyrightText: Copyright 2024 Stephen Merrony

with Gtk.Button;              use Gtk.Button;
with Gtk.Grid;                use Gtk.Grid;

package GUI.Tracks is

   --  Track display column ordering
   Row_Col      : constant Glib.Gint := 0;
   Select_Col   : constant Glib.Gint := 1;
   Title_Col    : constant Glib.Gint := 2;
   Skip_Col     : constant Glib.Gint := 3;
   Comment_Col  : constant Glib.Gint := 4;
   Vol_Col      : constant Glib.Gint := 5;
   File_Col     : constant Glib.Gint := 6;
   File_Del_Col : constant Glib.Gint := 7;
   Down_Col     : constant Glib.Gint := 8;
   Up_Col       : constant Glib.Gint := 9;
   Del_Col      : constant Glib.Gint := 10;

   Tracks_Grid : Gtk_Grid;

   New_Track_File_Btn : Gtk_Button;

   function  Create_Tracks_Grid return Gtk_Grid;
   procedure Clear_Tracks_Display;
   procedure Display_Empty_Track (Track_Row : Glib.Gint);
   procedure Display_Tracks;

end GUI.Tracks;